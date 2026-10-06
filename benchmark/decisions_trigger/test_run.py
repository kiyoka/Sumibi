"""Offline tests. No credentials or network calls are used."""
import copy
import io
import json
from pathlib import Path
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch
from urllib.error import HTTPError

sys.path.insert(0, str(Path(__file__).parent))
import run
import compare


class Tests(unittest.TestCase):
    def args(self, path, **options):
        return SimpleNamespace(backend="decisions", dataset="dev", case_id="greeting_thanks",
                               output=path, max_calls=3, threshold=.8, timeout=10,
                               resume=options.get("resume", False), dry_run=options.get("dry_run", False))

    def test_payload_identical_evidence_no_labels(self):
        row = run.load_rows("dev")[0]
        payload = run.build_payload(row, "decisions")
        self.assertEqual(json.loads(payload["input"]), run.build_payload(row, "jev")["state"])
        self.assertEqual(payload["questions"][0]["type"], "predicate")
        self.assertNotIn("gold", json.dumps(payload))
        self.assertNotIn("acceptable_after", json.dumps(payload))
        self.assertEqual(json.loads(payload["input"])["buffer_before_cursor"], row["buffer"])

    def test_keychain_lookup_fixed_target_no_environment_export(self):
        with patch.object(run.sys, "platform", "darwin"), patch.object(run.subprocess, "run") as lookup:
            lookup.return_value = SimpleNamespace(returncode=0, stdout="fake-secret\n", stderr="")
            with patch.dict(run.os.environ, {}, clear=True):
                self.assertEqual(run.get_api_key("decisions", "keychain"), "fake-secret")
                self.assertNotIn("OPENAI_API_KEY", run.os.environ)
            self.assertEqual(lookup.call_args.args[0], ["/usr/bin/security", "find-internet-password", "-s",
                                                       "api.openai.com", "-a", "apikey", "-w"])
            lookup.return_value = SimpleNamespace(returncode=1, stdout="", stderr="private details")
            with self.assertRaisesRegex(ValueError, "^keychain lookup failed$"):
                run.get_api_key("decisions", "keychain")
            with self.assertRaises(ValueError):
                run.get_api_key("jev", "keychain")

    def test_refusals_and_invalid_probabilities_are_not_zero(self):
        base = {"answers": [{"name": "convert_now", "type": "predicate", "probability": .8}],
                "usage": {"input_tokens": 500}, "model": "gpt-6-luna"}
        self.assertEqual(run.parse_response(base, "decisions"), (.8, 500, "gpt-6-luna"))
        for value in (True, "0.8", float("nan"), float("inf"), -.1, 1.1):
            data = copy.deepcopy(base)
            data["answers"][0]["probability"] = value
            with self.assertRaises(ValueError):
                run.parse_response(data, "decisions")
        for answers in ([], [{"name": "convert_now", "type": "refusal"}], base["answers"] * 2):
            with self.assertRaises(ValueError):
                run.parse_response({**base, "answers": answers}, "decisions")

    def test_http_errors_sanitize_body_and_credentials(self):
        error = HTTPError("https://api.openai.com/v1/decisions", 401, "secret text", {}, io.BytesIO(b"secret body"))
        with patch.object(run.request, "urlopen", side_effect=error):
            record = run.fetch(run.load_rows("dev")[0], "decisions", "secret-key", 10)
        self.assertEqual(record["error"], "http_401")
        self.assertIsNone(record["score"])
        self.assertNotIn("secret", json.dumps(record))

    def test_dry_run_full_count(self):
        args = self.args(None, dry_run=True)
        args.dataset, args.case_id = "all", None
        result = run.run(args)
        self.assertEqual(result["required_api_calls"], 1207)
        self.assertEqual(result["locally_excluded"], 32)
        self.assertEqual(result["illustrative_total_usd"], .07242)

    def test_checkpoint_resume_cap_and_no_overwrite(self):
        record = {"status": "ok", "score": .2, "input_tokens": 500,
                  "model": "gpt-6-luna", "latency_ms": 100}
        with tempfile.TemporaryDirectory() as directory, patch.dict(run.os.environ, {"OPENAI_API_KEY": "fake"}):
            path = Path(directory) / "scores.json"
            with patch.object(run, "fetch", return_value=record) as fetch:
                result = run.run(self.args(path))
                self.assertEqual(len(result["rows"]), 3)
                self.assertEqual(fetch.call_count, 3)
                result = run.run(self.args(path, resume=True))
                self.assertEqual(len(result["rows"]), 6)
                self.assertEqual(result["api_calls"], 6)
                self.assertEqual(result["estimated_usd"], .0003)
                with self.assertRaises(ValueError):
                    run.run(self.args(path))
                changed = self.args(path, resume=True)
                changed.threshold = .9
                with self.assertRaises(ValueError):
                    run.run(changed)
                self.assertEqual(len(json.loads(path.read_text())["rows"]), 6)

    def test_missing_key_does_not_create_or_empty_output(self):
        with tempfile.TemporaryDirectory() as directory, patch.dict(run.os.environ, {}, clear=True):
            path = Path(directory) / "scores.json"
            with self.assertRaises(ValueError):
                run.run(self.args(path))
            self.assertFalse(path.exists())

    def test_failure_stops_and_keeps_unknown_cost(self):
        record = {"status": "error", "score": None, "error": "http_429", "input_tokens": None,
                  "latency_ms": 100}
        with tempfile.TemporaryDirectory() as directory, patch.dict(run.os.environ, {"OPENAI_API_KEY": "fake"}):
            with patch.object(run, "fetch", return_value=record) as fetch:
                result = run.run(self.args(Path(directory) / "scores.json"))
                self.assertEqual(fetch.call_count, 1)
                self.assertEqual(result["summary"]["failed_keystrokes"], 1)
                self.assertEqual(result["summary"]["overall"]["n"], 0)
                self.assertEqual(result["unknown_usage_attempts"], 1)

    def test_excluded_never_sent(self):
        record = {"status": "ok", "score": .1, "input_tokens": 1, "model": "test", "latency_ms": 1}
        with tempfile.TemporaryDirectory() as directory, patch.dict(run.os.environ, {"OPENAI_API_KEY": "fake"}):
            args = self.args(Path(directory) / "scores.json")
            args.case_id, args.max_calls = None, 2000
            with patch.object(run, "fetch", return_value=record) as fetch:
                result = run.run(args)
                self.assertTrue(result["complete_dataset"])
                self.assertEqual(result["summary"]["locally_excluded_keystrokes"], 32)
                self.assertTrue(all(c.args[0]["context"] not in run.bench.EXCLUDED_CONTEXTS for c in fetch.call_args_list))

    def test_retry_failed_row_preserves_attempt_cost_and_failure(self):
        failure = {"status": "error", "score": None, "error": "network_or_timeout", "input_tokens": None, "latency_ms": 10000}
        success = {"status": "ok", "score": .8, "input_tokens": 400, "latency_ms": 300, "model": "gpt-6-luna"}
        with tempfile.TemporaryDirectory() as directory, patch.dict(run.os.environ, {"OPENAI_API_KEY": "fake"}):
            args = self.args(Path(directory) / "scores.json")
            args.max_calls = 1
            with patch.object(run, "fetch", return_value=failure):
                run.run(args)
            args.resume, args.retry_errors = True, True
            with patch.object(run, "fetch", return_value=success) as fetch:
                result = run.run(args)
                self.assertEqual(fetch.call_count, 1)
                self.assertEqual(len(result["rows"]), 1)
                self.assertEqual(result["api_calls"], 2)
                self.assertEqual(result["input_tokens"], 400)
                self.assertEqual(result["unknown_usage_attempts"], 1)
                self.assertEqual(result["summary"]["failed_keystrokes"], 0)
                self.assertEqual(result["summary"]["attempt_errors"], {"network_or_timeout": 1})

    def test_historical_matching_and_no_failed_score_plot(self):
        old = json.loads((run.bench.CASES.parent / "all_scores.json").read_text())
        decision = copy.deepcopy(old)
        decision["backend"] = "decisions"
        report, rows = compare.build_comparison(decision, old)
        self.assertEqual(report["total_keystrokes"], 1239)
        self.assertIsNone(report["backends"]["jev"]["latency_p99_ms"])
        self.assertEqual(sum(r["status"] == "excluded" for r in rows["jev"]), 32)
        decision["rows"][0]["status"] = "error"
        with self.assertRaises(ValueError):
            compare.build_comparison(decision, old)
        decision = copy.deepcopy(old)
        decision["backend"] = "decisions"
        decision["rows"][0]["buffer"] = "wrong"
        with self.assertRaises(ValueError):
            compare.build_comparison(decision, old)


if __name__ == "__main__":
    unittest.main()
