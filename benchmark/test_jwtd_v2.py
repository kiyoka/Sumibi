"""Issue #205 offline isolation and paid-request guard tests."""
import contextlib
from datetime import datetime, timezone
import io
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import jwtd_v2 as v2
from plot_jwtd_v2 import plot


class JWTDV2Test(unittest.TestCase):
    def test_resume_reuses_successes_and_preserves_original_and_failure(self):
        entries, utils = v2.load_dataset()
        model = "gemini-3.6-flash"
        metadata = {"model": model, "finish_reason": "stop", "empty_choices": False,
                    "usage": {"prompt_tokens": 500, "completion_tokens": 200, "total_tokens": 700}}
        error = RuntimeError("private-server-error-not-for-logs")
        error.status_code = 503
        with tempfile.TemporaryDirectory() as temp, patch.object(v2, "ISSUE_DIR", Path(temp)), \
             patch.object(v2, "load_dataset", return_value=(entries, utils)), \
             patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake", "SUMIBI_AI_TIMEOUT": "120"}, clear=True), \
             patch.object(v2, "datetime", wraps=datetime) as clock, \
             patch("sumibi_typical_convert_client.SumibiTypicalConvertClient") as client, \
             contextlib.redirect_stdout(io.StringIO()):
            clock.now.return_value = datetime(2026, 11, 1, tzinfo=timezone.utc)
            instance = client.return_value
            instance.last_response_metadata = metadata
            instance.convert.side_effect = ["日本語"] * 3 + [error]
            original = Path(temp) / "results/failed.json"
            with self.assertRaises(RuntimeError):
                v2.run(model, 1, original)
            before = original.read_bytes()
            self.assertNotIn(b"private-server-error", before)
            self.assertEqual(instance.convert.call_count, 4)
            instance.convert.reset_mock()
            instance.convert.side_effect = None
            instance.convert.return_value = "日本語"
            resumed = original.with_name("resumed.json")
            v2.run(model, 1, resumed, original)
            self.assertEqual(instance.convert.call_count, 6)
            self.assertEqual(original.read_bytes(), before)
            a = json.loads(resumed.read_text())
            self.assertTrue(a["complete"])
            self.assertEqual(len(a["rows"]), 9)
            self.assertEqual(a["rows"][:3], json.loads(before)["rows"][:3])
            self.assertEqual(a["failed_attempts"][0]["http_status"], 503)
            self.assertEqual(a["resume_history"][0]["preserved_successful_calls"], 3)
            self.assertEqual(a["resume_history"][0]["remaining_calls"], 6)
            # Complete runs and foreign models/settings/order fail before SDK construction.
            client.reset_mock()
            with self.assertRaises(ValueError):
                v2.run(model, 1, original.with_name("invalid.json"), resumed)
            client.assert_not_called()
            for field, value in [("model", "gemini-3.8-flash"), ("timeout_seconds", 60),
                                 ("dataset", {}), ("source_sha256", {})]:
                invalid = json.loads(before)
                invalid[field] = value
                source = original.with_name("tampered.json")
                source.write_text(json.dumps(invalid))
                with self.assertRaises(ValueError):
                    v2.run(model, 1, original.with_name("invalid.json"), source)
                client.assert_not_called()

    def test_resume_again_does_not_repeat_warmup_and_accumulates_errors(self):
        entries, utils = v2.load_dataset()
        model = "gemini-3.5-flash-lite"
        metadata = {"model": model, "finish_reason": "stop", "empty_choices": False}
        error = RuntimeError("fake failure")
        with tempfile.TemporaryDirectory() as temp, patch.object(v2, "ISSUE_DIR", Path(temp)), \
             patch.object(v2, "load_dataset", return_value=(entries, utils)), \
             patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake", "SUMIBI_AI_TIMEOUT": "120"}, clear=True), \
             patch.object(v2, "datetime", wraps=datetime) as clock, \
             patch("sumibi_typical_convert_client.SumibiTypicalConvertClient") as client, \
             contextlib.redirect_stdout(io.StringIO()):
            clock.now.return_value = datetime(2026, 11, 1, tzinfo=timezone.utc)
            instance = client.return_value
            instance.last_response_metadata = metadata
            paths = [Path(temp) / f"results/session{i}.json" for i in range(3)]
            for i in range(2):
                instance.convert.side_effect = ["日本語", error]
                with self.assertRaises(RuntimeError):
                    v2.run(model, 1, paths[i], paths[i-1] if i else None)
            instance.convert.reset_mock()
            instance.convert.side_effect = None
            instance.convert.return_value = "日本語"
            v2.run(model, 1, paths[2], paths[1])
            a = json.loads(paths[2].read_text())
            self.assertEqual(instance.convert.call_count, 7)
            self.assertEqual(len(a["failed_attempts"]), 2)
            self.assertEqual(len(a["resume_history"]), 2)
            self.assertEqual(len([r for r in a["rows"] if r["warmup"]]), 6)

    def test_pinned_dataset_and_legacy_membership(self):
        entries, _ = v2.load_dataset()
        self.assertEqual(len(entries), 628)
        self.assertEqual(sum(e["source"] == "v1" for e in entries), 200)
        self.assertEqual(sum(e["source"] == "v2-add" for e in entries), 428)

    def test_model_scope_and_alias(self):
        self.assertEqual(len(v2.MODELS), 10)
        self.assertEqual(len(v2.SKIPPED), 4)
        p = v2.plan()
        self.assertEqual(p["calls_per_model"], 1890)
        self.assertTrue(p["models"]["gemini-3.7-flash"]["alias_only"])
        self.assertAlmostEqual(p["models"]["gemini-4-argon"]["fixed_500_input_200_output_usd"], 5.67)
        self.assertAlmostEqual(p["models"]["gpt-6.1-terra"]["fixed_500_input_200_output_usd"], 2.835)
        self.assertAlmostEqual(p["models"]["gpt-6.1-sol"]["fixed_500_input_200_output_usd"], 5.67)

    def test_reject_v1_or_foreign_data(self):
        with tempfile.TemporaryDirectory() as temp:
            p = Path(temp) / "foreign.json"
            for data in ([], {"schema": "sumibi-jwtd-v2-run-1", "dataset": {"path": "JWTD_v2/v1"}},
                         {"schema": "sumibi-jwtd-v2-run-1", "dataset": v2.IDENTITY, "scoring": {}}):
                p.write_text(json.dumps(data))
                with self.assertRaises(ValueError):
                    v2.summarize([p])

    def test_modified_data_rejected(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(v2, "ISSUE_DIR", Path(temp)):
            with self.assertRaises(ValueError):
                v2.load_dataset()

    def test_safety_no_key_no_legacy_overwrite(self):
        for output in (v2.ISSUE_DIR / "results/new.json", v2.ROOT / "result_ver2.4.0/existing.json"):
            with patch.dict(os.environ, {"OPENAI_API_KEY": "do-not-use"}, clear=True):
                with patch("sumibi_typical_convert_client.OpenAI") as sdk:
                    with self.assertRaises(ValueError):
                        v2.run("gemini-3.5-flash-lite", 1, output)
                    sdk.assert_not_called()

    def test_fake_smoke_no_keys_or_other_endpoints(self):
        entries, utils = v2.load_dataset()
        metadata = {"model": "gemini-3.5-flash-lite", "finish_reason": "stop",
                    "empty_choices": False, "usage": {"prompt_tokens": 500,
                    "completion_tokens": 200, "total_tokens": 710}}
        with tempfile.TemporaryDirectory() as temp, patch.object(v2, "ISSUE_DIR", Path(temp)):
            output = Path(temp) / "results/smoke.json"
            with patch.object(v2, "load_dataset", return_value=(entries, utils)), patch.dict(os.environ,
                 {"SUMIBI_AI_API_KEY": "fake-secret", "SUMIBI_AI_BASEURL": "https://example.invalid",
                  "SUMIBI_AI_TIMEOUT": "120"}, clear=True):
                with patch("sumibi_typical_convert_client.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    with patch.object(v2, "datetime", wraps=datetime) as clock, contextlib.redirect_stdout(io.StringIO()):
                        clock.now.return_value = datetime(2026, 10, 31, tzinfo=timezone.utc)
                        v2.run("gemini-3.5-flash-lite", 1, output)
                    self.assertEqual(client.call_args.kwargs["base_url"], v2.BASE_URL)
                    self.assertEqual(client.call_args.kwargs["max_retries"], 0)
                    self.assertEqual(client.call_args.kwargs["reasoning_effort"], "minimal")
                    self.assertNotIn("fake-secret", output.read_text())
                    a = json.loads(output.read_text())
                    self.assertTrue(a["complete"])
                    self.assertEqual(len(a["rows"]), 9)
                    self.assertEqual(a["dataset"], v2.IDENTITY)
                    self.assertEqual(sum(r["unitemized_tokens"] for r in a["rows"]), 90)
                    with self.assertRaises(ValueError):
                        v2.run("gemini-3.5-flash-lite", 1, output)
                    self.assertEqual(client.call_count, 1)

    def test_cost_unknown_and_cache(self):
        self.assertIsNone(v2.cost(None, (1, .1, 5)))
        self.assertIsNone(v2.cost({"prompt_tokens": 200001, "completion_tokens": 1}, (1, .1, 5)))
        self.assertAlmostEqual(v2.cost({"prompt_tokens": 500, "completion_tokens": 200}, (1, .1, 5)), .0015)

    def test_foreign_summary_rejected_before_plot_output(self):
        with tempfile.TemporaryDirectory() as temp:
            output = Path(temp) / "plots"
            with self.assertRaises(ValueError):
                plot({"dataset": {"path": "JWTD_v2/v1"}}, output)
            self.assertFalse(output.exists())

    def test_full_coverage_and_source_breakdown_without_api(self):
        entries, utils = v2.load_dataset()
        audit = {"schema": "sumibi-jwtd-v2-run-1", "dataset": v2.IDENTITY,
                 "scoring": v2.SCORING, "model": "gemini-3.5-flash-lite",
                 "complete": True, "limit": 628, "alias_only": False,
                 "source_sha256": v2.measurement_hashes(), "rows": []}
        for mode in v2.MODES:
            for position, e in [(0, entries[0])] * 2 + list(enumerate(entries, 1)):
                audit["rows"].append({"position": position, "index": str(e["index"]),
                    "warmup": position == 0, "mode": mode, "status": "ok", "model": audit["model"],
                    "hypothesis": e["expected_output"][0], "elapsed_sec": .1,
                    "reference_usd": .001, "unitemized_tokens": 0})
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "full.json"
            path.write_text(json.dumps(audit))
            # Test exact alignment/group routing, without spending time on bootstrap sampling.
            with patch.object(utils, "evaluate_items", side_effect=lambda items, outputs, **kw:
                              {"overall": {"n": len(items), "MinCER": 0, "accuracy@1": 1}}), \
                 patch.object(v2, "load_dataset", return_value=(entries, utils)):
                summary = v2.summarize([path])
                hira = summary["models"][audit["model"]]["modes"]["hiragana_input"]
                self.assertEqual(hira["by_source"]["v1"]["overall"]["n"], 200)
                self.assertEqual(hira["by_source"]["v2-add"]["overall"]["n"], 428)
                self.assertEqual(hira["quality"]["width_normalized_accuracy@1"], 1)
                with self.assertRaises(ValueError):
                    v2.summarize([path, path])
                audit["rows"][3]["index"] = audit["rows"][2]["index"]
                path.write_text(json.dumps(audit))
                with self.assertRaises(ValueError):
                    v2.summarize([path])

    def test_gpt_models_use_openai_endpoint_and_keys(self):
        entries, utils = v2.load_dataset()
        metadata = {"model": "gpt-6.1-terra", "finish_reason": "stop",
                    "empty_choices": False, "usage": {"prompt_tokens": 500,
                    "completion_tokens": 200, "total_tokens": 700}}
        with tempfile.TemporaryDirectory() as temp, patch.object(v2, "ISSUE_DIR", Path(temp)):
            output = Path(temp) / "results/smoke_gpt.json"
            with patch.object(v2, "load_dataset", return_value=(entries, utils)), patch.dict(os.environ,
                 {"OPENAI_API_KEY": "fake-openai-key", "SUMIBI_AI_TIMEOUT": "120"}, clear=True):
                with patch("sumibi_typical_convert_client.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    with patch.object(v2, "datetime", wraps=datetime) as clock, contextlib.redirect_stdout(io.StringIO()):
                        clock.now.return_value = datetime(2026, 11, 1, tzinfo=timezone.utc)
                        v2.run("gpt-6.1-terra", 1, output)
                    self.assertEqual(client.call_args.kwargs["base_url"], v2.OPENAI_BASE_URL)
                    self.assertEqual(client.call_args.kwargs["model"], "gpt-6.1-terra")
                    self.assertEqual(client.call_args.kwargs["verbosity"], "low")
                    self.assertEqual(client.call_args.kwargs["service_tier"], "default")
                    self.assertNotIn("fake-openai-key", output.read_text())
                    a = json.loads(output.read_text())
                    self.assertTrue(a["complete"])
                    self.assertEqual(len(a["rows"]), 9)
                    self.assertEqual(a["endpoint"], "https://api.openai.com/v1/chat/completions")

    def test_gpt_6_1_sol_settings(self):
        entries, utils = v2.load_dataset()
        metadata = {"model": "gpt-6.1-sol", "finish_reason": "stop",
                    "empty_choices": False, "usage": {"prompt_tokens": 500,
                    "completion_tokens": 200, "total_tokens": 700}}
        with tempfile.TemporaryDirectory() as temp, patch.object(v2, "ISSUE_DIR", Path(temp)):
            output = Path(temp) / "results/smoke_gpt_sol.json"
            with patch.object(v2, "load_dataset", return_value=(entries, utils)), patch.dict(os.environ,
                 {"OPENAI_API_KEY": "fake-openai-key", "SUMIBI_AI_TIMEOUT": "120"}, clear=True):
                with patch("sumibi_typical_convert_client.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    with patch.object(v2, "datetime", wraps=datetime) as clock, contextlib.redirect_stdout(io.StringIO()):
                        clock.now.return_value = datetime(2026, 11, 1, tzinfo=timezone.utc)
                        v2.run("gpt-6.1-sol", 1, output)
                    self.assertEqual(client.call_args.kwargs["base_url"], v2.OPENAI_BASE_URL)
                    self.assertEqual(client.call_args.kwargs["model"], "gpt-6.1-sol")
                    self.assertIsNone(client.call_args.kwargs["temperature"])
                    self.assertEqual(client.call_args.kwargs["reasoning_effort"], "low")
                    self.assertEqual(client.call_args.kwargs["verbosity"], "low")
                    self.assertNotIn("service_tier", client.call_args.kwargs)
                    a = json.loads(output.read_text())
                    self.assertTrue(a["complete"])
                    self.assertEqual(len(a["rows"]), 9)


if __name__ == "__main__":
    unittest.main()
