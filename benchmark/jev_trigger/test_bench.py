"""Offline regression tests for the Jev trigger benchmark."""

import json
from pathlib import Path
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).parent))
import bench  # noqa: E402
from plot_all_keystrokes import validate_result


class BenchmarkTests(unittest.TestCase):
    def test_gallery_rejects_missing_or_changed_keystrokes(self):
        cases = [{"id": "sample", "group": "english_sentence", "context": "plain",
                  "keys": "Hello.", "trigger_after": []}]
        rows = [{**r, "score": 0.1} for r in bench.expand_cases(cases)]
        data = {"backend": "jev", "dataset": "all", "complete_dataset": True, "rows": rows}
        validate_result(data, cases)
        with self.assertRaises(ValueError):
            validate_result({**data, "rows": rows[:-1]}, cases)
        with self.assertRaises(ValueError):
            validate_result({**data, "rows": [{**rows[0], "buffer": "X"}] + rows[1:]}, cases)

    def test_cases_expand_and_have_both_labels(self):
        rows = list(bench.expand_cases(json.loads(bench.CASES.read_text())))
        self.assertGreater(len(rows), 100)
        self.assertTrue(any(r["gold"] for r in rows))
        self.assertTrue(any(not r["gold"] for r in rows))
        self.assertEqual(len(rows), len({(r["id"], r["index"]) for r in rows}))

    def test_additional_cases_expand(self):
        rows = list(bench.expand_cases(json.loads(bench.ADDITIONAL_CASES.read_text())))
        self.assertEqual(len(rows), 190)
        self.assertEqual(sum(r["gold"] for r in rows), 7)
        greeting_rows = list(bench.expand_cases(json.loads(bench.CASES.read_text())))
        greeting = {r["index"]: r for r in greeting_rows if r["id"] == "greeting_thanks"}
        self.assertFalse(greeting[9]["gold"])
        self.assertTrue(greeting[9]["acceptable"])
        self.assertTrue(greeting[19]["gold"])

    def test_resume_last_eight_rows(self):
        total = len(list(bench.expand_cases(json.loads(bench.ADDITIONAL_CASES.read_text()))))
        result = bench.run(SimpleNamespace(
            backend="rule-proxy", dataset="additional", prompt_version="v2",
            start_row=total - 7, threshold=0.8, max_calls=None, timeout=10.0,
        ))
        self.assertEqual(result["overall"]["n"], 8)
        self.assertEqual(result["total_keystrokes"], total)
        self.assertFalse(result["complete_dataset"])

    def test_acceptable_greeting_is_not_unwanted(self):
        result = bench.run(SimpleNamespace(
            backend="rule-proxy", dataset="dev", prompt_version="v2",
            start_row=1, threshold=0.8, max_calls=None, timeout=10.0,
        ))
        self.assertIn("greeting_thanks@9", result["acceptable_scores"])
        self.assertNotIn("greeting_thanks@9", result["unwanted_triggers"])

    def test_backspace_edits_buffer(self):
        rows = list(bench.expand_cases([{
            "id": "edit", "group": "editing", "context": "plain",
            "keys": "ab\bc", "trigger_after": [],
        }]))
        self.assertEqual([r["buffer"] for r in rows], ["a", "ab", "a", "ac"])

    def test_proxy_particle_and_debounce(self):
        rows = list(bench.expand_cases(json.loads(bench.CASES.read_text())))
        by_id = {(r["id"], r["index"]): r for r in rows}
        self.assertEqual(bench.rule_proxy(by_id["weather_sunny", 7]), 1.0)
        self.assertEqual(bench.rule_proxy(by_id["acknowledge", 4]), 0.0)
        self.assertEqual(bench.rule_proxy(by_id["decimal_pi", 2]), 0.0)
        self.assertEqual(bench.rule_proxy(by_id["private_note", 21]), 0.0)

    def test_new_cases_are_complete_sentences_or_explicit_controls(self):
        for path in (bench.CASES, bench.ADDITIONAL_CASES):
            for case in json.loads(path.read_text()):
                if case["group"] not in ("english", "number", "code"):
                    self.assertIn("japanese", case)
                    self.assertTrue(case["keys"].endswith((" ", ".", "?")))

    def test_english_sentences_are_all_no_convert(self):
        rows = list(bench.expand_cases(json.loads(bench.ENGLISH_CASES.read_text())))
        self.assertEqual(len(rows), 685)
        self.assertEqual(len({r["id"] for r in rows}), 15)
        self.assertTrue(all(not r["gold"] and not r["acceptable"] for r in rows))
        self.assertTrue(any(r["key"] == "." and r["pause_ms"] == 700 for r in rows))

    def test_english_safety_counts_affected_cases(self):
        rows = [
            {"id": "one", "group": "english_sentence", "score": 0.9},
            {"id": "one", "group": "english_sentence", "score": 0.9},
            {"id": "two", "group": "english_sentence", "score": 0.1},
        ]
        result = bench.english_safety(rows, 0.8)
        self.assertEqual(result["false_triggers"], 2)
        self.assertEqual(result["cases_with_trigger"], 1)
        self.assertEqual(result["affected_cases"], ["one"])

    def test_explicit_punctuation_labels_and_pauses(self):
        rows = list(bench.expand_cases(json.loads(bench.PUNCTUATION_CASES.read_text())))
        self.assertEqual(len(rows), 167)
        marks = {r["key"] for r in rows if r["gold"]}
        self.assertEqual(marks, set(".,!?。、！？"))
        self.assertTrue(all(r["gold"] == (r["key"] in marks) for r in rows))
        self.assertTrue(all(r["pause_ms"] == 700 for r in rows if r["gold"]))

    def test_select_single_case_includes_all_scores(self):
        result = bench.run(SimpleNamespace(
            backend="rule-proxy", dataset="english", prompt_version="v2",
            case_id="english_long_report", include_rows=True,
            start_row=1, threshold=0.8, max_calls=None, timeout=10.0,
        ))
        self.assertTrue(result["complete_dataset"])
        self.assertEqual(len(result["rows"]), 91)
        self.assertEqual({r["id"] for r in result["rows"]}, {"english_long_report"})

    def test_all_dataset_call_cap_covers_every_row(self):
        cases = []
        for path in (bench.CASES, bench.ADDITIONAL_CASES, bench.ENGLISH_CASES, bench.PUNCTUATION_CASES):
            cases.extend(json.loads(path.read_text()))
        rows = list(bench.expand_cases(cases))
        calls = sum(r["context"] not in bench.EXCLUDED_CONTEXTS for r in rows)
        args = SimpleNamespace(
            backend="jev", dataset="all", prompt_version="v2",
            start_row=1, threshold=0.8, max_calls=calls, timeout=10.0,
        )
        with patch.dict(bench.os.environ, {"TYPESAFE_API_KEY": "fake"}):
            with patch.object(bench, "jev_score", return_value=(0.0, 1, 1.0, "jev-test")):
                result = bench.run(args)
        self.assertTrue(result["complete_dataset"])
        self.assertEqual(result["total_keystrokes"], len(rows))
        self.assertEqual(result["api_calls"], calls)

    def test_request_does_not_include_gold_or_future_keys(self):
        row = next(bench.expand_cases([{
            "id": "sample", "group": "romanized", "context": "plain",
            "keys": "wa ", "trigger_after": [3],
        }]))
        payload = bench.build_request(row)
        self.assertNotIn("gold", json.dumps(payload))
        self.assertEqual(payload["state"]["buffer_before_cursor"], "w")
        self.assertIn("criteria", bench.build_request(row, "v2")["questions"]["convert_now"])

    def test_metrics(self):
        rows = [{"gold": True, "score": 0.9}, {"gold": True, "score": 0.1},
                {"gold": False, "score": 0.9}, {"gold": False, "score": 0.1}]
        result = bench.metrics(rows, 0.8)
        self.assertEqual((result["tp"], result["fp"], result["fn"], result["tn"]),
                         (1, 1, 1, 1))

    def test_threshold_changes_decision(self):
        rows = [{"gold": True, "score": 0.7}, {"gold": False, "score": 0.4}]
        self.assertEqual(bench.metrics(rows, 0.8)["fn"], 1)
        self.assertEqual(bench.metrics(rows, 0.6)["tp"], 1)

    def test_preference_summary_distinguishes_acceptable_early_trigger(self):
        rows = [
            {"gold": True, "acceptable": True, "score": 0.9},
            {"gold": False, "acceptable": True, "score": 0.9},
            {"gold": False, "acceptable": False, "score": 0.9},
        ]
        summary = bench.preference_summary(rows, 0.8)
        self.assertEqual(summary["preferred_detected"], 1)
        self.assertEqual(summary["acceptable_but_nonpreferred"], 1)
        self.assertEqual(summary["unwanted_triggers"], 1)
        self.assertEqual(summary["acceptable_precision"], 0.6667)

    def test_jev_response_parsing(self):
        row = {"buffer": "wa ", "key": " ", "context": "plain", "pause_ms": 0}
        class FakeResponse:
            def __enter__(self):
                return self

            def __exit__(self, *_args):
                return False

            def read(self, *_args):
                return b'{"model":"jev-1.13.0","answers":{"convert_now":{"noul":0.91}},"usage":{"input_tokens":42}}'

        with patch.object(bench.request, "urlopen", return_value=FakeResponse()):
            score, tokens, elapsed, model = bench.jev_score(row, "fake", 1)
        self.assertEqual((score, tokens, model), (0.91, 42, "jev-1.13.0"))
        self.assertGreaterEqual(elapsed, 0)


if __name__ == "__main__":
    unittest.main()
