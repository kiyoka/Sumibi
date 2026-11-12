import json
import unittest
from unittest.mock import Mock

from run_plamo_local import SYSTEM, completion, evaluate, input_for_mode, messages, summarize


class PlamoLocalTest(unittest.TestCase):
    def test_input_modes(self):
        self.assertEqual(input_for_mode("ハシ。", "hiragana_input"), "はし。")
        self.assertEqual(input_for_mode("ハシ。", "katakana_input"), "ハシ。")
        with self.assertRaises(ValueError):
            input_for_mode("ハシ。", "unknown")

    def test_context_is_not_part_of_source_text(self):
        self.assertEqual(messages("はし", "川の話。"), [
            {"role": "system", "content": SYSTEM + "\n---\n川の話。"},
            {"role": "user", "content": "はし"},
        ])

    def test_completion_does_not_follow_redirect_or_accept_other_model(self):
        connection = Mock()
        response = connection.getresponse.return_value
        response.status = 302
        with self.assertRaisesRegex(RuntimeError, "HTTP 302"):
            completion(connection, "plamo", "はし", "")
        connection.request.assert_called_once()
        method, path, body, headers = connection.request.call_args.args
        self.assertEqual((method, path), ("POST", "/v1/chat/completions"))
        self.assertEqual(json.loads(body)["model"], "plamo")
        self.assertNotIn("Authorization", headers)
        response.status = 200
        response.read.return_value = b'{"model":"other"}'
        with self.assertRaisesRegex(RuntimeError, "Unexpected response model"):
            completion(connection, "plamo", "はし", "")

    def test_scoring_preserves_extra_output_and_counts_empty(self):
        rows = [evaluate({"output": text, "elapsed_seconds": t, "finish_reason": "stop"}, ["箸", "お箸"])
                for text, t in [(" お箸\n", 1), ("箸\n説明", 2), ("", 3)]]
        self.assertEqual(rows[1]["output"], "箸\n説明")
        self.assertGreater(rows[1]["min_cer"], 1)
        summary = summarize(rows)
        self.assertEqual(summary["exact_matches"], 1)
        self.assertEqual(summary["empty_outputs"], 1)
        self.assertEqual(summary["p95_seconds"], 3)
