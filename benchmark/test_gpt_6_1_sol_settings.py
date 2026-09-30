"""Check the Chat Completions settings required by GPT-6.1 Sol."""

import os
import unittest
from types import SimpleNamespace
from unittest.mock import patch

from sumibi_bench import SumibiBench
from sumibi_typical_convert_client import SumibiTypicalConvertClient


class GPT61SolSettingsTest(unittest.TestCase):
    def test_benchmark_selects_supported_parameters(self):
        with patch.dict(os.environ, {"SUMIBI_AI_MODEL": "gpt-6.1-sol"}):
            with patch("sumibi_bench.SumibiTypicalConvertClient") as client_class:
                SumibiBench()

        self.assertEqual(
            client_class.call_args.kwargs,
            {
                "model": "gpt-6.1-sol",
                "temperature": None,
                "reasoning_effort": "low",
                "verbosity": "low",
            },
        )

    def test_existing_gpt6_sol_keeps_none_reasoning(self):
        with patch.dict(os.environ, {"SUMIBI_AI_MODEL": "gpt-6-sol"}):
            with patch("sumibi_bench.SumibiTypicalConvertClient") as client_class:
                SumibiBench()

        self.assertEqual(client_class.call_args.kwargs["reasoning_effort"], "none")
        self.assertEqual(client_class.call_args.kwargs["temperature"], 1.0)

    def test_request_omits_temperature_when_reasoning_is_enabled(self):
        response = SimpleNamespace(
            choices=[SimpleNamespace(message=SimpleNamespace(content="私は学生です"))]
        )
        with patch("sumibi_typical_convert_client.OpenAI") as openai_class:
            openai_class.return_value.chat.completions.create.return_value = response
            client = SumibiTypicalConvertClient(
                api_key="test-key",
                model="gpt-6.1-sol",
                temperature=None,
                reasoning_effort="low",
                verbosity="low",
            )
            self.assertEqual(client.convert("こんにちは。", "わたしは学生です"), "私は学生です")

        params = openai_class.return_value.chat.completions.create.call_args.kwargs
        self.assertNotIn("temperature", params)
        self.assertEqual(params["reasoning_effort"], "low")
        self.assertEqual(params["verbosity"], "low")


if __name__ == "__main__":
    unittest.main()
