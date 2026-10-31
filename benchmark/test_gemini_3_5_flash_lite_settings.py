"""Issue #201 offline checks; never contact Gemini or load real credentials."""
import contextlib
from datetime import datetime, timezone
import io
import json
import os
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import run_gemini_3_5_flash_lite as run
from sumibi_bench import SumibiBench
from sumibi_typical_convert_client import SumibiTypicalConvertClient


class FlashLiteSettingsTest(unittest.TestCase):
    def setUp(self):
        clock = patch.object(run.shared, "datetime", wraps=run.shared.datetime)
        self.clock = clock.start()
        self.clock.now.return_value = datetime(2026, 10, 31, tzinfo=timezone.utc)
        self.addCleanup(clock.stop)

    def test_explicit_model_settings(self):
        with patch.dict(os.environ, {"SUMIBI_AI_MODEL": run.MODEL}):
            with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                SumibiBench()
        self.assertEqual(client.call_args.kwargs, {
            "model": run.MODEL, "temperature": None, "reasoning_effort": "minimal",
            "verbosity": None, "max_retries": 0})

    def test_request_omits_deprecated_parameters(self):
        response = SimpleNamespace(choices=[SimpleNamespace(
            message=SimpleNamespace(content="日本語"), finish_reason="stop")], usage=None)
        with patch("sumibi_typical_convert_client.OpenAI") as sdk:
            sdk.return_value.chat.completions.create.return_value = response
            client = SumibiTypicalConvertClient(api_key="fake", base_url=run.shared.BASE_URL,
                model=run.MODEL, temperature=None, reasoning_effort="minimal", max_retries=0)
            self.assertEqual(client.convert("日本語", "nihongo"), "日本語")
        args = sdk.return_value.chat.completions.create.call_args.kwargs
        self.assertEqual(args["model"], run.MODEL)
        self.assertEqual(args["reasoning_effort"], "minimal")
        for parameter in ("temperature", "top_p", "top_k", "verbosity", "service_tier"):
            self.assertNotIn(parameter, args)

    def test_smoke_uses_own_model_prices_and_records_token_gap(self):
        usage = {"prompt_tokens": 500, "completion_tokens": 200, "total_tokens": 710}
        metadata = {"model": run.MODEL, "finish_reason": "stop", "empty_choices": False, "usage": usage}
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake",
                                        "SUMIBI_AI_BASEURL": "https://example.invalid"}, clear=True):
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    with patch("sys.argv", ["run", "--limit", "1", "--output-dir", directory]), contextlib.redirect_stdout(io.StringIO()):
                        run.main()
                    self.assertEqual(os.environ["SUMIBI_AI_BASEURL"], run.shared.BASE_URL)
                    text = (Path(directory) / f"{run.MODEL}_run.json").read_text()
                    self.assertNotIn("fake", text)
                    audit = json.loads(text)
                    self.assertEqual(audit["model"], run.MODEL)
                    self.assertEqual(audit["reasoning_effort"], "minimal")
                    self.assertIsNone(audit["temperature"])
                    self.assertEqual(audit["price_usd_per_million"], run.PRICES)
                    self.assertEqual(len(audit["attempts"]), 9)
                    self.assertEqual(audit["unitemized_token_gap"], 90)
                    self.assertEqual(audit["token_gap_unknown_calls"], 0)
                    self.assertAlmostEqual(audit["reference_usd_known_usage_only"], .00585)
                    for suffix in ("", "_hiragana", "_katakana"):
                        self.assertEqual(len(json.loads((Path(directory) / f"{run.MODEL}{suffix}.json").read_text())), 1)

    def test_no_openai_key_fallback(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "new"
            with patch.dict(os.environ, {"OPENAI_API_KEY": "fake-openai"}, clear=True):
                with patch("sys.argv", ["run", "--output-dir", str(output)]), contextlib.redirect_stderr(io.StringIO()):
                    with patch.object(run.shared, "SumibiBench") as bench:
                        with self.assertRaises(SystemExit):
                            run.main()
                        bench.assert_not_called()
            self.assertFalse(output.exists())

    def test_unknown_token_gap_is_not_zero(self):
        self.assertIsNone(run.shared.unitemized_tokens({"prompt_tokens": 500, "completion_tokens": 200}))
        self.assertEqual(run.shared.unitemized_tokens({"prompt_tokens": 500, "completion_tokens": 200, "total_tokens": 700}), 0)
        self.assertAlmostEqual(run.shared.reference_cost({"prompt_tokens": 500, "completion_tokens": 200}, run.PRICES), .00065)


if __name__ == "__main__":
    unittest.main()
