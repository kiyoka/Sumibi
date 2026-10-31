"""Issue #202 tests without paid requests or real keys."""
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

import run_gemini_4_argon as run
from sumibi_bench import SumibiBench
from sumibi_typical_convert_client import SumibiTypicalConvertClient


class ArgonSettingsTest(unittest.TestCase):
    def setUp(self):
        clock = patch.object(run.shared, "datetime", wraps=run.shared.datetime)
        self.clock = clock.start()
        self.clock.now.return_value = datetime(2026, 10, 31, tzinfo=timezone.utc)
        self.addCleanup(clock.stop)

    def test_supported_settings(self):
        with patch.dict(os.environ, {"SUMIBI_AI_MODEL": run.MODEL}):
            with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                SumibiBench()
        self.assertEqual(client.call_args.kwargs, {"model": run.MODEL, "temperature": None,
                         "reasoning_effort": "low", "verbosity": None, "max_retries": 0})

    def test_request_and_no_sampling_parameters(self):
        response = SimpleNamespace(choices=[SimpleNamespace(
            message=SimpleNamespace(content="日本語"), finish_reason="stop")], usage=None)
        with patch("sumibi_typical_convert_client.OpenAI") as sdk:
            sdk.return_value.chat.completions.create.return_value = response
            client = SumibiTypicalConvertClient(api_key="fake", base_url=run.shared.BASE_URL,
                      model=run.MODEL, temperature=None, reasoning_effort="low", max_retries=0)
            self.assertEqual(client.convert("日本語", "nihongo"), "日本語")
        args = sdk.return_value.chat.completions.create.call_args.kwargs
        self.assertEqual(args["model"], run.MODEL)
        self.assertEqual(args["reasoning_effort"], "low")
        for name in ("temperature", "top_p", "top_k", "verbosity", "service_tier"):
            self.assertNotIn(name, args)

    def test_smoke_model_price_and_token_gap(self):
        metadata = {"model": run.MODEL, "finish_reason": "stop", "empty_choices": False,
                    "usage": {"prompt_tokens": 500, "completion_tokens": 200, "total_tokens": 710}}
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake",
                                        "SUMIBI_AI_TIMEOUT": "120",
                                        "SUMIBI_AI_BASEURL": "https://example.invalid"}, clear=True):
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    with patch("sys.argv", ["run", "--limit", "1", "--output-dir", directory]), contextlib.redirect_stdout(io.StringIO()):
                        run.main()
                    self.assertEqual(os.environ["SUMIBI_AI_BASEURL"], run.shared.BASE_URL)
                    text = (Path(directory) / f"{run.MODEL}_run.json").read_text()
                    self.assertNotIn("fake", text)
                    a = json.loads(text)
                    self.assertEqual(a["model"], run.MODEL)
                    self.assertEqual(a["reasoning_effort"], "low")
                    self.assertIsNone(a["temperature"])
                    self.assertEqual(a["timeout_seconds"], 120)
                    self.assertEqual(a["pricing_valid_through"], "2026-11-30")
                    self.assertEqual(a["price_usd_per_million"], run.PRICES)
                    self.assertEqual(len(a["attempts"]), 9)
                    self.assertEqual(a["unitemized_token_gap"], 90)
                    self.assertAlmostEqual(a["reference_usd_known_usage_only"], .027)
                    for suffix in ("", "_hiragana", "_katakana"):
                        self.assertEqual(len(json.loads((Path(directory) / f"{run.MODEL}{suffix}.json").read_text())), 1)

    def test_no_openai_fallback(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "new"
            with patch.dict(os.environ, {"OPENAI_API_KEY": "fake-openai"}, clear=True):
                with patch("sys.argv", ["run", "--output-dir", str(output)]), contextlib.redirect_stderr(io.StringIO()):
                    with patch.object(run.shared, "SumibiBench") as bench:
                        with self.assertRaises(SystemExit):
                            run.main()
                        bench.assert_not_called()
            self.assertFalse(output.exists())

    def test_introductory_price_expiry_stops_before_requests(self):
        self.clock.now.return_value = datetime(2026, 12, 1, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "new"
            with patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake"}, clear=True):
                with patch("sys.argv", ["run", "--output-dir", str(output)]), contextlib.redirect_stderr(io.StringIO()):
                    with patch.object(run.shared, "SumibiBench") as bench:
                        with self.assertRaises(SystemExit):
                            run.main()
                        bench.assert_not_called()
            self.assertFalse(output.exists())

    def test_invalid_timeout_stops_before_requests(self):
        for value in ("private", "nan", "inf", "0", "-1"):
            with tempfile.TemporaryDirectory() as directory:
                output = Path(directory) / "new"
                env = {"SUMIBI_AI_API_KEY": "fake", "SUMIBI_AI_TIMEOUT": value}
                with patch.dict(os.environ, env, clear=True):
                    with patch("sys.argv", ["run", "--output-dir", str(output)]), contextlib.redirect_stderr(io.StringIO()):
                        with patch.object(run.shared, "SumibiBench") as bench:
                            with self.assertRaises(SystemExit):
                                run.main()
                            bench.assert_not_called()
                self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
