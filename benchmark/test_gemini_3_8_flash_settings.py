"""Issue #197 offline tests; no real credentials or API calls."""
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

import run_gemini_3_8_flash as run
from sumibi_bench import SumibiBench
from sumibi_typical_convert_client import SumibiTypicalConvertClient


class Gemini38Test(unittest.TestCase):
    def setUp(self):
        clock = patch.object(run, "datetime", wraps=run.datetime)
        self.clock = clock.start()
        self.clock.now.return_value = datetime(2026, 10, 31, tzinfo=timezone.utc)
        self.addCleanup(clock.stop)

    def test_settings_and_older_models_unchanged(self):
        for model, effort in (("gemini-3.8-flash", "low"), ("gemini-3.7-flash", "none"),
                              ("gemini-3.6-flash", "minimal")):
            with patch.dict(os.environ, {"SUMIBI_AI_MODEL": model}):
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    SumibiBench()
            params = client.call_args.kwargs
            self.assertEqual(params["reasoning_effort"], effort)
            self.assertEqual(params["temperature"], 1.0)
            self.assertIsNone(params["verbosity"])
            if model == run.MODEL:
                self.assertEqual(params["max_retries"], 0)
                self.assertNotIn("service_tier", params)

    def test_compatible_request(self):
        response = SimpleNamespace(choices=[SimpleNamespace(message=SimpleNamespace(content="日本語"),
                                   finish_reason="stop")], usage=None)
        with patch("sumibi_typical_convert_client.OpenAI") as sdk:
            sdk.return_value.chat.completions.create.return_value = response
            client = SumibiTypicalConvertClient(api_key="fake", base_url=run.BASE_URL,
                      model=run.MODEL, temperature=1.0, reasoning_effort="low", max_retries=0)
            self.assertEqual(client.convert("日本語", "nihongo"), "日本語")
        self.assertEqual(sdk.call_args.kwargs["base_url"], run.BASE_URL.rstrip("/"))
        params = sdk.return_value.chat.completions.create.call_args.kwargs
        self.assertEqual(params["model"], run.MODEL)
        self.assertEqual(params["reasoning_effort"], "low")
        self.assertNotIn("verbosity", params)
        self.assertNotIn("service_tier", params)

    def test_empty_choices_safety_block(self):
        response = SimpleNamespace(choices=[], usage=None,
                    model_extra={"promptFeedback": {"blockReason": "PROHIBITED_CONTENT"}})
        with patch("sumibi_typical_convert_client.OpenAI") as sdk:
            sdk.return_value.chat.completions.create.return_value = response
            client = SumibiTypicalConvertClient(api_key="fake", base_url=run.BASE_URL, model=run.MODEL)
            with contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(client.convert("日本語", "nihongo"), "")
        self.assertEqual(run.response_status(client.last_response_metadata), "blocked")
        self.assertIsNone(run.reference_cost(client.last_response_metadata["usage"]))

    def test_missing_key_never_falls_back_or_creates_output(self):
        with tempfile.TemporaryDirectory() as temp:
            output = Path(temp) / "new"
            with patch.dict(os.environ, {"OPENAI_API_KEY": "not-for-gemini"}, clear=True):
                with patch("sys.argv", ["run", "--output-dir", str(output)]):
                    with patch.object(run, "SumibiBench") as bench, contextlib.redirect_stderr(io.StringIO()):
                        with self.assertRaises(SystemExit):
                            run.main()
                        bench.assert_not_called()
            self.assertFalse(output.exists())

    def test_reference_cost_and_unknown_usage(self):
        self.assertAlmostEqual(run.reference_cost({"prompt_tokens": 500, "completion_tokens": 200}), .001125)
        self.assertAlmostEqual(run.reference_cost({"prompt_tokens": 500, "completion_tokens": 200,
                               "prompt_tokens_details": {"cached_tokens": 100}}), .0010575)
        self.assertIsNone(run.reference_cost(None))
        self.assertIsNone(run.reference_cost({"prompt_tokens": 500}))
        with self.assertRaises(ValueError):
            run.response_status({"empty_choices": True})

    def test_filter_reason_with_provider_suffix(self):
        for reason in ("content_filter", "content_filter: OTHER", "content_filter: PROHIBITED_CONTENT"):
            self.assertEqual(run.response_status({"finish_reason": reason}), "blocked")

    def test_expired_prices_require_review_before_api_calls(self):
        self.clock.now.return_value = datetime(2027, 1, 1, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "new"
            with patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake"}, clear=True):
                with patch("sys.argv", ["run", "--output-dir", str(output)]):
                    with patch.object(run, "SumibiBench") as bench, contextlib.redirect_stderr(io.StringIO()):
                        with self.assertRaises(SystemExit):
                            run.main()
                        bench.assert_not_called()
            self.assertFalse(output.exists())

    def test_smoke_checkpoint_and_overwrite_protection(self):
        metadata = {"model": run.MODEL, "finish_reason": "stop", "empty_choices": False,
                    "usage": {"prompt_tokens": 500, "completion_tokens": 200}}
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake",
                                        "SUMIBI_AI_BASEURL": "https://example.invalid"}, clear=True):
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    args = ["run", "--limit", "1", "--output-dir", directory]
                    with patch("sys.argv", args), contextlib.redirect_stdout(io.StringIO()):
                        run.main()
                    self.assertEqual(os.environ["SUMIBI_AI_BASEURL"], run.BASE_URL)
                    text = (Path(directory) / (run.MODEL + "_run.json")).read_text()
                    self.assertNotIn("fake", text)
                    audit = json.loads(text)
                    self.assertTrue(audit["complete"])
                    self.assertEqual(len(audit["attempts"]), 9)
                    self.assertEqual(audit["usage_unknown_calls"], 0)
                    self.assertAlmostEqual(audit["reference_usd_known_usage_only"], .010125)
                    for suffix in ("", "_hiragana", "_katakana"):
                        self.assertEqual(len(json.loads((Path(directory) / f"{run.MODEL}{suffix}.json").read_text())), 1)
                    calls = client.return_value.convert.call_count
                    with patch("sys.argv", args), contextlib.redirect_stderr(io.StringIO()):
                        with self.assertRaises(SystemExit):
                            run.main()
                    self.assertEqual(client.return_value.convert.call_count, calls)

    def test_error_stops_without_raw_response_or_retry(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ, {"SUMIBI_AI_API_KEY": "fake"}, clear=True):
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.side_effect = RuntimeError("private server response")
                    with patch("sys.argv", ["run", "--limit", "1", "--output-dir", directory]):
                        with self.assertRaises(SystemExit):
                            run.main()
                    text = (Path(directory) / (run.MODEL + "_run.json")).read_text()
                    self.assertNotIn("private", text)
                    self.assertNotIn("fake", text)
                    self.assertEqual(client.return_value.convert.call_count, 1)
                    self.assertEqual(json.loads(text)["attempts"][0]["error"], "RuntimeError")


if __name__ == "__main__":
    unittest.main()
