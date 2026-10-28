"""Offline verification of GPT-6.1 Terra settings and measured-cost accounting."""
import os
import contextlib
import io
import json
from pathlib import Path
import tempfile
import unittest
from types import SimpleNamespace
from unittest.mock import patch

from sumibi_bench import SumibiBench
from sumibi_typical_convert_client import SumibiTypicalConvertClient
from run_gpt_6_1_terra import get_key, usage_cost
import run_gpt_6_1_terra as run


class TerraSettingsTest(unittest.TestCase):
    def test_benchmark_settings(self):
        with patch.dict(os.environ, {"SUMIBI_AI_MODEL": "gpt-6.1-terra"}):
            with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                SumibiBench()
        self.assertEqual(client.call_args.kwargs, dict(model="gpt-6.1-terra", temperature=1.0,
                         reasoning_effort="none", verbosity="low", max_retries=0, service_tier="default"))

    def test_request_and_usage(self):
        usage = {"prompt_tokens": 500, "completion_tokens": 200,
                 "prompt_tokens_details": {"cached_tokens": 0}}
        response = SimpleNamespace(choices=[SimpleNamespace(message=SimpleNamespace(content="日本語"),
                                  finish_reason="stop")], usage=SimpleNamespace(model_dump=lambda: usage),
                                  model="gpt-6.1-terra", service_tier="default")
        with patch("sumibi_typical_convert_client.OpenAI") as sdk:
            sdk.return_value.chat.completions.create.return_value = response
            client = SumibiTypicalConvertClient(api_key="fake", model="gpt-6.1-terra",
                      temperature=1.0, reasoning_effort="none", verbosity="low",
                      max_retries=0, service_tier="default")
            self.assertEqual(client.convert("日本語", "nihongo"), "日本語")
        self.assertEqual(sdk.call_args.kwargs["max_retries"], 0)
        params = sdk.return_value.chat.completions.create.call_args.kwargs
        self.assertEqual(params["service_tier"], "default")
        self.assertEqual(params["reasoning_effort"], "none")
        self.assertEqual(client.last_response_metadata["usage"], usage)
        self.assertAlmostEqual(usage_cost(usage), 0.0015)
        self.assertAlmostEqual(usage_cost({**usage, "prompt_tokens_details": {"cached_tokens": 100}}), 0.001405)
        self.assertIsNone(usage_cost(None))

    def test_keychain_target(self):
        with patch("run_gpt_6_1_terra.subprocess.run") as command:
            command.return_value = SimpleNamespace(returncode=0, stdout="fake\n")
            self.assertEqual(get_key("keychain"), "fake")
        self.assertEqual(command.call_args.args[0], ["/usr/bin/security", "find-internet-password",
                         "-s", "api.openai.com", "-a", "apikey", "-w"])
        self.assertTrue(command.call_args.kwargs["capture_output"])

    def test_keychain_failure_does_not_expose_output(self):
        with patch("run_gpt_6_1_terra.subprocess.run") as command:
            command.return_value = SimpleNamespace(returncode=1, stdout="private", stderr="private")
            with self.assertRaisesRegex(ValueError, "^OpenAI keychain lookup failed$"):
                get_key("keychain")

    def test_runner_counts_warmups_and_refuses_overwrite(self):
        metadata = {"model": "gpt-6.1-terra", "finish_reason": "stop", "service_tier": "default",
                    "usage": {"prompt_tokens": 500, "completion_tokens": 200}}
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ), patch.object(run, "get_key", return_value="fake") as key:
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.return_value = "日本語"
                    client.return_value.last_response_metadata = metadata
                    argv = ["run", "--limit", "1", "--output-dir", directory]
                    with patch("sys.argv", argv), contextlib.redirect_stdout(io.StringIO()):
                        run.main()
                    audit = json.loads((Path(directory) / "gpt-6.1-terra_run.json").read_text())
                    self.assertTrue(audit["complete"])
                    self.assertEqual(len(audit["attempts"]), 9)
                    self.assertEqual(sum(a["warmup"] for a in audit["attempts"]), 6)
                    self.assertAlmostEqual(audit["estimated_usd"], 0.0135)
                    for suffix in ("", "_hiragana", "_katakana"):
                        self.assertEqual(len(json.loads((Path(directory) / f"gpt-6.1-terra{suffix}.json").read_text())), 1)
                    key.reset_mock()
                    with patch("sys.argv", argv), contextlib.redirect_stderr(io.StringIO()):
                        with self.assertRaises(SystemExit):
                            run.main()
                    key.assert_not_called()

    def test_runner_preserves_safe_failure_checkpoint(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ), patch.object(run, "get_key", return_value="fake"):
                with patch("sumibi_bench.SumibiTypicalConvertClient") as client:
                    client.return_value.convert.side_effect = RuntimeError("private server response")
                    with patch("sys.argv", ["run", "--limit", "1", "--output-dir", directory]):
                        with self.assertRaises(SystemExit):
                            run.main()
                    audit_text = (Path(directory) / "gpt-6.1-terra_run.json").read_text()
                    audit = json.loads(audit_text)
                    self.assertFalse(audit["complete"])
                    self.assertEqual(audit["attempts"][0]["error"], "RuntimeError")
                    self.assertNotIn("private", audit_text)
                    self.assertNotIn("fake", audit_text)
                    self.assertEqual(client.return_value.convert.call_count, 1)


if __name__ == "__main__":
    unittest.main()
