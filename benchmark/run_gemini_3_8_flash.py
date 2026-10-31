#!/usr/bin/env python3
"""Issue #197: checkpointed Gemini 3.8 Flash measurement, user-supplied key only.

No automatic retries, no implicit overwrites, and no raw exception text in logs.
"""
import argparse
import contextlib
from datetime import date, datetime, timezone
import hashlib
import io
import json
import os
from pathlib import Path
import platform
import time

from sumibi_bench import SumibiBench
from run_gpt_6_1_terra import MODES, save

ROOT = Path(__file__).resolve().parent
MODEL = "gemini-3.8-flash"
BASE_URL = "https://generativelanguage.googleapis.com/v1beta/openai/"
PRICES = {"input": 0.75, "cached_input": 0.075, "output": 3.75}


def reference_cost(usage):
    """Paid Standard reference, not the project's actual billed amount.

Use the compatible API's completion_tokens as reported (do not double-count
reasoning_tokens). Missing usage is unknown, never a zero-cost request.
"""
    if not isinstance(usage, dict):
        return None
    prompt, completion = usage.get("prompt_tokens"), usage.get("completion_tokens")
    cached = (usage.get("prompt_tokens_details") or {}).get("cached_tokens", 0)
    if not all(type(n) is int and n >= 0 for n in (prompt, completion, cached)) or cached > prompt:
        return None
    return ((prompt - cached) * PRICES["input"] + cached * PRICES["cached_input"]
            + completion * PRICES["output"]) / 1_000_000


def response_status(metadata):
    reason = metadata.get("finish_reason") or ""
    if metadata.get("block_reason") or reason.split(":", 1)[0].strip() == "content_filter":
        return "blocked"
    if metadata.get("empty_choices") or not metadata.get("finish_reason"):
        raise ValueError("Missing response choice or metadata")
    return "ok"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mode", choices=tuple(MODES) + ("all",), default="all")
    parser.add_argument("--limit", type=int, default=200)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    # Check before opening output files, reading credentials elsewhere, or SDK construction.
    key = os.getenv("SUMIBI_AI_API_KEY")
    if not key or not key.strip():
        parser.error("Set SUMIBI_AI_API_KEY to your Gemini key in this same shell first")
    if datetime.now(timezone.utc).date() > date(2026, 12, 31):
        parser.error("Reference prices have expired; verify official pricing before running")
    dataset = ROOT / "AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json"
    entries = json.loads(dataset.read_text(encoding="utf-8"))
    if not 1 <= args.limit <= min(200, len(entries)):
        parser.error("limit must be within 1..200 and the dataset size")
    modes = list(MODES) if args.mode == "all" else [args.mode]
    targets = [args.output_dir / (MODEL + MODES[m] + ".json") for m in modes]
    audit_path = args.output_dir / (MODEL + "_run.json")
    if any(path.exists() for path in targets + [audit_path]):
        parser.error("Output exists; choose an unused directory (no implicit reruns)")
    # The user explicitly selected this key. Never fall back to OPENAI_API_KEY,
    # keychain, inherited proxy base URLs, or another model.
    os.environ["SUMIBI_AI_BASEURL"] = BASE_URL
    os.environ["SUMIBI_AI_MODEL"] = MODEL
    args.output_dir.mkdir(parents=True, exist_ok=True)
    audit = {"model": MODEL, "started_at": datetime.now(timezone.utc).isoformat(),
             "endpoint": BASE_URL + "chat/completions", "reasoning_effort": "low",
             "temperature": 1.0, "verbosity": None, "sdk_retries": 0,
             "key_source": "SUMIBI_AI_API_KEY", "limit_per_mode": args.limit,
             "dataset_sha256": hashlib.sha256(dataset.read_bytes()).hexdigest(),
             "client_sha256": hashlib.sha256((ROOT / "sumibi_typical_convert_client.py").read_bytes()).hexdigest(),
             "benchmark_sha256": hashlib.sha256((ROOT / "sumibi_bench.py").read_bytes()).hexdigest(),
             "python": platform.python_version(), "price_usd_per_million": PRICES,
             "pricing_valid_through": "2026-12-31", "complete": False, "attempts": []}
    save(audit_path, audit)
    for mode, output in zip(modes, targets):
        bench = SumibiBench(mode)
        for index, entry in [(0, entries[0]), (0, entries[0])] + list(enumerate(entries[:args.limit], 1)):
            attempt = {"mode": mode, "index": index, "warmup": index == 0,
                       "started_at": datetime.now(timezone.utc).isoformat()}
            started = time.perf_counter()
            try:
                romaji = bench.romaji_converter.convert(entry["input"])
                context = entry.get("context_text", "")
                with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                    bench.henkan(entry["expected_output"], context + romaji, romaji,
                                 entry["input"], context, skip_save=index == 0)
                attempt.update(bench.client.last_response_metadata)
                attempt["status"] = response_status(attempt)
                attempt["reference_usd"] = reference_cost(attempt.get("usage"))
            except Exception as exc:
                attempt.update(status="error", error=type(exc).__name__,
                               http_status=getattr(exc, "status_code", None))
            attempt["elapsed_sec"] = time.perf_counter() - started
            audit["attempts"].append(attempt)
            save(audit_path, audit)
            if attempt["status"] == "error":
                raise SystemExit("Stopped; safe details are in the audit JSON. No automatic retry.")
            save(output, bench.result_arr)
            if index % 20 == 0 or index == args.limit:
                phase = "warmup" if index == 0 else f"{index}/{args.limit}"
                print(f"{mode}: {phase}, calls={len(audit['attempts'])}, status={attempt['status']}", flush=True)
    audit["complete"] = True
    audit["finished_at"] = datetime.now(timezone.utc).isoformat()
    audit["blocked_calls"] = sum(a["status"] == "blocked" for a in audit["attempts"])
    audit["usage_unknown_calls"] = sum(a.get("reference_usd") is None for a in audit["attempts"])
    audit["reference_usd_known_usage_only"] = sum(a.get("reference_usd") or 0 for a in audit["attempts"])
    save(audit_path, audit)
    print(f"Complete: blocked={audit['blocked_calls']}, unknown usage={audit['usage_unknown_calls']}, "
          f"paid-tier reference USD={audit['reference_usd_known_usage_only']:.6f}")


if __name__ == "__main__":
    main()
