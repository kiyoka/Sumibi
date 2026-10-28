#!/usr/bin/env python3
"""Issue #198: sequential Standard measurements with safe keychain and checkpoints.

No automatic retries. Existing runs are never overwritten. Failure records contain
only exception class/status, never credentials, headers, or raw server errors.
"""
import argparse
import contextlib
from datetime import datetime, timezone
import hashlib
import io
import json
import os
from pathlib import Path
import platform
import subprocess
import tempfile
import time

from sumibi_bench import SumibiBench

ROOT = Path(__file__).resolve().parent
MODES = {"romaji_direct_input": "", "hiragana_input": "_hiragana", "katakana_input": "_katakana"}


def save(path, data):
    """Atomically checkpoint JSON in the same directory."""
    with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=path.parent,
                                     delete=False) as stream:
        json.dump(data, stream, ensure_ascii=False, indent=2)
        temporary = stream.name
    os.replace(temporary, path)


def get_key(source):
    if source == "keychain":
        result = subprocess.run(
            ["/usr/bin/security", "find-internet-password", "-s", "api.openai.com",
             "-a", "apikey", "-w"], capture_output=True, text=True, timeout=30)
        if result.returncode or not result.stdout.strip():
            raise ValueError("OpenAI keychain lookup failed")
        return result.stdout.rstrip("\r\n")
    key = os.getenv("OPENAI_API_KEY")
    if not key:
        raise ValueError("OPENAI_API_KEY is required")
    return key


def usage_cost(usage):
    """Standard short-context estimate, excluding unknown failed usage/taxes."""
    if not usage:
        return None
    cached = (usage.get("prompt_tokens_details") or {}).get("cached_tokens", 0)
    return ((usage["prompt_tokens"] - cached) + cached * 0.05
            + usage["completion_tokens"] * 5) / 1_000_000


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--key-source", choices=("keychain", "environment"), default="keychain")
    parser.add_argument("--mode", choices=tuple(MODES) + ("all",), default="all")
    parser.add_argument("--limit", type=int, default=200)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    dataset = ROOT / "AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json"
    entries = json.loads(dataset.read_text(encoding="utf-8"))
    if not 1 <= args.limit <= len(entries):
        parser.error("limit must be within the dataset")
    modes = list(MODES) if args.mode == "all" else [args.mode]
    targets = [args.output_dir / ("gpt-6.1-terra" + MODES[m] + ".json") for m in modes]
    audit_path = args.output_dir / "gpt-6.1-terra_run.json"
    if any(path.exists() for path in targets + [audit_path]):
        parser.error("output exists; choose another directory (no implicit reruns)")
    key = get_key(args.key_source)
    # Override inherited Gemini/local settings before creating the API client.
    os.environ["SUMIBI_AI_MODEL"] = "gpt-6.1-terra"
    os.environ["SUMIBI_AI_BASEURL"] = "https://api.openai.com/v1"
    os.environ["SUMIBI_AI_API_KEY"] = key
    args.output_dir.mkdir(parents=True, exist_ok=True)
    audit = {"model": "gpt-6.1-terra", "started_at": datetime.now(timezone.utc).isoformat(),
             "endpoint": "https://api.openai.com/v1/chat/completions",
             "reasoning_effort": "none", "verbosity": "low", "temperature": 1.0,
             "service_tier": "default", "sdk_retries": 0,
             "dataset_sha256": hashlib.sha256(dataset.read_bytes()).hexdigest(),
             "client_sha256": hashlib.sha256((ROOT / "sumibi_typical_convert_client.py").read_bytes()).hexdigest(),
             "python": platform.python_version(), "limit_per_mode": args.limit,
             "price_usd_per_million": {"input": 1, "cached_input": 0.05, "output": 5},
             "complete": False, "attempts": []}
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
                # Existing benchmark prints raw exceptions on timeout; suppress
                # per-case output and retain only whitelisted audit fields.
                with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                    bench.henkan(entry["expected_output"], context + romaji, romaji,
                                 entry["input"], context, skip_save=index == 0)
                attempt.update(bench.client.last_response_metadata)
                if not attempt.get("usage"):
                    raise ValueError("missing response usage (possibly timeout)")
                attempt["estimated_usd"] = usage_cost(attempt["usage"])
                attempt["status"] = "ok"
            except Exception as exc:
                attempt.update(status="error", error=type(exc).__name__,
                               http_status=getattr(exc, "status_code", None))
            attempt["elapsed_sec"] = time.perf_counter() - started
            audit["attempts"].append(attempt)
            save(audit_path, audit)
            if attempt["status"] != "ok":
                raise SystemExit("Measurement stopped; inspect safe audit metadata. No automatic retry.")
            save(output, bench.result_arr)
            if index % 20 == 0 or index == args.limit:
                print(f"{mode}: {index}/{args.limit}, calls={len(audit['attempts'])}", flush=True)
    audit["complete"] = True
    audit["finished_at"] = datetime.now(timezone.utc).isoformat()
    audit["estimated_usd"] = sum(a["estimated_usd"] for a in audit["attempts"])
    save(audit_path, audit)
    print(f"Complete: estimated USD {audit['estimated_usd']:.6f}")


if __name__ == "__main__":
    main()
