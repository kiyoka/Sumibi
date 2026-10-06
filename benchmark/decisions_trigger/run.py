#!/usr/bin/env python3
"""Issue #191: comparable, checkpointed Decisions/Jev trigger measurements."""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import subprocess
import sys
import tempfile
import time
from urllib import error, request

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "jev_trigger"))
import bench

DATASETS = {
    "dev": (bench.CASES,), "additional": (bench.ADDITIONAL_CASES,),
    "english": (bench.ENGLISH_CASES,), "punctuation": (bench.PUNCTUATION_CASES,),
    "all": (bench.CASES, bench.ADDITIONAL_CASES, bench.ENGLISH_CASES, bench.PUNCTUATION_CASES),
}
PRICES = {"decisions": 0.10, "jev": bench.INPUT_USD_PER_MILLION}
ENDPOINTS = {"decisions": "https://api.openai.com/v1/decisions", "jev": bench.API_URL}
MODELS = {"decisions": "gpt-6-luna", "jev": "jev-latest"}
THRESHOLDS = (0.5, 0.6, 0.7, 0.8, 0.9, 0.95)


def utc_now():
    return datetime.now(timezone.utc).isoformat()


def load_rows(dataset, case_id=None):
    cases = [c for path in DATASETS[dataset] for c in json.loads(path.read_text(encoding="utf-8"))]
    if case_id:
        cases = [c for c in cases if c["id"] == case_id]
        if not cases:
            raise ValueError("case-id not found in dataset")
    return list(bench.expand_cases(cases))


def build_payload(row, backend):
    jev = bench.build_request(row, "v2")
    if backend == "jev":
        return jev
    question = bench.QUESTION_V2
    # Preserve criteria semantics; Decisions has no Jev-style criteria field.
    instructions = (question["instructions"] + "\nTrue criteria: " + question["criteria"]["true"]
                    + "\nFalse criteria: " + question["criteria"]["false"])
    return {"model": MODELS[backend],
            "input": json.dumps(jev["state"], ensure_ascii=False),
            "questions": [{"type": "predicate", "name": "convert_now", "instructions": instructions}]}


def parse_response(result, backend):
    """Return validated score, usage and model; refusals are not negative scores."""
    if backend == "decisions":
        answers = [a for a in result["answers"] if a.get("name") == "convert_now"]
        if len(answers) != 1 or answers[0].get("type") != "predicate":
            raise ValueError("missing/refused/duplicate predicate")
        score = answers[0]["probability"]
    else:
        score = result["answers"]["convert_now"]["noul"]
    tokens = result["usage"]["input_tokens"]
    model = result["model"]
    if type(score) not in (int, float) or not math.isfinite(score) or not 0 <= score <= 1:
        raise ValueError("invalid probability")
    if type(tokens) is not int or tokens < 0 or not isinstance(model, str) or not model:
        raise ValueError("invalid usage/model")
    return float(score), tokens, model


def fetch(row, backend, api_key, timeout):
    """One request, no automatic retries, no body or exception text in logs."""
    req = request.Request(ENDPOINTS[backend], method="POST",
                          data=json.dumps(build_payload(row, backend), ensure_ascii=False).encode("utf-8"),
                          headers={"Authorization": "Bearer " + api_key, "Content-Type": "application/json"})
    started = time.monotonic()
    tokens = None
    try:
        with request.urlopen(req, timeout=timeout) as response:
            result = json.load(response)
        candidate = result.get("usage", {}).get("input_tokens")
        if type(candidate) is int and candidate >= 0:
            tokens = candidate
        score, tokens, model = parse_response(result, backend)
        record = {"status": "ok", "score": score, "input_tokens": tokens, "model": model}
    except error.HTTPError as exc:
        record = {"status": "error", "score": None, "error": "http_" + str(exc.code),
                  "input_tokens": tokens}
        exc.close()
    except (TimeoutError, error.URLError, OSError):
        record = {"status": "error", "score": None, "error": "network_or_timeout", "input_tokens": tokens}
    except (ValueError, KeyError, TypeError, AttributeError):
        record = {"status": "error", "score": None, "error": "invalid_or_refused_response", "input_tokens": tokens}
    record["latency_ms"] = round((time.monotonic() - started) * 1000, 3)
    return record


def summarize(rows, threshold):
    measured = [r for r in rows if r.get("status", "ok") == "ok"]
    excluded = [r for r in rows if r.get("status") == "excluded"]
    failures = [r for r in rows if r.get("status") == "error"]
    attempts = [a for r in rows for a in r.get("attempts", [r]) if a.get("status") != "excluded"]
    latencies = [r["latency_ms"] for r in attempts if r.get("latency_ms") is not None]
    return {"measured_keystrokes": len(measured), "locally_excluded_keystrokes": len(excluded),
            "failed_keystrokes": len(failures),
            "errors": dict(Counter(r["error"] for r in failures)),
            "attempt_errors": dict(Counter(a["error"] for a in attempts if a.get("status") == "error")),
            "overall": bench.metrics(measured, threshold),
            "preference_summary": bench.preference_summary(measured, threshold),
            "english_safety": bench.english_safety(measured, threshold),
            "threshold_sweep": {str(t): bench.preference_summary(measured, t) for t in THRESHOLDS},
            "latency_ms": {label: bench.percentile(latencies, fraction)
                           for label, fraction in (("p50", .5), ("p95", .95), ("p99", .99))},
            "latency_scope": "all completed API attempts including failures"}


def atomic_save(path, result):
    """Replace only this explicitly selected checkpoint, never stdout-redirection."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temp = None
    try:
        with tempfile.NamedTemporaryFile(mode="w", dir=path.parent, encoding="utf-8", delete=False) as file:
            temp = Path(file.name)
            json.dump(result, file, ensure_ascii=False, indent=2, allow_nan=False)
            file.write("\n")
        os.replace(temp, path)
    finally:
        if temp and temp.exists():
            temp.unlink()


def config_for(args, rows):
    return {"backend": args.backend, "dataset": args.dataset, "case_id": args.case_id,
            "prompt_version": "v2", "model": MODELS[args.backend], "endpoint": ENDPOINTS[args.backend],
            "threshold": args.threshold, "timeout": args.timeout,
            "dataset_sha256": hashlib.sha256(json.dumps(rows, sort_keys=True,
                                                        ensure_ascii=False).encode()).hexdigest(),
            "prompt_sha256": hashlib.sha256(json.dumps(bench.QUESTION_V2, sort_keys=True).encode()).hexdigest()}


def get_api_key(backend, source="environment"):
    """Read only the explicitly selected key; never print/store keychain output."""
    if source == "keychain":
        if backend != "decisions" or sys.platform != "darwin":
            raise ValueError("keychain is only supported for Decisions on macOS")
        try:
            result = subprocess.run(
                ["/usr/bin/security", "find-internet-password", "-s", "api.openai.com", "-a", "apikey", "-w"],
                capture_output=True, text=True, timeout=30, check=False)
        except (OSError, subprocess.TimeoutExpired):
            raise ValueError("keychain lookup failed") from None
        if result.returncode != 0 or not result.stdout.strip():
            raise ValueError("keychain lookup failed")
        return result.stdout.rstrip("\r\n")
    key = os.environ.get("OPENAI_API_KEY" if backend == "decisions" else "TYPESAFE_API_KEY")
    if not key or not key.strip():
        raise ValueError("API key environment variable is required")
    return key


def run(args):
    rows = load_rows(args.dataset, args.case_id)
    count = sum(r["context"] not in bench.EXCLUDED_CONTEXTS for r in rows)
    if args.dry_run:
        # Not a tokenizer estimate: show the assumption explicitly.
        return {"dry_run": True, "cases": len({r["id"] for r in rows}), "total_keystrokes": len(rows),
                "required_api_calls": count, "locally_excluded": len(rows) - count,
                "input_usd_per_million": PRICES[args.backend],
                "assumed_tokens_per_call": 600,
                "illustrative_total_usd": round(count * 600 * PRICES[args.backend] / 1_000_000, 6),
                "note": "600 tokens/call is a planning assumption, not measured usage or a spending cap"}
    if args.output is None or args.max_calls is None:
        raise ValueError("--output and --max-calls are required for paid measurements")
    config = config_for(args, rows)
    result = {"schema_version": 1, **config, "config": config, "started_at": utc_now(),
              "environment": {"python": platform.python_version(), "platform": platform.platform()},
              "measurement_mode": "independent observations; sequential requests; no actual conversion",
              "rows": []}
    if args.output.exists():
        if not args.resume:
            raise ValueError("output exists; use --resume or choose another file")
        result = json.loads(args.output.read_text(encoding="utf-8"))
        if result.get("config") != config:
            raise ValueError("resume config/dataset/prompt mismatch")
        for actual, expected in zip(result["rows"], rows):
            if any(actual.get(k) != v for k, v in expected.items()):
                raise ValueError("invalid checkpoint rows")
        if len(result["rows"]) > len(rows):
            raise ValueError("invalid checkpoint length")
    api_key = get_api_key(args.backend, getattr(args, "key_source", "environment"))
    calls = 0
    for index, row in enumerate(rows):
        previous = result["rows"][index] if index < len(result["rows"]) else None
        if previous and (previous.get("status") != "error" or not getattr(args, "retry_errors", False)):
            continue
        if row["context"] in bench.EXCLUDED_CONTEXTS:
            record = {"status": "excluded", "score": None, "input_tokens": 0}
        else:
            if calls >= args.max_calls:
                break
            calls += 1
            record = fetch(row, args.backend, api_key, args.timeout)
        if previous:
            record["attempts"] = previous.get("attempts", [{k: v for k, v in previous.items() if k not in row}]) + [dict(record)]
            result["rows"][index] = {**row, **record}
        else:
            result["rows"].append({**row, **record})
        result["updated_at"] = utc_now()
        result["complete_dataset"] = len(result["rows"]) == len(rows)
        result["total_keystrokes"] = len(rows)
        result["summary"] = summarize(result["rows"], args.threshold)
        attempts = [a for r in result["rows"] for a in r.get("attempts", [r]) if a["status"] != "excluded"]
        tokens = sum(r["input_tokens"] or 0 for r in attempts)
        result["api_calls"] = len(attempts)
        result["model_versions"] = sorted({r["model"] for r in attempts if r.get("model")})
        result["input_tokens"] = tokens
        result["unknown_usage_attempts"] = sum(r["input_tokens"] is None for r in attempts)
        result["input_usd_per_million"] = PRICES[args.backend]
        result["estimated_usd"] = round(tokens * PRICES[args.backend] / 1_000_000, 8)
        result["estimated_usd_per_1000_keystrokes"] = round(
            result["estimated_usd"] * 1000 / len(result["rows"]), 6)
        atomic_save(args.output, result)
        if record.get("status") == "error":
            # Stop on any failure: don't repeatedly charge a bad request.
            print("stopped: " + record["error"] + "; checkpoint saved", file=sys.stderr)
            break
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--backend", choices=("decisions", "jev"), default="decisions")
    parser.add_argument("--dataset", choices=tuple(DATASETS), default="all")
    parser.add_argument("--case-id")
    parser.add_argument("--key-source", choices=("environment", "keychain"), default="environment",
                        help="keychain: macOS internet password, api.openai.com / apikey (Decisions only)")
    parser.add_argument("--max-calls", type=int, help="maximum paid attempts in THIS invocation, including failures")
    parser.add_argument("--threshold", type=float, default=.8)
    parser.add_argument("--timeout", type=float, default=10)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--resume", action="store_true")
    parser.add_argument("--retry-errors", action="store_true", help="with --resume, explicitly retry failed rows and preserve attempt history")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    if args.retry_errors and not args.resume:
        parser.error("--retry-errors requires --resume")
    if not math.isfinite(args.threshold) or not 0 <= args.threshold <= 1:
        parser.error("invalid threshold")
    if not math.isfinite(args.timeout) or args.timeout <= 0 or (args.max_calls is not None and args.max_calls < 1):
        parser.error("timeout and max-calls must be positive")
    try:
        result = run(args)
    except (OSError, ValueError, KeyError, TypeError):
        # Do not echo arbitrary exception messages (including secrets/HTTP bodies).
        parser.exit(1, "benchmark error: check API key, output path, options or resume configuration; existing output preserved\n")
    print(json.dumps({k: v for k, v in result.items() if k not in ("rows", "config")}, ensure_ascii=False, indent=2))
    if any(r.get("status") == "error" for r in result.get("rows", [])):
        parser.exit(1)


if __name__ == "__main__":
    main()
