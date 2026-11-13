#!/usr/bin/env python3
"""Issue #205: isolated, pinned JWTD_v2/v2 Gemini measurement (never v1)."""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import statistics
import tempfile
import time

ROOT = Path(__file__).resolve().parent
ISSUE_DIR = ROOT / "issue205"
REVISION = "c0783bba1ec6b9a9e06d07fa52ce3e23d0a7dfe1"
REPOSITORY = "https://github.com/nyanko3141592/AJIMEE-Bench"
DATA_PATH = "JWTD_v2/v2/evaluation_items.json"
DATA_HASH = "69a0dde4c1f67b3d06268c761ee3a7b7d38680a4f1c81360cc2d4f0f120e731f"
FILES = {
    DATA_PATH: DATA_HASH,
    "utils.py": "ed5ae2b05783a0b6856ca783e0fb169963a27c252fc7b9de4be0f0b04c8fd132",
    "README.md": "8b2b41e291fdac82efe6ba4cffc5bdd6c59637c8045dbb55f2620c2d086ae915",
}
IDENTITY = {"repository": REPOSITORY, "revision": REVISION,
            "path": DATA_PATH, "sha256": DATA_HASH, "count": 628}
MODES = ("romaji_direct_input", "hiragana_input", "katakana_input")
GEMINI_BASE_URL = "https://generativelanguage.googleapis.com/v1beta/openai/"
OPENAI_BASE_URL = "https://api.openai.com/v1/"
BASE_URL = GEMINI_BASE_URL
# Standard short-context prices. Hydrogen checked 2026-11-13, others 2026-10-31.
# Values: reasoning_effort, temperature, input/cached/output USD per million.
MODELS = {
    "gemini-2.5-pro": (None, None, (1.25, .125, 10)),
    "gemini-2.5-flash": (None, None, (.30, .03, 2.50)),
    "gemini-3-flash-preview": ("low", 1.0, (.50, .05, 3)),
    "gemini-3.5-flash-lite": ("minimal", None, (.30, .03, 2.50)),
    "gemini-3.6-flash": ("minimal", 1.0, (.75, .075, 3.75)),
    "gemini-3.7-flash": ("low", 1.0, (.75, .075, 3.75)),
    "gemini-3.8-flash": ("low", 1.0, (.75, .075, 3.75)),
    "gemini-4-argon": ("low", None, (2, .10, 10)),
    "gemini-4-hydrogen": ("low", None, (1.50, .075, 7.50)),
    "gpt-6.1-terra": ("none", 1.0, (1.0, 0.05, 5.0)),
    "gpt-6.1-sol": ("low", None, (2.0, 0.10, 10.0)),
}


def base_url_for_model(model):
    if model.startswith("gpt-"):
        return OPENAI_BASE_URL
    return GEMINI_BASE_URL

SKIPPED = {
    "gemini-2.0-flash": "shutdown 2026-06-01",
    "gemini-2.0-flash-lite": "shutdown 2026-06-01",
    "gemini-3-pro-preview": "shutdown 2026-03-09",
    "gemini-3.1-flash-lite-preview": "shutdown 2026-05-25",
}
SCORING = {"primary": "raw_MinCER", "secondary": "raw_accuracy@1",
           "width_normalized_accuracy": True, "right_context": False,
           "prompt": "sumibi_typical_convert_client", "n": 1}
LEGACY_RUNNER_HASH = "ba0e48c0092197d629cb512fdb8f5d41b4bb2bd5b817b72a1c85ef48b8c487a2"
MEASUREMENT_FILES = ("sumibi_typical_convert_client.py", "katakana_to_romaji_converter.py",
                     "katakana_to_hiragana_converter.py")


def measurement_hashes():
    return {name: digest(ROOT / name) for name in MEASUREMENT_FILES}


def schedule(entries, limit, converters):
    jobs = []
    for mode in MODES:
        for position, entry in [(0, entries[0])] * 2 + list(enumerate(entries[:limit], 1)):
            jobs.append((entry, {"mode": mode, "position": position,
                         "index": str(entry["index"]), "warmup": position == 0,
                         "input_text": converters[mode](entry["input"])}))
    return jobs


def resume_audit(path, model, limit, jobs, timeout):
    """Validate a checkpoint before any SDK construction or output mutation."""
    if not path.resolve().is_relative_to((ISSUE_DIR / "results").resolve()):
        raise ValueError("Resume input must be in issue205/results")
    a = json.loads(path.read_text())
    reasoning, temperature, prices = MODELS[model]
    base_url = base_url_for_model(model)
    expected = {"schema": "sumibi-jwtd-v2-run-1", "dataset": IDENTITY,
                "scoring": SCORING, "model": model, "limit": limit, "complete": False,
                "endpoint": base_url + "chat/completions", "sdk_retries": 0,
                "reasoning_effort": reasoning, "temperature": temperature,
                "timeout_seconds": timeout, "prices": list(prices),
                "pricing_valid_through": "2026-11-30" if model == "gemini-4-argon" else "2026-12-31",
                "alias_only": model == "gemini-3.7-flash",
                "alias_target": "gemini-3.8-flash" if model == "gemini-3.7-flash" else None}
    if not isinstance(a, dict) or any(a.get(k) != v for k, v in expected.items()):
        raise ValueError("Resume dataset/model/settings differ, or already complete")
    hashes = a.get("source_sha256", {})
    if (any(hashes.get(k) != v for k, v in measurement_hashes().items())
            or hashes.get("jwtd_v2.py") not in (LEGACY_RUNNER_HASH, digest(ROOT / "jwtd_v2.py"))):
        raise ValueError("Resume measurement code changed")
    rows = a.get("rows")
    if not isinstance(rows, list) or not rows or len(rows) > len(jobs):
        raise ValueError("Invalid resume coverage")
    for i, row in enumerate(rows):
        if any(row.get(k) != v for k, v in jobs[i][1].items()):
            raise ValueError("Resume input/order mismatch")
        if row.get("status") not in ("ok", "blocked", "error"):
            raise ValueError("Invalid attempt status")
        if row["status"] == "error" and i != len(rows) - 1:
            raise ValueError("Only the terminal failed attempt can be resumed")
        accepted = (model, expected["alias_target"]) if expected["alias_only"] else (model,)
        if row["status"] != "error" and (not isinstance(row.get("hypothesis"), str)
                or row.get("model") not in accepted):
            raise ValueError("Invalid saved successful response")
    # Preserve all failed attempts separately; only successes occupy evaluation slots.
    if rows[-1]["status"] == "error":
        a.setdefault("failed_attempts", []).append(rows.pop())
    history = {"checkpoint_sha256": digest(path),
               "checkpoint": str(path.relative_to(ISSUE_DIR)) if path.is_relative_to(ISSUE_DIR) else path.name,
               "started_at": datetime.now(timezone.utc).isoformat(),
               "preserved_successful_calls": len(rows), "remaining_calls": len(jobs) - len(rows),
               "runner_sha256": digest(ROOT / "jwtd_v2.py")}
    a.setdefault("resume_history", []).append(history)
    return a


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def save(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=path.parent,
                                     delete=False) as stream:
        json.dump(data, stream, ensure_ascii=False, indent=2, allow_nan=False)
        temporary = stream.name
    os.replace(temporary, path)


def load_dataset():
    for name, expected in FILES.items():
        path = ISSUE_DIR / "upstream" / name
        if not path.is_file() or digest(path) != expected:
            raise ValueError("Pinned upstream missing or modified: " + name)
    entries = json.loads((ISSUE_DIR / "upstream" / DATA_PATH).read_text())
    if (len(entries) != 628 or len({str(e["index"]) for e in entries}) != 628
            or Counter(e["source"] for e in entries) != {"v1": 200, "v2-add": 428}):
        raise ValueError("Unexpected v2 dataset membership")
    spec = importlib.util.spec_from_file_location("issue205_pinned_utils", ISSUE_DIR / "upstream/utils.py")
    utils = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(utils)
    return entries, utils


def prepare(source):
    """Copy an already downloaded upstream; validate all bytes before writing."""
    content = {}
    for name, expected in FILES.items():
        value = (source / name).read_bytes()
        if hashlib.sha256(value).hexdigest() != expected:
            raise ValueError("Source differs from pinned revision: " + name)
        content[name] = value
    for name, value in content.items():
        destination = ISSUE_DIR / "upstream" / name
        if destination.exists() and destination.read_bytes() != value:
            raise ValueError("Refusing to overwrite different upstream data")
    for name, value in content.items():
        destination = ISSUE_DIR / "upstream" / name
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(value)
    save(ISSUE_DIR / "dataset.json", {**IDENTITY, "files_sha256": FILES,
         "sources": {"v1": 200, "v2-add": 428}, "licenses": {
             "Wikipedia/JWTD": "CC-BY-SA-3.0", "original-examples-and-code": "CC0-1.0"}})


def cost(usage, prices):
    if not isinstance(usage, dict):
        return None
    p, c = usage.get("prompt_tokens"), usage.get("completion_tokens")
    cached = (usage.get("prompt_tokens_details") or {}).get("cached_tokens", 0)
    if not all(type(n) is int and n >= 0 for n in (p, c, cached)) or cached > p or p > 200000:
        return None
    return ((p - cached) * prices[0] + cached * prices[1] + c * prices[2]) / 1e6


def plan(limit=628):
    calls = 3 * (limit + 2)
    return {"dataset": IDENTITY, "calls_per_model": calls,
            "models": {model: {"calls": calls, "fixed_500_input_200_output_usd":
                         calls * (500 * values[2][0] + 200 * values[2][2]) / 1e6,
                         "alias_only": model == "gemini-3.7-flash"}
                       for model, values in MODELS.items()}, "not_run": SKIPPED,
            "note": "Reference only; not a spending cap. Thinking and actual usage vary."}


def run(model, limit, output, resume_from=None):
    entries, _ = load_dataset()
    if not 1 <= limit <= len(entries):
        raise ValueError("limit must be 1..628")
    if output.suffix != ".json" or not output.resolve().is_relative_to((ISSUE_DIR / "results").resolve()):
        raise ValueError("Output must be a JSON inside issue205/results (not legacy folders)")
    if output.exists():
        raise ValueError("Output exists; no implicit overwrite or rerun")
    today = datetime.now(timezone.utc).date().isoformat()
    expiry = "2026-11-30" if model == "gemini-4-argon" else "2026-12-31"
    if today > expiry:
        raise ValueError("Reference prices expired; recheck before running")
    timeout = float(os.getenv("SUMIBI_AI_TIMEOUT", "120"))
    if not math.isfinite(timeout) or timeout <= 0:
        raise ValueError("Timeout must be positive and finite")
    from sumibi_typical_convert_client import SumibiTypicalConvertClient
    from katakana_to_romaji_converter import KatakanaToRomajiConverter
    from katakana_to_hiragana_converter import KatakanaToHiraganaConverter
    reasoning, temperature, prices = MODELS[model]
    converters = {"romaji_direct_input": KatakanaToRomajiConverter().convert,
                  "hiragana_input": KatakanaToHiraganaConverter().convert,
                  "katakana_input": lambda x: x}
    jobs = schedule(entries, limit, converters)
    base_url = base_url_for_model(model)
    audit = {"schema": "sumibi-jwtd-v2-run-1", "dataset": IDENTITY,
             "scoring": SCORING, "model": model, "limit": limit,
             "alias_only": model == "gemini-3.7-flash", "alias_target":
             "gemini-3.8-flash" if model == "gemini-3.7-flash" else None,
             "reasoning_effort": reasoning, "temperature": temperature,
             "timeout_seconds": timeout, "sdk_retries": 0,
             "endpoint": base_url + "chat/completions", "prices": prices,
             "pricing_valid_through": expiry, "started_at": datetime.now(timezone.utc).isoformat(),
             "source_sha256": {name: digest(ROOT / name) for name in
                  ("jwtd_v2.py", "sumibi_typical_convert_client.py", "katakana_to_romaji_converter.py",
                   "katakana_to_hiragana_converter.py")}, "complete": False, "rows": []}
    if resume_from is not None:
        audit = resume_audit(resume_from, model, limit, jobs, timeout)
    if model.startswith("gpt-"):
        key = os.getenv("OPENAI_API_KEY", "").strip() or os.getenv("SUMIBI_AI_API_KEY", "").strip()
        if not key:
            raise ValueError("Set OPENAI_API_KEY (or SUMIBI_AI_API_KEY) in this shell first")
    else:
        key = os.getenv("SUMIBI_AI_API_KEY", "").strip()
        if not key:
            raise ValueError("Set SUMIBI_AI_API_KEY in this shell first")
    client_kwargs = dict(api_key=key, base_url=base_url, model=model,
                         reasoning_effort=reasoning, temperature=temperature, max_retries=0, timeout=timeout)
    if model == "gpt-6.1-terra":
        client_kwargs["verbosity"] = "low"
        client_kwargs["service_tier"] = "default"
    elif model == "gpt-6.1-sol":
        client_kwargs["verbosity"] = "low"
    client = SumibiTypicalConvertClient(**client_kwargs)
    # Reserve the filename exclusively, including against concurrent invocations.
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("x", encoding="utf-8") as stream:
        json.dump(audit, stream, ensure_ascii=False, indent=2)
    for entry, row in jobs[len(audit["rows"]):]:
        mode, position, text = row["mode"], row["position"], row["input_text"]
        started = time.perf_counter()
        try:
            # Suppress client diagnostic output; never persist raw exceptions or headers.
            import contextlib
            import io
            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                row["hypothesis"] = client.convert(entry.get("context_text", "") + text, text)
            row.update(client.last_response_metadata)
            reason = row.get("finish_reason") or ""
            row["status"] = "blocked" if row.get("block_reason") or reason.split(":")[0].strip() == "content_filter" else "ok"
            row["reference_usd"] = cost(row.get("usage"), prices)
            usage = row.get("usage") or {}
            counts = [usage.get(k) for k in ("total_tokens", "prompt_tokens", "completion_tokens")]
            row["unitemized_tokens"] = (counts[0] - counts[1] - counts[2]
                if all(type(n) is int and n >= 0 for n in counts) else None)
            if (row.get("empty_choices") or not reason) and row["status"] != "blocked":
                raise ValueError("Missing response choice")
            returned = row.get("model")
            if not returned:
                raise ValueError("Missing returned model")
            if model.startswith("gemini-") and not returned.startswith("gemini-"):
                raise ValueError("Missing or non-Gemini returned model")
            if model.startswith("gpt-") and not returned.startswith("gpt-"):
                raise ValueError("Missing or non-GPT returned model")
            # Known 3.7 alias is permitted, other silent substitutions stop the run.
            accepted = (model, audit["alias_target"]) if audit["alias_only"] else (model,)
            if returned not in accepted:
                raise ValueError("Unexpected returned model")
        except Exception as exc:
            row.update(status="error", error=type(exc).__name__,
                       http_status=getattr(exc, "status_code", None))
        row["elapsed_sec"] = time.perf_counter() - started
        audit["rows"].append(row)
        save(output, audit)
        if row["status"] == "error":
            raise RuntimeError("Stopped; safe error type/status saved. No automatic retry.")
        if position % 20 == 0 or position == limit:
            print(f"{model}: {mode} {position}/{limit}, calls={len(audit['rows'])}, {row['status']}", flush=True)
    audit["complete"] = True
    audit["finished_at"] = datetime.now(timezone.utc).isoformat()
    save(output, audit)
    print("Complete:", output)


def summarize(paths):
    entries, utils = load_dataset()
    result = {"schema": "sumibi-jwtd-v2-summary-1", "dataset": IDENTITY,
              "scoring": SCORING, "models": {}, "alias_checks": {}}
    source_hashes = None
    for path in paths:
        a = json.loads(path.read_text())
        if (not isinstance(a, dict) or a.get("schema") != "sumibi-jwtd-v2-run-1"
                or a.get("dataset") != IDENTITY or a.get("scoring") != SCORING):
            raise ValueError("Foreign dataset/schema/scoring rejected: " + str(path))
        if not a["complete"] or a["limit"] != 628 or a["model"] not in MODELS:
            raise ValueError("Only complete 628-item Gemini runs can enter the report")
        expected_alias = a["model"] == "gemini-3.7-flash"
        if a["alias_only"] != expected_alias or a.get("source_sha256") is None:
            raise ValueError("Missing provenance or incorrect alias classification")
        # Runner changes for checkpoint/resume do not change the prompt/converters.
        # Keep original runner hash and each resume runner hash in the raw audit.
        comparable = {k: a["source_sha256"].get(k) for k in MEASUREMENT_FILES}
        if any(not isinstance(h, str) or len(h) != 64 for h in comparable.values()):
            raise ValueError("Missing measurement/prompt/converter hashes")
        if source_hashes is None:
            source_hashes = comparable
        if source_hashes != comparable:
            raise ValueError("Mixed measurement/prompt/converter versions")
        if len(a["rows"]) != 3 * 630 or any(r["status"] not in ("ok", "blocked") for r in a["rows"]):
            raise ValueError("Invalid/incomplete attempt coverage")
        modes = {}
        for mode in MODES:
            warmups = [r for r in a["rows"] if r["mode"] == mode and r["warmup"]]
            if len(warmups) != 2 or any(r["position"] != 0 for r in warmups):
                raise ValueError("Invalid warmup coverage")
            rows = [r for r in a["rows"] if r["mode"] == mode and not r["warmup"]]
            if ([r["position"] for r in rows] != list(range(1, 629))
                    or [r["index"] for r in rows] != [str(e["index"]) for e in entries]):
                raise ValueError("Mismatched or duplicate case order")
            def scores(selected):
                items = [e for e, r in selected]
                hypotheses = [r["hypothesis"] for e, r in selected]
                raw = utils.evaluate_items(items, hypotheses, n_resamples=1000)
                raw["width_normalized_accuracy@1"] = statistics.mean(
                    utils.calculate_accuracy_at1(e["expected_output"], h, normalize=True)
                    for e, h in zip(items, hypotheses))
                return raw
            times = sorted(r["elapsed_sec"] for r in rows)
            p95 = times[math.ceil(.95 * len(times)) - 1]
            modes[mode] = {"quality": scores(list(zip(entries, rows))),
                "by_source": {source: scores([(e, r) for e, r in zip(entries, rows) if e["source"] == source])
                              for source in ("v1", "v2-add")},
                "by_domain": {domain: scores([(e, r) for e, r in zip(entries, rows) if e.get("domain", "") == domain])
                              for domain in sorted({e.get("domain", "") for e in entries})},
                "latency": {"mean": statistics.mean(times), "median": statistics.median(times),
                            "p95_nearest_rank": p95, "mean_at_or_below_p95": statistics.mean(t for t in times if t <= p95),
                            "max": max(times)}, "blocked": sum(r["status"] == "blocked" for r in rows),
                "empty": sum(not r["hypothesis"] for r in rows)}
        bucket = result["alias_checks"] if a["alias_only"] else result["models"]
        if a["model"] in bucket:
            raise ValueError("Duplicate model runs cannot be silently pooled")
        attempts = a["rows"] + a.get("failed_attempts", [])
        bucket[a["model"]] = {"modes": modes, "returned_models": dict(Counter(r["model"] for r in a["rows"])),
                "known_reference_usd": sum(r.get("reference_usd") or 0 for r in attempts),
                "unknown_cost_calls": sum(r.get("reference_usd") is None for r in attempts),
                "unitemized_token_gap": sum(r.get("unitemized_tokens") or 0 for r in attempts),
                "unknown_token_gap_calls": sum(r.get("unitemized_tokens") is None for r in attempts),
                "failed_attempts": len(a.get("failed_attempts", [])),
                "resume_sessions": len(a.get("resume_history", []))}
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    prepare_parser = sub.add_parser("prepare")
    prepare_parser.add_argument("--source-root", type=Path, required=True)
    plan_parser = sub.add_parser("plan")
    plan_parser.add_argument("--limit", type=int, default=628, choices=range(1, 629), metavar="1..628")
    run_parser = sub.add_parser("run")
    run_parser.add_argument("--model", choices=MODELS, required=True)
    run_parser.add_argument("--limit", type=int, default=628)
    run_parser.add_argument("--output", type=Path, required=True)
    run_parser.add_argument("--resume-from", type=Path,
                            help="Explicitly resume a checkpoint into a NEW output file")
    summary_parser = sub.add_parser("summarize")
    summary_parser.add_argument("results", type=Path, nargs="+")
    summary_parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        if args.command == "prepare":
            prepare(args.source_root)
        elif args.command == "plan":
            print(json.dumps(plan(args.limit), indent=2, ensure_ascii=False))
        elif args.command == "run":
            run(args.model, args.limit, args.output, args.resume_from)
        else:
            if args.output.exists():
                raise ValueError("Summary exists; choose a new output")
            save(args.output, summarize(args.results))
    except (ValueError, RuntimeError) as exc:
        # Do not print arbitrary exception messages (may contain secret input).
        parser.exit(1, f"{type(exc).__name__}: validation/run failed; check settings and safe audit.\n")


if __name__ == "__main__":
    main()
