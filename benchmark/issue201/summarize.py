#!/usr/bin/env python3
"""Summarize Issue #201 saved measurements only; never call an API."""
from collections import Counter
import json
from pathlib import Path
import statistics
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from aggregate_results import summarize
from run_gemini_3_8_flash import response_status, unitemized_tokens

MODELS = ("gemini-3.5-flash-lite", "gemini-3.8-flash", "gemini-3.6-flash", "gemini-3.1-flash-lite-preview")
MODES = {"romaji": "", "hiragana": "_hiragana", "katakana": "_katakana"}


def main():
    base = ROOT / "issue201"
    audit = json.loads((base / f"full/{MODELS[0]}_run.json").read_text())
    attempts = audit["attempts"]
    if not audit["complete"] or audit["limit_per_mode"] != 200 or len(attempts) != 606:
        raise ValueError("Full 606-call measurement is not complete")
    if any(a["status"] == "error" for a in attempts):
        raise ValueError("Unresolved request errors")
    for mode in ("romaji_direct_input", "hiragana_input", "katakana_input"):
        selected = [a for a in attempts if a["mode"] == mode]
        if sum(a["warmup"] for a in selected) != 2:
            raise ValueError("Warmup count mismatch")
        if [a["index"] for a in selected if not a["warmup"]] != list(range(1, 201)):
            raise ValueError("Missing/duplicate/out-of-order measured indices")
    gaps = [unitemized_tokens(a.get("usage")) for a in attempts]
    measurement = {
        "started_at": audit["started_at"], "finished_at": audit["finished_at"],
        "calls": len(attempts), "request_errors": 0,
        "blocked_calls": sum(response_status(a) == "blocked" for a in attempts),
        "finish_reasons": dict(Counter(a["finish_reason"] for a in attempts)),
        "response_models": sorted({a["model"] for a in attempts}),
        "input_tokens": sum(a["usage"]["prompt_tokens"] for a in attempts),
        "reported_completion_tokens": sum(a["usage"]["completion_tokens"] for a in attempts),
        "reported_total_tokens": sum(a["usage"]["total_tokens"] for a in attempts),
        "unitemized_token_gap": sum(g for g in gaps if g is not None),
        "token_gap_unknown_calls": sum(g is None for g in gaps),
        "reasoning_token_details": "not_reported" if all(
            (a["usage"].get("completion_tokens_details") or {}).get("reasoning_tokens") is None
            for a in attempts) else "see_audit",
        "reference_usd_including_warmup": audit["reference_usd_known_usage_only"],
        "reference_usd_excluding_warmup": sum(a["reference_usd"] for a in attempts if not a["warmup"]),
    }
    result = {"measurement": measurement, "models": {}}
    for model in MODELS:
        result["models"][model] = {}
        for mode, suffix in MODES.items():
            reference = json.loads((base / f"full/{MODELS[0]}{suffix}.json").read_text())
            path = ROOT / f"result_ver2.4.0/{model}{suffix}.json"
            rows = json.loads(path.read_text())
            if len(rows) != 200 or len(reference) != 200:
                raise ValueError("Incomplete results")
            if [(r["expect"], r["henkan_text"]) for r in rows] != [(r["expect"], r["henkan_text"]) for r in reference]:
                raise ValueError("Different evaluation examples")
            cer, at1, minimum, maximum, median, p95, trimmed = summarize(rows)
            result["models"][model][mode] = {
                "n": len(rows), "cer": cer, "at1": at1,
                "mean_all_seconds": statistics.mean(r["elapsed_sec"] for r in rows),
                "mean_below_p95_seconds": trimmed, "median_seconds": median, "p95_seconds": p95,
                "min_seconds": minimum, "max_seconds": maximum,
                "empty_results": sum(not r["result"] for r in rows),
            }
    smoke = json.loads((base / f"smoke/{MODELS[0]}_run.json").read_text())
    measurement["smoke_calls"] = len(smoke["attempts"])
    measurement["smoke_reference_usd"] = smoke["reference_usd_known_usage_only"]
    measurement["combined_reference_usd"] = measurement["reference_usd_including_warmup"] + measurement["smoke_reference_usd"]
    (base / "summary.json").write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
