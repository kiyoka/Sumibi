#!/usr/bin/env python3
"""Recompute Issue #197 statistics without API calls or rewriting raw audits."""
from collections import Counter
import json
from pathlib import Path
import statistics
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from aggregate_results import summarize
from run_gemini_3_8_flash import response_status

MODELS = ("gemini-3.8-flash", "gemini-3.7-flash", "gemini-3.6-flash")
MODES = {"romaji": "", "hiragana": "_hiragana", "katakana": "_katakana"}


def main():
    audit = json.loads((ROOT / "issue197/full/gemini-3.8-flash_run.json").read_text())
    attempts = audit["attempts"]
    if not audit["complete"] or audit["limit_per_mode"] != 200 or len(attempts) != 606:
        raise ValueError("Full 606-call measurement is not complete")
    if any(a["status"] == "error" for a in attempts):
        raise ValueError("Unresolved request errors")
    for mode in ("romaji_direct_input", "hiragana_input", "katakana_input"):
        measured = [a for a in attempts if a["mode"] == mode and not a["warmup"]]
        if [a["index"] for a in measured] != list(range(1, 201)):
            raise ValueError("Missing/duplicate/out-of-order measured indices")
    gaps = [a["usage"]["total_tokens"] - a["usage"]["prompt_tokens"]
            - a["usage"]["completion_tokens"] for a in attempts]
    measurement = {
        "started_at": audit["started_at"], "finished_at": audit["finished_at"],
        "calls": len(attempts), "request_errors": 0,
        "blocked_calls_recomputed": sum(response_status(a) == "blocked" for a in attempts),
        "blocked_calls_original_runner": audit["blocked_calls"],
        "blocked_positions": [{"mode": a["mode"], "index": a["index"], "reason": a["finish_reason"]}
                              for a in attempts if response_status(a) == "blocked"],
        "finish_reasons": dict(Counter(a["finish_reason"] for a in attempts)),
        "response_models": sorted({a["model"] for a in attempts}),
        "input_tokens": sum(a["usage"]["prompt_tokens"] for a in attempts),
        "reported_completion_tokens": sum(a["usage"]["completion_tokens"] for a in attempts),
        "reported_total_tokens": sum(a["usage"]["total_tokens"] for a in attempts),
        "unitemized_token_gap": sum(gaps), "calls_with_unitemized_gap": sum(g != 0 for g in gaps),
        "reasoning_token_details": "not_reported",
        "reference_usd_reported_usage": audit["reference_usd_known_usage_only"],
        "reference_usd_if_gap_is_billed_output": audit["reference_usd_known_usage_only"] + sum(gaps) * 3.75 / 1_000_000,
    }
    result = {"measurement": measurement, "models": {}}
    for model in MODELS:
        result["models"][model] = {}
        for mode, suffix in MODES.items():
            directory = ROOT / ("issue197/full" if model == MODELS[0] else "result_ver2.4.0")
            rows = json.loads((directory / f"{model}{suffix}.json").read_text())
            if len(rows) != 200:
                raise ValueError("Incomplete saved results")
            reference = json.loads((ROOT / f"issue197/full/{MODELS[0]}{suffix}.json").read_text())
            if [(r["expect"], r["henkan_text"]) for r in rows] != [(r["expect"], r["henkan_text"]) for r in reference]:
                raise ValueError("Different evaluation examples")
            cer, at1, _, _, median, p95, trimmed = summarize(rows)
            result["models"][model][mode] = {
                "n": len(rows), "cer": cer, "at1": at1,
                "mean_all_seconds": statistics.mean(r["elapsed_sec"] for r in rows),
                "mean_below_p95_seconds": trimmed, "median_seconds": median, "p95_seconds": p95,
                "empty_results": sum(not r["result"] for r in rows),
            }
    smoke = json.loads((ROOT / "issue197/smoke/gemini-3.8-flash_run.json").read_text())
    measurement["smoke_calls"] = len(smoke["attempts"])
    measurement["smoke_reference_usd"] = smoke["reference_usd_known_usage_only"]
    measurement["combined_reference_usd_reported_usage"] = (measurement["reference_usd_reported_usage"]
                                                           + measurement["smoke_reference_usd"])
    path = ROOT / "issue197/summary.json"
    path.write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
