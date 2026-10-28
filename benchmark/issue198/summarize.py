#!/usr/bin/env python3
"""Generate the Issue #198 comparison from completed saved measurements only."""
import json
from pathlib import Path
import statistics
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from aggregate_results import summarize

MODELS = ("gpt-6.1-terra", "gpt-6.1-sol", "gpt-6-sol", "gpt-5.6-terra")
MODES = {"romaji": "", "hiragana": "_hiragana", "katakana": "_katakana"}


def main():
    audit = json.loads((ROOT / "issue198/full/gpt-6.1-terra_run.json").read_text())
    if not audit["complete"] or audit["limit_per_mode"] != 200 or len(audit["attempts"]) != 606:
        raise ValueError("Full 606-call measurement is not complete")
    result = {"models": {}, "measurement": {
        "started_at": audit["started_at"], "finished_at": audit["finished_at"],
        "calls": len(audit["attempts"]),
        "errors": sum(a["status"] != "ok" for a in audit["attempts"]),
        "finish_reasons": sorted({a["finish_reason"] for a in audit["attempts"]}),
        "response_models": sorted({a["model"] for a in audit["attempts"]}),
        "input_tokens": sum(a["usage"]["prompt_tokens"] for a in audit["attempts"]),
        "cached_input_tokens": sum((a["usage"].get("prompt_tokens_details") or {}).get("cached_tokens", 0)
                                    for a in audit["attempts"]),
        "output_tokens": sum(a["usage"]["completion_tokens"] for a in audit["attempts"]),
        "reasoning_tokens": sum((a["usage"].get("completion_tokens_details") or {}).get("reasoning_tokens", 0)
                                for a in audit["attempts"]),
        "estimated_usd_including_warmup": audit["estimated_usd"],
        "estimated_usd_excluding_warmup": sum(a["estimated_usd"] for a in audit["attempts"] if not a["warmup"]),
    }}
    for model in MODELS:
        result["models"][model] = {}
        for mode, suffix in MODES.items():
            path = ROOT / "result_ver2.4.0" / f"{model}{suffix}.json"
            rows = json.loads(path.read_text(encoding="utf-8"))
            if len(rows) != 200:
                raise ValueError(f"Incomplete results: {path}")
            cer, at1, _, _, median, p95, truncated_mean = summarize(rows)
            result["models"][model][mode] = {
                "n": len(rows), "cer": cer, "at1": at1,
                "mean_all_seconds": statistics.mean(r["elapsed_sec"] for r in rows),
                "mean_below_p95_seconds": truncated_mean,
                "median_seconds": median, "p95_seconds": p95,
                "empty_results": sum(not r["result"] for r in rows),
            }
            if model != "gpt-6.1-terra":
                reference = json.loads((ROOT / "result_ver2.4.0" / f"gpt-6.1-terra{suffix}.json").read_text())
                if [r["expect"] for r in reference] != [r["expect"] for r in rows]:
                    raise ValueError(f"Different evaluation examples: {path}")
    smoke = json.loads((ROOT / "issue198/smoke/gpt-6.1-terra_run.json").read_text())
    result["measurement"]["smoke_calls"] = len(smoke["attempts"])
    result["measurement"]["smoke_estimated_usd"] = smoke["estimated_usd"]
    result["measurement"]["all_estimated_usd"] = audit["estimated_usd"] + smoke["estimated_usd"]
    output = ROOT / "issue198/summary.json"
    output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
