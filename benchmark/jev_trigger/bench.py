#!/usr/bin/env python3
"""Per-keystroke conversion trigger benchmark for Sumibi issue #187."""

import argparse
import json
import math
import os
from pathlib import Path
import re
import statistics
import sys
import time
from urllib import request


CASES = Path(__file__).with_name("cases.json")
ADDITIONAL_CASES = Path(__file__).with_name("additional_cases.json")
ENGLISH_CASES = Path(__file__).with_name("english_cases.json")
PUNCTUATION_CASES = Path(__file__).with_name("punctuation_cases.json")
LEGACY_CASES = Path(__file__).with_name("legacy_cases.json")
LEGACY_ADDITIONAL_CASES = Path(__file__).with_name("legacy_additional_cases.json")
API_URL = "https://api.typesafe.ai/v1/systemone"
INPUT_USD_PER_MILLION = 0.042  # Vendor-announced reference price; verify at billing.
PARTICLES = ("wa", "ha", "ga", "wo", "ni", "de", "to", "kara", "made", "he", "mo", "no", "ya", "desu")
EXCLUDED_CONTEXTS = {"shell", "gpg"}
QUESTION_V1 = {
    "type": "noul",
    "instructions": (
        "Should a Japanese romaji input method start automatic Japanese conversion now, "
        "at this observation point after the latest key event and any reported pause? "
        "Answer yes only at a clear completed "
        "Japanese phrase boundary. Answer no for partial words, English, code, numbers, "
        "editing, and punctuation that was followed quickly by another key. "
        "A punctuation pause of at least 500 ms can mark a completed phrase."
    ),
}
QUESTION_V2 = {
    "type": "noul",
    "instructions": (
        "At the current observation point, should a Japanese romaji input method "
        "start automatic conversion of the text before the cursor? A space after a "
        "Japanese particle (for example wa, ga, wo, ni, de, ha, no) is a useful "
        "conversion point even if the sentence may continue. A space after a "
        "complete Japanese utterance is also a conversion point. A period, comma, "
        "or question mark is a conversion point only after a pause of at least "
        "500 milliseconds. Judge the text and latest key together; do not predict "
        "future typing."
    ),
    "criteria": {
        "true": "The latest key completed a plausible Japanese romaji phrase at a space, or punctuation was followed by a pause of at least 500 ms.",
        "false": "Still inside a word or phrase, a quick punctuation continuation, English prose, code, a number, or a non-Japanese editing context.",
    },
}
QUESTIONS = {"v1": QUESTION_V1, "v2": QUESTION_V2}


def expand_cases(cases):
    """Validate compact scenarios and yield one labelled observation per key."""
    seen = set()
    for case in cases:
        case_id = case["id"]
        if case_id in seen:
            raise ValueError(f"duplicate id: {case_id}")
        seen.add(case_id)
        keys = case["keys"]
        if not keys:
            raise ValueError(f"empty keys: {case_id}")
        positives = case["trigger_after"]
        if len(positives) != len(set(positives)) or any(
            type(i) is not int or i < 1 or i > len(keys) for i in positives
        ):
            raise ValueError(f"invalid trigger_after: {case_id}")
        acceptable = case.get("acceptable_after", [])
        if len(acceptable) != len(set(acceptable)) or any(
            type(i) is not int or i < 1 or i > len(keys) or i in positives
            for i in acceptable
        ):
            raise ValueError(f"invalid acceptable_after: {case_id}")
        pauses = case.get("pause_after_ms", {})
        if any(not str(i).isdigit() or not 1 <= int(i) <= len(keys)
               or type(ms) is not int or ms < 0 for i, ms in pauses.items()):
            raise ValueError(f"invalid pause_after_ms: {case_id}")
        buffer = ""
        for index, key in enumerate(keys, 1):
            if key == "\b":
                buffer = buffer[:-1]
            else:
                buffer += key
            yield {
                "id": case_id,
                "group": case["group"],
                "context": case["context"],
                "index": index,
                "key": "BACKSPACE" if key == "\b" else key,
                "buffer": buffer,
                "pause_ms": pauses.get(str(index), 0),
                "gold": index in positives,
                "acceptable": index in positives or index in acceptable,
            }


def rule_proxy(row):
    """A deliberately small proxy for ambient's space/particle and punctuation gates."""
    if row["context"] != "plain":
        return 0.0
    text = row["buffer"]
    key = row["key"]
    if key == " " and re.search(r"[A-Za-z]+ $", text):
        word = re.search(r"([A-Za-z]+) $", text).group(1).lower()
        if word.endswith(PARTICLES):
            # A proxy English guard: not a faithful copy of Lisp heuristics.
            if re.search(r"\b(the|this|is|test|data|hello)\b", text.lower()):
                return 0.0
            return 1.0
    if key in ".,?" and row["pause_ms"] >= 500:
        if re.search(r"[A-Za-z][A-Za-z ]*[.,?]$", text):
            if re.search(r"\b(the|this|is|test|data|hello)\b", text.lower()):
                return 0.0
            return 1.0
    return 0.0


def build_request(row, prompt_version="v1"):
    # No future characters or gold labels are included in the model input.
    state = {
        "task": "Japanese romaji input method automatic-conversion trigger",
        "buffer_before_cursor": row["buffer"],
        "latest_key": row["key"],
        "context": row["context"],
        "pause_after_latest_key_ms": row["pause_ms"],
    }
    return {"state": state, "model": "jev-latest", "questions": {"convert_now": QUESTIONS[prompt_version]}}


def jev_score(row, api_key, timeout, prompt_version="v1"):
    payload = json.dumps(build_request(row, prompt_version), ensure_ascii=False).encode("utf-8")
    req = request.Request(
        API_URL,
        data=payload,
        headers={"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"},
        method="POST",
    )
    started = time.monotonic()
    with request.urlopen(req, timeout=timeout) as response:
        result = json.load(response)
    elapsed_ms = (time.monotonic() - started) * 1000
    score = result["answers"]["convert_now"]["noul"]
    tokens = result["usage"]["input_tokens"]
    model = result["model"]
    if type(score) not in (int, float) or not math.isfinite(score) or not 0 <= score <= 1:
        raise ValueError("Jev returned an invalid Noul probability")
    if type(tokens) is not int or tokens < 0:
        raise ValueError("Jev returned invalid input token usage")
    if not isinstance(model, str) or not model:
        raise ValueError("Jev returned an invalid model name")
    return float(score), tokens, elapsed_ms, model


def metrics(rows, threshold):
    tp = sum(r["gold"] and r["score"] >= threshold for r in rows)
    fp = sum(not r["gold"] and r["score"] >= threshold for r in rows)
    fn = sum(r["gold"] and r["score"] < threshold for r in rows)
    tn = len(rows) - tp - fp - fn
    precision = tp / (tp + fp) if tp + fp else 0.0
    recall = tp / (tp + fn) if tp + fn else 0.0
    f1 = 2 * precision * recall / (precision + recall) if precision + recall else 0.0
    return {"n": len(rows), "tp": tp, "fp": fp, "fn": fn, "tn": tn,
            "precision": round(precision, 4), "recall": round(recall, 4), "f1": round(f1, 4)}


def preference_summary(rows, threshold):
    """Keep preferred, merely acceptable, and unwanted trigger outcomes separate."""
    predicted = [r for r in rows if r["score"] >= threshold]
    preferred = [r for r in predicted if r["gold"]]
    acceptable_early = [r for r in predicted if r["acceptable"] and not r["gold"]]
    unwanted = [r for r in predicted if not r["acceptable"]]
    preferred_total = sum(r["gold"] for r in rows)
    return {
        "preferred_detected": len(preferred),
        "preferred_total": preferred_total,
        "preferred_recall": round(len(preferred) / preferred_total, 4) if preferred_total else 0.0,
        "acceptable_but_nonpreferred": len(acceptable_early),
        "unwanted_triggers": len(unwanted),
        "acceptable_precision": round((len(preferred) + len(acceptable_early)) / len(predicted), 4)
        if predicted else 0.0,
    }


def english_safety(rows, threshold):
    """Report English false triggers per key and per sentence."""
    english = [r for r in rows if r["group"] == "english_sentence"]
    case_ids = {r["id"] for r in english}
    unwanted = [r for r in english if r["score"] >= threshold]
    affected = sorted({r["id"] for r in unwanted})
    return {
        "keystrokes": len(english),
        "cases": len(case_ids),
        "false_triggers": len(unwanted),
        "cases_with_trigger": len(affected),
        "false_triggers_per_1000_keystrokes": round(1000 * len(unwanted) / len(english), 2)
        if english else 0.0,
        "affected_cases": affected,
    }


def percentile(values, fraction):
    values = sorted(values)
    return values[math.ceil(fraction * len(values)) - 1] if values else None


def run(args):
    files = {
        "dev": (CASES,),
        "additional": (ADDITIONAL_CASES,),
        "english": (ENGLISH_CASES,),
        "punctuation": (PUNCTUATION_CASES,),
        "all": (CASES, ADDITIONAL_CASES, ENGLISH_CASES, PUNCTUATION_CASES),
        "legacy-dev": (LEGACY_CASES,),
        "legacy-additional": (LEGACY_ADDITIONAL_CASES,),
    }[args.dataset]
    cases = []
    for case_file in files:
        cases.extend(json.loads(case_file.read_text(encoding="utf-8")))
    if getattr(args, "case_id", None):
        cases = [case for case in cases if case["id"] == args.case_id]
        if not cases:
            raise ValueError(f"unknown case id in selected dataset: {args.case_id}")
    all_rows = list(expand_cases(cases))
    if args.start_row > len(all_rows):
        raise ValueError(f"--start-row exceeds dataset length ({len(all_rows)})")
    rows = all_rows[args.start_row - 1:]
    api_key = os.environ.get("TYPESAFE_API_KEY")
    if args.backend == "jev" and not api_key:
        raise ValueError("TYPESAFE_API_KEY is required for --backend jev")
    if args.backend == "jev" and args.max_calls is None:
        raise ValueError("--max-calls is required for --backend jev")
    evaluated = []
    latencies = []
    input_tokens = 0
    calls = 0
    model_versions = set()
    for row in rows:
        if args.backend == "jev":
            if calls >= args.max_calls:
                break
            if row["context"] in EXCLUDED_CONTEXTS:
                score = 0.0  # Never transmit excluded/sensitive buffers.
            else:
                score, tokens, elapsed_ms, model = jev_score(
                    row, api_key, args.timeout, args.prompt_version
                )
                calls += 1
                input_tokens += tokens
                latencies.append(elapsed_ms)
                model_versions.add(model)
        else:
            score = rule_proxy(row)
        evaluated.append({**row, "score": score})
    groups = sorted({r["group"] for r in evaluated})
    positive_scores = {f"{r['id']}@{r['index']}": round(r["score"], 4)
                       for r in evaluated if r["gold"]}
    acceptable_scores = {f"{r['id']}@{r['index']}": round(r["score"], 4)
                         for r in evaluated if r["acceptable"] and not r["gold"]}
    top_negative_scores = [
        {"position": f"{r['id']}@{r['index']}", "score": round(r["score"], 4)}
        for r in sorted((r for r in evaluated if not r["acceptable"]),
                        key=lambda r: r["score"], reverse=True)[:5]
    ]
    result = {
        "backend": args.backend,
        "dataset": args.dataset,
        "case_id": getattr(args, "case_id", None),
        "prompt_version": args.prompt_version if args.backend == "jev" else None,
        "start_row": args.start_row,
        "threshold": args.threshold,
        "complete_dataset": args.start_row == 1 and len(evaluated) == len(all_rows),
        "total_keystrokes": len(all_rows),
        "overall": metrics(evaluated, args.threshold),
        "preference_summary": preference_summary(evaluated, args.threshold),
        "english_safety": english_safety(evaluated, args.threshold),
        "threshold_sweep": {str(t): metrics(evaluated, t)
                            for t in (0.5, 0.6, 0.7, 0.8, 0.9)},
        "positive_scores": positive_scores,
        "acceptable_scores": acceptable_scores,
        "top_negative_scores": top_negative_scores,
        "by_group": {g: metrics([r for r in evaluated if r["group"] == g], args.threshold)
                     for g in groups},
        "acceptable_but_nonpreferred": [f"{r['id']}@{r['index']}" for r in evaluated
                                        if r["acceptable"] and not r["gold"]
                                        and r["score"] >= args.threshold],
        "unwanted_triggers": [f"{r['id']}@{r['index']}" for r in evaluated
                              if not r["acceptable"] and r["score"] >= args.threshold],
        "misses": [f"{r['id']}@{r['index']}" for r in evaluated
                   if r["gold"] and r["score"] < args.threshold],
        "api_calls": calls,
        "model_versions": sorted(model_versions),
        "input_tokens": input_tokens,
        "estimated_usd": round(input_tokens * INPUT_USD_PER_MILLION / 1_000_000, 8),
        "estimated_usd_per_1000_keystrokes": round(
            input_tokens * INPUT_USD_PER_MILLION / 1_000_000 * 1000 / len(evaluated), 6
        ) if evaluated else 0,
        "latency_p50_ms": round(statistics.median(latencies), 1) if latencies else None,
        "latency_p95_ms": round(percentile(latencies, 0.95), 1) if latencies else None,
    }
    if getattr(args, "include_rows", False):
        result["rows"] = evaluated
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--backend", choices=("rule-proxy", "jev"), default="rule-proxy")
    parser.add_argument("--dataset", choices=("dev", "additional", "english", "punctuation", "all",
                                              "legacy-dev", "legacy-additional"),
                        default="dev")
    parser.add_argument("--prompt-version", choices=tuple(QUESTIONS), default="v1")
    parser.add_argument("--case-id", help="evaluate only this case within the selected dataset")
    parser.add_argument("--start-row", type=int, default=1,
                        help="1-based dataset row at which to resume (partial metrics)")
    parser.add_argument("--threshold", type=float, default=0.8)
    parser.add_argument("--max-calls", type=int, help="hard cap on paid Jev requests")
    parser.add_argument("--timeout", type=float, default=10.0)
    parser.add_argument("--include-rows", action="store_true",
                        help="include each keystroke and score for plotting")
    args = parser.parse_args()
    if not 0 <= args.threshold <= 1:
        parser.error("--threshold must be between 0 and 1")
    if args.max_calls is not None and args.max_calls < 1:
        parser.error("--max-calls must be positive")
    if args.start_row < 1:
        parser.error("--start-row must be positive")
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    try:
        result = run(args)
    except (ValueError, OSError, KeyError) as exc:
        parser.exit(1, f"benchmark error: {exc}\n")
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
