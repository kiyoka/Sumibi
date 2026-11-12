#!/usr/bin/env python3
"""Issue #209: measure PLaMo ja→ja against the legacy 200-item dataset."""
import argparse
from datetime import datetime, timezone
from hashlib import sha256
from http.client import HTTPConnection
import json
import math
from pathlib import Path
import statistics
import time

from Levenshtein import distance
from katakana_to_hiragana_converter import KatakanaToHiraganaConverter

DATASET = Path(__file__).resolve().parent / "AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json"
DATASET_HASH = "e9eb668fd6aa14b1e26436f429b5550108af0a1dfd443b8cea0bcb3ab3028fca"
SYSTEM = "[plamo-translate]\nsource_lang=ja\ntarget_lang=ja"
STOP = ["<|plamo:tag|>", "<|plamo:bos|>"]
MAX_TOKENS = 256


def messages(text, context):
    system = SYSTEM + ("\n---\n" + context if context else "")
    return [{"role": "system", "content": system}, {"role": "user", "content": text}]


def input_for_mode(text, mode):
    if mode == "hiragana_input":
        return KatakanaToHiraganaConverter().convert(text)
    if mode == "katakana_input":
        return text
    raise ValueError("Unsupported input mode")


def completion(connection, model, text, context):
    payload = {"model": model, "messages": messages(text, context), "temperature": 0,
               "max_tokens": MAX_TOKENS, "stop": STOP, "stream": False}
    start = time.perf_counter()
    connection.request("POST", "/v1/chat/completions", json.dumps(payload).encode(),
                       {"Content-Type": "application/json"})
    response = connection.getresponse()
    body = response.read()
    elapsed = time.perf_counter() - start
    if response.status != 200:
        raise RuntimeError(f"LM Studio HTTP {response.status}")
    result = json.loads(body)
    if result.get("model") != model:
        raise RuntimeError("Unexpected response model")
    choice = result["choices"][0]
    return {"output": choice["message"]["content"] or "", "elapsed_seconds": elapsed,
            "finish_reason": choice.get("finish_reason"), "usage": result.get("usage")}


def evaluate(row, expected):
    text = row["output"].strip()
    return {**row, "expected": expected, "exact": text in expected,
            "min_cer": min(distance(ref, text) / len(ref) for ref in expected)}


def summarize(rows):
    times = sorted(r["elapsed_seconds"] for r in rows)
    return {"count": len(rows), "mean_min_cer": statistics.mean(r["min_cer"] for r in rows),
            "exact_matches": sum(r["exact"] for r in rows),
            "mean_seconds": statistics.mean(times), "median_seconds": statistics.median(times),
            "p95_seconds": times[math.ceil(len(times) * 0.95) - 1], "max_seconds": times[-1],
            "empty_outputs": sum(not r["output"].strip() for r in rows),
            "finish_reasons": {reason: sum(r["finish_reason"] == reason for r in rows)
                               for reason in {r["finish_reason"] for r in rows}}}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model", default="plamo-3-translate-0.8b")
    parser.add_argument("--port", type=int, default=1234)
    parser.add_argument("--mode", choices=("hiragana_input", "katakana_input"),
                        default="hiragana_input")
    parser.add_argument("--limit", type=int, choices=range(1, 201), default=200, metavar="1..200")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    data = DATASET.read_bytes()
    if sha256(data).hexdigest() != DATASET_HASH:
        parser.error("Expected the pinned JWTD_v2/v1 200-item dataset")
    entries = json.loads(data)[:args.limit]
    # Loopback only; no credentials, proxies, redirects, or retries.
    connection = HTTPConnection("127.0.0.1", args.port, timeout=60)
    rows = []
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x", encoding="utf-8") as stream:
        def record(kind, item):
            print(json.dumps({"kind": kind, **item}, ensure_ascii=False), file=stream, flush=True)

        record("metadata", {"dataset": "JWTD_v2/v1", "sha256": DATASET_HASH,
                            "model": args.model, "limit": args.limit, "port": args.port,
                            "mode": args.mode, "system": SYSTEM, "stop": STOP,
                            "max_tokens": MAX_TOKENS, "temperature": 0,
                            "started_at": datetime.now(timezone.utc).isoformat()})
        try:
            for position, entry in enumerate([entries[0]] * 2 + entries, start=-1):
                text = input_for_mode(entry["input"], args.mode)
                context = entry.get("context_text", "") or ""
                try:
                    row = completion(connection, args.model, text, context)
                except (OSError, RuntimeError, ValueError, KeyError, IndexError) as exc:
                    record("error", {"position": position, "error": type(exc).__name__})
                    raise SystemExit(str(exc) if isinstance(exc, RuntimeError) else type(exc).__name__)
                row.update(position=position, index=entry["index"], input=text, context=context)
                if position <= 0:
                    record("warmup", row)
                else:
                    row = evaluate(row, entry["expected_output"])
                    rows.append(row)
                    record("result", row)
                    if position % 20 == 0 or position == args.limit:
                        print(f"{position}/{args.limit}", flush=True)
            summary = summarize(rows)
            record("summary", summary)
            print(json.dumps(summary, ensure_ascii=False))
        finally:
            connection.close()


if __name__ == "__main__":
    main()
