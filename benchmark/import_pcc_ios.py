#!/usr/bin/env python3
"""Validate and import iOS PCC AJIMEE v1 exports into Sumibi's result schema.

The exported text and references derive from AJIMEE-Bench v1 (CC BY-SA 3.0).
The 200 v1 cases are also retained, with ``source=v1``, in the tracked v2
dataset. We use that copy only to verify identity, not to score against v2.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from katakana_to_hiragana_converter import KatakanaToHiraganaConverter
from katakana_to_romaji_converter import KatakanaToRomajiConverter


INPUT_TYPES = ("romaji", "hiragana", "katakana")
SOURCE = Path(__file__).parent / "JWTD_v2/upstream/JWTD_v2/v2/evaluation_items.json"


def edit_distance(left: str, right: str) -> int:
    previous = list(range(len(right) + 1))
    for row, character in enumerate(left, 1):
        current = [row]
        for column, other in enumerate(right, 1):
            current.append(min(current[-1] + 1, previous[column] + 1,
                               previous[column - 1] + (character != other)))
        previous = current
    return previous[-1]


def prepare_cases(source: Path = SOURCE) -> dict[str, dict]:
    dataset = json.loads(source.read_text(encoding="utf-8"))
    v1 = [item for item in dataset if item.get("source") == "v1"]
    if len(v1) != 200 or len({item["index"] for item in v1}) != 200:
        raise ValueError("Expected exactly 200 distinct AJIMEE v1 cases")
    romaji = KatakanaToRomajiConverter()
    hiragana = KatakanaToHiraganaConverter()
    return {
        item["index"]: {
            "context": item["context_text"],
            "expected": item["expected_output"],
            "romaji": romaji.convert(item["input"]),
            "hiragana": hiragana.convert(item["input"]),
            "katakana": item["input"],
        }
        for item in v1
    }


def normalize(rows: list[dict], input_type: str, cases: dict[str, dict]) -> list[dict]:
    if input_type not in INPUT_TYPES:
        raise ValueError(f"Unknown input type: {input_type}")
    if len(rows) != 200:
        raise ValueError(f"Expected 200 PCC results, got {len(rows)}")
    seen = set()
    normalized = []
    for row in rows:
        identifier = str(row["id"])
        if identifier in seen or identifier not in cases:
            raise ValueError(f"Duplicate or unknown case ID: {identifier}")
        seen.add(identifier)
        expected = cases[identifier]["expected"]
        if row["context"] != cases[identifier]["context"] or row["expected"] != expected:
            raise ValueError(f"Context or accepted answers differ: {identifier}")
        if row["romaji"] != cases[identifier]["romaji"]:
            raise ValueError(f"Romaji input differs: {identifier}")
        input_text = row.get("inputText", row["romaji"])
        if input_text != cases[identifier][input_type]:
            raise ValueError(f"{input_type} input differs: {identifier}")
        answer = row["result"]
        if not isinstance(answer, str) or not expected or any(not x for x in expected):
            raise ValueError(f"Invalid answer or reference: {identifier}")
        elapsed = row["elapsedSec"]
        if not isinstance(elapsed, (int, float)) or elapsed < 0:
            raise ValueError(f"Invalid elapsed time: {identifier}")
        error = row.get("error")
        if error is not None and answer:
            raise ValueError(f"Error has a nonempty answer: {identifier}")
        normalized.append({
            "id": identifier,
            "surrounding_text": row["context"] + input_text,
            "henkan_text": input_text,
            "expect": expected,
            "result": answer,
            "cer": min(1.0, min(edit_distance(ref, answer) / len(ref) for ref in expected)),
            "at1": int(answer in expected),
            "elapsed_sec": elapsed,
            "error": error,
        })
    if seen != cases.keys():
        raise ValueError("The PCC results do not cover all AJIMEE v1 IDs")
    return normalized


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("romaji", type=Path)
    parser.add_argument("hiragana", type=Path)
    parser.add_argument("katakana", type=Path)
    parser.add_argument("--output-dir", type=Path,
                        default=Path(__file__).parent / "result_pcc_ios")
    args = parser.parse_args()
    cases = prepare_cases()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for input_type in INPUT_TYPES:
        source = getattr(args, input_type)
        rows = json.loads(source.read_text(encoding="utf-8"))
        result = normalize(rows, input_type, cases)
        suffix = "" if input_type == "romaji" else f"_{input_type}"
        destination = args.output_dir / f"apple-pcc-ios17e{suffix}.json"
        destination.write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n",
                               encoding="utf-8")
        print(f"{destination}: {len(result)} verified v1 cases")


if __name__ == "__main__":
    main()
