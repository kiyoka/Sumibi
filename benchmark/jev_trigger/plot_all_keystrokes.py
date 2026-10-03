#!/usr/bin/env python3
"""Generate one PNG/SVG per case and a Markdown gallery without API calls."""
import argparse
import json
from pathlib import Path

from bench import CASES, ADDITIONAL_CASES, ENGLISH_CASES, PUNCTUATION_CASES, EXCLUDED_CONTEXTS, expand_cases


def validate_result(data, cases):
    if data.get("backend") != "jev" or data.get("dataset") != "all" or not data.get("complete_dataset"):
        raise ValueError("--dataset all --include-rowsによる全件測定結果が必要です。")
    expected = list(expand_cases(cases))
    rows = data.get("rows", [])
    if len(rows) != len(expected):
        raise ValueError("打鍵データが不足しています。--include-rowsで測定してください。")
    for actual, reference in zip(rows, expected):
        if any(actual.get(k) != v for k, v in reference.items()):
            raise ValueError("測定結果と現在のデータセットが一致しません。")
        score = actual.get("score")
        if type(score) not in (int, float) or not 0 <= score <= 1:
            raise ValueError("評価値が不正です。")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("result", type=Path)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    data = json.loads(args.result.read_text(encoding="utf-8"))
    cases = [case for path in (CASES, ADDITIONAL_CASES, ENGLISH_CASES, PUNCTUATION_CASES)
             for case in json.loads(path.read_text(encoding="utf-8"))]
    try:
        validate_result(data, cases)
    except ValueError as exc:
        parser.error(str(exc))
    from plot_keystrokes import main as plot_one
    output_dir = args.output_dir or args.result.parent / "keystroke_graphs"
    output_dir.mkdir(parents=True, exist_ok=True)
    report = ["# 全例文の打鍵グラフ", "",
              f"{len(cases)}ケース。1ケースにつき1枚（長文は段分け）。閾値は0.8と0.9。", "",
              "各時点を独立評価し、途中で変換せず入力を継続した状態です。shell/gpgの2例はAPI未送信で、0はJevの実測値ではありません。", "",
              "[結果の解釈と暫定判断](../VISUAL_REPORT.md) | [実行手順](../README.md)", ""]
    for case in cases:
        case_id = case["id"]
        plot_one([str(args.result), "--case-id", case_id, "--output-dir", str(output_dir)])
        report.extend([f"## {case_id}", "", f"入力：`{case['keys'].replace(' ', '␣')}`", ""])
        if "japanese" in case:
            report.extend([f"意図する日本語：{case['japanese']}", ""])
        if case["context"] in EXCLUDED_CONTEXTS:
            report.extend(["API未測定：ローカル除外。", ""])
        report.extend([f"![{case_id}の打鍵評価値]({case_id}_keystrokes.png)", "",
                       f"[SVG版]({case_id}_keystrokes.svg)", ""])
    index = output_dir / "INDEX.md"
    index.write_text("\n".join(report), encoding="utf-8")
    print(f"グラフ一覧: {index}")


if __name__ == "__main__":
    main()
