#!/usr/bin/env python3
"""Generate a reproducible AJIMEE v1 all-model comparison for iOS PCC."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from aggregate_results import summarize
from import_pcc_ios import prepare_cases


HERE = Path(__file__).parent
PCC_NAME = "apple-pcc-ios17e"
INPUT_TYPES = ("romaji", "hiragana", "katakana")
LABELS = {"romaji": "ローマ字", "hiragana": "ひらがな", "katakana": "カタカナ"}


def input_type_for(path: Path) -> str:
    if path.stem.endswith("_hiragana"):
        return "hiragana"
    if path.stem.endswith("_katakana"):
        return "katakana"
    return "romaji"


def base_name(path: Path) -> str:
    for suffix in ("_hiragana", "_katakana"):
        if path.stem.endswith(suffix):
            return path.stem.removesuffix(suffix)
    return path.stem


def load_comparison() -> dict[str, list[dict]]:
    prepared = prepare_cases()
    cases = list(prepared.values())
    tables = {kind: [] for kind in INPUT_TYPES}
    for directory, version in (("result_ver2.4.0", "v2.4.0"),
                               ("result_ver2.3.0", "v2.3.0"),
                               ("result_pcc_ios", "iOS PCC")):
        for path in sorted((HERE / directory).glob("*.json")):
            # Where both prompt versions exist, prefer the newer one. Older-only
            # measurements are still listed but not used in the main rank.
            if version == "v2.3.0" and (HERE / "result_ver2.4.0" / path.name).exists():
                continue
            rows = json.loads(path.read_text(encoding="utf-8"))
            if len(rows) != 200:
                raise ValueError(f"Expected 200 v1 results in {path}, got {len(rows)}")
            for index, row in enumerate(rows):
                case = prepared.get(row.get("id")) if version == "iOS PCC" else cases[index]
                if case is None:
                    raise ValueError(f"Unknown PCC case in {path}, row {index}")
                if row.get("expect") != case["expected"]:
                    raise ValueError(f"Reference/order mismatch in {path}, row {index}")
                if not row.get("surrounding_text", "").startswith(case["context"]):
                    raise ValueError(f"Context/order mismatch in {path}, row {index}")
            cer, exact, _, _, median, _, p95_mean = summarize(rows)
            errors = sum(row.get("error") is not None for row in rows)
            tables[input_type_for(path)].append({
                "model": base_name(path), "version": version,
                "cer": cer, "exact": exact, "median": median,
                "p95_mean": p95_mean,
                "errors": errors if version == "iOS PCC" else None,
            })
    for entries in tables.values():
        entries.sort(key=lambda x: (x["cer"], x["model"]))
    return tables


def render(tables: dict[str, list[dict]]) -> str:
    lines = [
        "# iOS PCCと既存全モデルの比較（AJIMEE-Bench v1・200問）", "",
        "この表は、各入力形式で保存済みの全モデル・設定の実測値を集計したものです。"
        "同じ200問・同じ許容解について文字誤り率（CER、各問を100%で上限処理）と"
        "Top-1完全一致率を比較します。応答時間は既存の `aggregate_results.py` と同様に"
        "95パーセンタイル以下の平均です。欠測の入力形式は表に現れません。", "",
        "**比較上の注意:** PCCはiPhone 17e / iOS 27.0.1のFoundation Models API、"
        "既存モデルは別の端末・API・プロンプト構造で測定されています。"
        "PCCには2回のウォームアップもありません。従って以下の順位は**参考上の位置**であり、"
        "同一環境での厳密な優劣ではありません。`v2.3.0` はプロンプト版が異なるため主順位から除外します。"
        "新版628問（JWTD_v2/v2）のスコアは混在させていません。", "",
        "AppleのAIは無料枠に日次上限があります。外部APIのドル建て課金額と同列の"
        "『無料コスト』としてプロットしません。PCCの安全制限・回答拒否は空回答の不正解として"
        "200問の分母に含めています。他モデルの拒否件数は保存形式が一律でないため"
        "この表では一律に比較しません。", "",
        "## PCCの位置（同じ入力形式のv2.4.0計測との参考比較）", "",
        "| 入力形式 | PCC CER | 完全一致 | 応答時間（p95以下平均） | 安全制限・回答拒否 | CER順位 | 応答時間順位 |",
        "|---|---:|---:|---:|---:|---:|---:|",
    ]
    for kind in INPUT_TYPES:
        entries = [e for e in tables[kind] if e["version"] != "v2.3.0"]
        pcc = next(e for e in entries if e["model"] == PCC_NAME)
        cer_rank = 1 + sum(e["cer"] < pcc["cer"] for e in entries)
        time_rank = 1 + sum(e["p95_mean"] < pcc["p95_mean"] for e in entries)
        lines.append(
            f'| {LABELS[kind]} | {pcc["cer"]:.2%} | {pcc["exact"]:.1%} '
            f'| {pcc["p95_mean"]:.2f}秒 | {pcc["errors"]}/200 '
            f'| {cer_rank}/{len(entries)} | {time_rank}/{len(entries)} |'
        )
    for kind in INPUT_TYPES:
        entries = tables[kind]
        lines += ["", f"## {LABELS[kind]}入力: 全モデル・設定", "",
                  "| モデル・設定 | プロンプト版 | CER | 完全一致 | 応答時間（p95以下平均） |",
                  "|---|---|---:|---:|---:|"]
        for entry in entries:
            model = f'**`{entry["model"]}`**' if entry["version"] == "iOS PCC" else f'`{entry["model"]}`'
            lines.append(f'| {model} | {entry["version"]} | {entry["cer"]:.2%} '
                         f'| {entry["exact"]:.1%} | {entry["p95_mean"]:.2f}秒 |')
    lines += ["", "## 補足: Gemini・ローカルAFM", "",
              "- PCCのひらがな入力はCER 28.95%、46/200完全一致。"
              "Gemini 3.6 Flashの2.73%・149/200より大きく劣ります。"
              "ただしPCCの応答時間は0.76秒で、同モデルの約1.18秒より短い。",
              "- ローカルAFM 3 Core Advancedのひらがな入力はCER約93%、0/200完全一致。"
              "PCCはそれより変換精度が大幅に高い一方、PCCには29/200件の安全制限・拒否があります。"
              "AFMはMacで異なるガードレール設定を使用したため、この点の厳密比較はできません。",
              "", "## 再生成", "",
              "```sh", "cd benchmark", "python3 generate_pcc_comparison.py -o PCC_IOS_COMPARISON.md", "```", "",
              "PCCの元結果はiOSベンチマーク専用ブランチの3ファイル。"
              "`import_pcc_ios.py`で設問ID・左文脈・許容解・入力文字列を検証して"
              "`result_pcc_ios/`へ取り込んでいます。AJIMEE-Bench v1由来の入力・許容解は"
              "CC BY-SA 3.0に従い、出典を同ディレクトリのREADMEに記載しています。", ""]
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-o", "--output", type=Path,
                        default=HERE / "PCC_IOS_COMPARISON.md")
    args = parser.parse_args()
    args.output.write_text(render(load_comparison()), encoding="utf-8")
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
