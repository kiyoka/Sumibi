#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""plot_context_length_vs_response_time.py

mlx-community/gemma-4-26b-a4b-it の Context Length 別ベンチマーク結果を可視化する。

対象ファイル (hiragana_input モード):
  result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx1024_hiragana.json
  result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx2048_hiragana.json
  result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx4096_hiragana.json
  result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx8192_hiragana.json

グラフ構成
============
- X 軸: Context Length (1024 / 2048 / 4096 / 8192)
- 左 Y 軸: レスポンス時間 (中央値, p95)
- 右 Y 軸: CER（文字誤り率 %）
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

import matplotlib.pyplot as plt

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from aggregate_results import summarize

RESULT_DIR = "result_ver2.4.0"
CTX_LENGTHS = [512, 1024, 2048, 4096, 8192]


def load_ctx_results() -> dict:
    """Context Length ごとの統計情報を読み込む。"""
    results = {}
    for ctx in CTX_LENGTHS:
        path = Path(RESULT_DIR) / f"gemma-4-26b-a4b-it-mlx_ctx{ctx}_hiragana.json"
        if not path.exists():
            print(f"Warning: {path} not found — skipping ctx={ctx}")
            continue
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
        (mean_cer, mean_at1, min_e, max_e,
         median_e, p95_e, mean_e_p95) = summarize(data)
        results[ctx] = {
            "mean_cer": mean_cer,
            "median_elapsed": median_e,
            "p95_elapsed": p95_e,
        }
    return results


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Plot context length vs response time for gemma-4-26b-a4b-it-mlx"
    )
    parser.add_argument("-o", "--output", help="Output image file path")
    args = parser.parse_args()

    data = load_ctx_results()
    if not data:
        print("Error: no result files found. Run benchmarks first.")
        sys.exit(1)

    ctx_list = sorted(data.keys())
    medians = [data[c]["median_elapsed"] for c in ctx_list]
    p95s    = [data[c]["p95_elapsed"]    for c in ctx_list]
    cers    = [data[c]["mean_cer"] * 100 for c in ctx_list]

    fig, ax1 = plt.subplots(figsize=(8, 5))

    ax1.plot(ctx_list, medians, marker="o", color="steelblue",
             label="Median response time (sec)")
    ax1.plot(ctx_list, p95s, marker="s", linestyle="--", color="cornflowerblue",
             label="p95 response time (sec)")

    # 実用基準ライン (2秒)
    ax1.axhline(y=2.0, color="red", linestyle=":", linewidth=1.0, label="2s threshold (IME target)")

    ax1.set_xlabel("Context Length (tokens)")
    ax1.set_ylabel("Response Time (sec)")
    ax1.set_xscale("log", base=2)
    ax1.set_xticks(ctx_list)
    ax1.set_xticklabels([str(c) for c in ctx_list])
    ax1.grid(axis="y", linestyle="--", alpha=0.5)

    # 右軸: CER
    ax2 = ax1.twinx()
    ax2.plot(ctx_list, cers, marker="^", linestyle="-.", color="teal",
             label="CER (%)")
    ax2.set_ylabel("CER (%)")
    ax2.set_ylim(0, 100)

    # 凡例を統合
    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper right", fontsize=8)

    plt.title("gemma-4-26b-a4b-it-mlx: Context Length vs Response Time & CER\n(hiragana_input, AJIMEE-Bench 200 items)")
    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=150, bbox_inches="tight")
        print(f"Saved: {args.output}")
    else:
        plt.show()


if __name__ == "__main__":
    main()
