# -*- coding: utf-8 -*-
"""plot_errorrate_vs_inputtype_jwtd_v2.py

JWTD_v2 ベンチマーク結果における入力タイプ別（ローマ字、カタカナ、ひらがな）の
エラー率（CER）と平均レスポンス時間を比較するグラフを生成する。
images/plot_errorrate_vs_inputtype_1000x600.png と同一のスタイルを採用し、
タイトル等で JWTD_v2 であることを示す。
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from plot_jwtd_v2 import load_summary


def plot_errorrate_vs_inputtype_jwtd_v2(
    summary_path: Path,
    output_path: Path,
    figsize: Tuple[int, int] = (10, 6),
):
    summary = load_summary(summary_path)

    models_data = []
    for model_name, model_info in summary["models"].items():
        romaji_q = model_info["modes"]["romaji_direct_input"]["quality"]["overall"]
        hiragana_q = model_info["modes"]["hiragana_input"]["quality"]["overall"]
        katakana_q = model_info["modes"]["katakana_input"]["quality"]["overall"]

        romaji_lat = model_info["modes"]["romaji_direct_input"]["latency"]
        hiragana_lat = model_info["modes"]["hiragana_input"]["latency"]
        katakana_lat = model_info["modes"]["katakana_input"]["latency"]

        models_data.append({
            "model": model_name,
            "romaji": romaji_q["MinCER"],
            "hiragana": hiragana_q["MinCER"],
            "katakana": katakana_q["MinCER"],
            "romaji_time": romaji_lat["mean"],
            "hiragana_time": hiragana_lat["mean"],
            "katakana_time": katakana_lat["mean"],
        })

    # ひらがな入力のエラー率でソート（小さい順、元スクリプトと同一）
    models_data.sort(key=lambda x: x["hiragana"])

    models = [d["model"] for d in models_data]
    romaji_cers = [d["romaji"] * 100 for d in models_data]
    katakana_cers = [d["katakana"] * 100 for d in models_data]
    hiragana_cers = [d["hiragana"] * 100 for d in models_data]

    avg_times = [
        (d["romaji_time"] + d["hiragana_time"] + d["katakana_time"]) / 3
        for d in models_data
    ]

    fig, ax1 = plt.subplots(figsize=figsize, dpi=100)

    x = np.arange(len(models))
    width = 0.22

    # 棒グラフの作成（ローマ字、カタカナ、ひらがなの順）- 左Y軸
    bars1 = ax1.bar(
        x - width,
        romaji_cers,
        width,
        label="Romaji Input (Error Rate)",
        color="tab:blue",
        alpha=0.7,
    )
    bars2 = ax1.bar(
        x,
        katakana_cers,
        width,
        label="Katakana Input (Error Rate)",
        color="tab:green",
        alpha=0.7,
    )
    bars3 = ax1.bar(
        x + width,
        hiragana_cers,
        width,
        label="Hiragana Input (Error Rate)",
        color="tab:red",
        alpha=0.7,
    )

    # 左Y軸のラベルと設定
    ax1.set_xlabel("Model", fontsize=11, fontweight="bold")
    ax1.set_ylabel(
        "Character Error Rate (%)", fontsize=11, fontweight="bold", color="black"
    )
    ax1.set_xticks(x)
    ax1.set_xticklabels(models, rotation=15, ha="right", fontsize=9)
    ax1.set_ylim(0, 22)
    ax1.tick_params(axis="y", labelcolor="black")
    ax1.grid(axis="y", alpha=0.3, linestyle="--")

    # 右Y軸の作成（レスポンス時間用）
    ax2 = ax1.twinx()

    # 折れ線グラフの作成（平均レスポンス時間）- 右Y軸
    line = ax2.plot(
        x,
        avg_times,
        marker="o",
        linestyle="-",
        color="black",
        linewidth=2.2,
        markersize=6,
        label="Average Response Time",
        zorder=5,
    )

    ax2.set_ylabel(
        "Mean Response Time (sec)",
        fontsize=11,
        fontweight="bold",
        color="black",
    )
    ax2.set_ylim(0, 5)
    ax2.tick_params(axis="y", labelcolor="black")

    # タイトル
    ax1.set_title(
        "Error Rate and Response Time Comparison by Input Type (JWTD_v2)\n"
        "(Bars: Error Rate by Input Type, Line: Average Response Time)",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )

    # 凡例の統合
    lines_labels = [ax1.get_legend_handles_labels(), ax2.get_legend_handles_labels()]
    lines, labels = [sum(lol, []) for lol in zip(*lines_labels)]
    ax1.legend(lines, labels, loc="upper left", fontsize=8.5)

    plt.tight_layout()

    output_path.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_path, dpi=100, bbox_inches="tight")
    print(f"Graph saved to: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Plot error rate and response time comparison by input type (JWTD_v2)"
    )
    parser.add_argument(
        "summary",
        type=Path,
        nargs="?",
        default=Path("benchmark/issue205/summary-7models.json"),
        help="Path to summary JSON",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=Path("benchmark/issue205/figures-7models/plot_errorrate_vs_inputtype_jwtd_v2.png"),
        help="Output image file path",
    )
    args = parser.parse_args()

    plot_errorrate_vs_inputtype_jwtd_v2(args.summary, args.output)


if __name__ == "__main__":
    main()
