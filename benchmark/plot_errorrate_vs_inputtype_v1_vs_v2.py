# -*- coding: utf-8 -*-
"""plot_errorrate_vs_inputtype_v1_vs_v2.py

JWTD_v1 (200件) と JWTD_v2 (628件) の両方で計測されたモデルについて、
入力タイプ別（ローマ字、カタカナ、ひらがな）のエラー率（CER）と平均レスポンス時間を
同一グラフ上で比較する棒グラフ＋折れ線グラフを生成する。

- 棒グラフ: 入力タイプ別の文字誤り率 (CER %)
  - v1 (200件): 斜線ハッチング (hatched) / 透過 (alpha=0.45)
  - v2 (628件): 塗りつぶし (solid) / 不透明 (alpha=0.85)
- 折れ線: 平均レスポンス時間 (秒)
  - v1 (200件): 破線 + 白抜きマーカー
  - v2 (628件): 実線 + 黒塗りマーカー
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path
from plot_jwtd_v2 import load_summary
from typing import Dict, List, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

# v1 (JWTD_v1/v2.4.0・200件) の計測値（フォールバック用キャッシュ）
DATA_V1_FALLBACK: Dict[str, Dict[str, float]] = {
    "gemini-4-argon": {
        "romaji": 0.027494649413082238,
        "hiragana": 0.007474521814020325,
        "katakana": 0.024333011679545526,
        "romaji_time": 3.3869802383421015,
        "hiragana_time": 3.3154889479842113,
        "katakana_time": 3.5607017637631686,
    },
    "gpt-6.1-sol": {
        "romaji": 0.023608194915825262,
        "hiragana": 0.00552032352752167,
        "katakana": 0.0099546635339205,
        "romaji_time": 2.0812378040052635,
        "hiragana_time": 1.6624392774736838,
        "katakana_time": 1.820378386605262,
    },
    "gpt-6.1-terra": {
        "romaji": 0.03696009597346714,
        "hiragana": 0.02200597610500356,
        "katakana": 0.03681622463919584,
        "romaji_time": 1.4642856723736846,
        "hiragana_time": 1.4075337736789466,
        "katakana_time": 1.4613380692894715,
    },
    "gemini-3.6-flash": {
        "romaji": 0.04821060620984765,
        "hiragana": 0.027319387771485352,
        "katakana": 0.04619854002459639,
        "romaji_time": 1.2267254784197767,
        "hiragana_time": 1.1802212621066965,
        "katakana_time": 1.3092365715784438,
    },
    "gemini-3.8-flash": {
        "romaji": 0.036695487835916384,
        "hiragana": 0.015681412135767268,
        "katakana": 0.03541723105452404,
        "romaji_time": 2.023743754405264,
        "hiragana_time": 2.0184177116052577,
        "katakana_time": 2.0459374243052695,
    },
    "gemini-3.5-flash-lite": {
        "romaji": 0.1285344627840322,
        "hiragana": 0.08386833152343957,
        "katakana": 0.15693418205973933,
        "romaji_time": 0.8999512191315788,
        "hiragana_time": 1.0297683624736862,
        "katakana_time": 0.9751635776315766,
    },
}


def load_v1_data(v1_dir: Path, models: List[str]) -> Dict[str, Dict[str, float]]:
    """result_ver2.4.0 ディレクトリから v1 データを読み込む。"""
    v1_data = {}
    benchmark_dir = Path(__file__).resolve().parent
    sys.path.insert(0, str(benchmark_dir))
    try:
        from aggregate_results import summarize
        has_summarize = True
    except ImportError:
        has_summarize = False

    for m in models:
        r_file = v1_dir / f"{m}.json"
        h_file = v1_dir / f"{m}_hiragana.json"
        k_file = v1_dir / f"{m}_katakana.json"

        if has_summarize and r_file.exists() and h_file.exists() and k_file.exists():
            def calc_mean_cer(data):
                return sum(min(rec.get("cer", 0.0), 1.0) for rec in data) / len(data)

            with open(r_file, "r", encoding="utf-8") as f:
                r_d = json.load(f)
            with open(h_file, "r", encoding="utf-8") as f:
                h_d = json.load(f)
            with open(k_file, "r", encoding="utf-8") as f:
                k_d = json.load(f)

            v1_data[m] = {
                "romaji": calc_mean_cer(r_d),
                "hiragana": calc_mean_cer(h_d),
                "katakana": calc_mean_cer(k_d),
                "romaji_time": summarize(r_d)[6],
                "hiragana_time": summarize(h_d)[6],
                "katakana_time": summarize(k_d)[6],
            }
        elif m in DATA_V1_FALLBACK:
            v1_data[m] = DATA_V1_FALLBACK[m]

    return v1_data


def plot_errorrate_vs_inputtype_v1_vs_v2(
    summary_path: Path,
    output_path: Path,
    v1_dir: Path = Path("benchmark/result_ver2.4.0"),
    figsize: Tuple[int, int] = (10, 6),
):
    summary = load_summary(summary_path)
    target_models = list(summary["models"])
    v1_data = load_v1_data(v1_dir, target_models)

    models_data = []
    for m in target_models:
        v1_info = v1_data.get(m)
        v2_info = summary["models"][m]

        rq2 = v2_info["modes"]["romaji_direct_input"]["quality"]["overall"]["MinCER"]
        hq2 = v2_info["modes"]["hiragana_input"]["quality"]["overall"]["MinCER"]
        kq2 = v2_info["modes"]["katakana_input"]["quality"]["overall"]["MinCER"]

        rl2 = v2_info["modes"]["romaji_direct_input"]["latency"]["mean"]
        hl2 = v2_info["modes"]["hiragana_input"]["latency"]["mean"]
        kl2 = v2_info["modes"]["katakana_input"]["latency"]["mean"]

        v1_avg_time = (sum(v1_info[f"{mode}_time"] for mode in ("romaji", "hiragana", "katakana")) / 3
                       if v1_info else np.nan)
        v2_avg_time = (rl2 + hl2 + kl2) / 3

        models_data.append({
            "model": m,
            "romaji_v1": v1_info["romaji"] * 100 if v1_info else np.nan,
            "romaji_v2": rq2 * 100,
            "katakana_v1": v1_info["katakana"] * 100 if v1_info else np.nan,
            "katakana_v2": kq2 * 100,
            "hiragana_v1": v1_info["hiragana"] * 100 if v1_info else np.nan,
            "hiragana_v2": hq2 * 100,
            "avg_time_v1": v1_avg_time,
            "avg_time_v2": v2_avg_time,
        })

    # v2 ひらがな入力のエラー率でソート（小さい順、plot_errorrate_vs_inputtype_jwtd_v2.py と整合）
    models_data.sort(key=lambda x: x["hiragana_v2"])

    models = [d["model"] for d in models_data]
    x = np.arange(len(models))
    w = 0.11

    fig, ax1 = plt.subplots(figsize=figsize, dpi=100)

    # 棒グラフの描画（ローマ字、カタカナ、ひらがな × v1, v2）
    # Romaji
    ax1.bar(
        x - 2.5 * w,
        [d["romaji_v1"] for d in models_data],
        w,
        label="Romaji v1 (200 items)",
        color="tab:blue",
        alpha=0.45,
        hatch="//",
        edgecolor="tab:blue",
        linewidth=1.0,
    )
    ax1.bar(
        x - 1.5 * w,
        [d["romaji_v2"] for d in models_data],
        w,
        label="Romaji v2 (628 items)",
        color="tab:blue",
        alpha=0.85,
        edgecolor="tab:blue",
        linewidth=1.0,
    )

    # Katakana
    ax1.bar(
        x - 0.5 * w,
        [d["katakana_v1"] for d in models_data],
        w,
        label="Katakana v1 (200 items)",
        color="tab:green",
        alpha=0.45,
        hatch="//",
        edgecolor="tab:green",
        linewidth=1.0,
    )
    ax1.bar(
        x + 0.5 * w,
        [d["katakana_v2"] for d in models_data],
        w,
        label="Katakana v2 (628 items)",
        color="tab:green",
        alpha=0.85,
        edgecolor="tab:green",
        linewidth=1.0,
    )

    # Hiragana
    ax1.bar(
        x + 1.5 * w,
        [d["hiragana_v1"] for d in models_data],
        w,
        label="Hiragana v1 (200 items)",
        color="tab:red",
        alpha=0.45,
        hatch="//",
        edgecolor="tab:red",
        linewidth=1.0,
    )
    ax1.bar(
        x + 2.5 * w,
        [d["hiragana_v2"] for d in models_data],
        w,
        label="Hiragana v2 (628 items)",
        color="tab:red",
        alpha=0.85,
        edgecolor="tab:red",
        linewidth=1.0,
    )

    # 左Y軸のラベルと設定
    ax1.set_xlabel("Model", fontsize=11, fontweight="bold")
    ax1.set_ylabel("Character Error Rate (%)", fontsize=11, fontweight="bold", color="black")
    ax1.set_xticks(x)
    ax1.set_xticklabels([m if m in v1_data else f"{m} (v2 only)" for m in models],
                       rotation=15, ha="right", fontsize=9.5)
    ax1.set_ylim(0, 22)
    ax1.tick_params(axis="y", labelcolor="black")
    ax1.grid(axis="y", alpha=0.3, linestyle="--")

    # 右Y軸の作成（レスポンス時間用）
    ax2 = ax1.twinx()

    # 折れ線グラフの作成（平均レスポンス時間）
    ax2.plot(
        x,
        [d["avg_time_v1"] for d in models_data],
        marker="o",
        linestyle="--",
        color="dimgray",
        linewidth=2.0,
        markersize=6,
        markerfacecolor="white",
        markeredgecolor="dimgray",
        label="Mean Response Time v1 (sec)",
        zorder=5,
    )
    ax2.plot(
        x,
        [d["avg_time_v2"] for d in models_data],
        marker="o",
        linestyle="-",
        color="black",
        linewidth=2.2,
        markersize=6,
        markerfacecolor="black",
        markeredgecolor="black",
        label="Mean Response Time v2 (sec)",
        zorder=5,
    )

    ax2.set_ylabel("Mean Response Time (sec)", fontsize=11, fontweight="bold", color="black")
    ax2.set_ylim(0, 5)
    ax2.tick_params(axis="y", labelcolor="black")

    # タイトル
    ax1.set_title(
        "Error Rate and Response Time Comparison by Input Type (v1 vs v2)\n"
        "(Hatched bars / Dashed line: v1, Solid bars / Solid line: v2)",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )

    # 凡例の並び順（2列で美しく整理）
    handles1, labels1 = ax1.get_legend_handles_labels()
    handles2, labels2 = ax2.get_legend_handles_labels()

    # Col 1: v1 (Romaji, Katakana, Hiragana, Time)
    # Col 2: v2 (Romaji, Katakana, Hiragana, Time)
    ordered_handles = [
        handles1[0], handles1[2], handles1[4], handles2[0],
        handles1[1], handles1[3], handles1[5], handles2[1],
    ]
    ordered_labels = [
        labels1[0], labels1[2], labels1[4], labels2[0],
        labels1[1], labels1[3], labels1[5], labels2[1],
    ]

    ax1.legend(ordered_handles, ordered_labels, loc="upper left", ncol=2, fontsize=8.2, framealpha=0.9)

    plt.tight_layout()

    output_path.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_path, dpi=100, bbox_inches="tight")
    plt.close(fig)
    print(f"Graph saved to: {output_path}")

    # 1000x600 バージョンの生成
    if "_1000x600" not in output_path.stem:
        resized_path = output_path.parent / f"{output_path.stem}_1000x600.png"
    else:
        resized_path = output_path

    # PIL または convert でリサイズ
    try:
        img = Image.open(output_path)
        img_resized = img.resize((1000, 600), Image.Resampling.LANCZOS)
        img_resized.save(resized_path)
        print(f"Resized graph saved to: {resized_path}")
    except Exception as e:
        print(f"Warning: Failed to resize image with PIL ({e}), trying convert command...")
        subprocess.run(["convert", str(output_path), "-resize", "1000x600", str(resized_path)], check=True)


def main():
    parser = argparse.ArgumentParser(
        description="Plot error rate and response time comparison by input type (v1 vs v2)"
    )
    parser.add_argument(
        "summary",
        type=Path,
        nargs="?",
        default=Path("benchmark/issue205/summary-7models.json"),
        help="Path to JWTD_v2 summary JSON",
    )
    parser.add_argument(
        "--v1-dir",
        type=Path,
        default=Path("benchmark/result_ver2.4.0"),
        help="Path to directory containing v1 result JSONs",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("benchmark/issue205/figures-7models"),
        help="Output directory for generated plots",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=None,
        help="Specific output image path",
    )
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)

    if args.output:
        plot_errorrate_vs_inputtype_v1_vs_v2(args.summary, args.output, v1_dir=args.v1_dir)
    else:
        raw_output = args.output_dir / "plot_errorrate_vs_inputtype_v1_vs_v2.png"
        plot_errorrate_vs_inputtype_v1_vs_v2(args.summary, raw_output, v1_dir=args.v1_dir)


if __name__ == "__main__":
    main()
