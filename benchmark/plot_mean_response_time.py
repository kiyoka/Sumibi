#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""plot_mean_response_time.py

グラフ構成
============
1. X 軸 (主軸, ax1)
   • 各モデルの平均応答時間 mean_elapsed_sec_p95 (v2.4.0)
     -> 横向きバーで表示（95パーセンタイル以下の平均、外れ値除外）

2. X 軸 (副軸, ax2 – 上側)
   • 各モデルの誤り率 mean_cer (v2.4.0)
     -> ティール菱形 & 破線の折れ線表示

凡例
----
• 右下 : Error Rate (v2.4.0)

バーの右横には v2.4.0 の平均応答時間値を注釈。
"""

from __future__ import annotations

import argparse
import json
import glob
from typing import List, Dict, Tuple
from pathlib import Path

import matplotlib.pyplot as plt

# aggregate_results.pyからsummarize関数をインポート
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from aggregate_results import summarize

# ---------------------------------------------------------------------------
# データ読み込み
# ---------------------------------------------------------------------------

def load_all_results(result_dir: str = "result_ver2.4.0") -> Dict[str, Tuple[float, float, float]]:
    """
    JSONファイルからすべてのモデルの統計情報を読み込む
    戻り値: {model_name: (mean_cer, mean_elapsed_sec_p95)}
    """
    results = {}
    json_files = glob.glob(f"{result_dir}/*.json")

    for json_file in json_files:
        model_name = Path(json_file).stem
        try:
            with open(json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            (mean_cer, mean_at1, min_elapsed_sec, max_elapsed_sec,
             median_elapsed_sec, p95_elapsed_sec, mean_elapsed_sec_p95) = summarize(data)
            results[model_name] = (mean_cer, mean_elapsed_sec_p95)
        except Exception as e:
            print(f"Warning: Failed to load {json_file}: {e}")

    return results

def get_color_for_model(model_name: str) -> str:
    """モデル名に基づいて色を割り当てる"""
    if model_name.startswith("gpt-3"):
        return "palegreen"
    elif model_name.startswith("gpt-4.1-mini"):
        return "lightgreen"
    elif model_name.startswith("gpt-4.1"):
        return "mediumspringgreen"
    elif model_name.startswith("gpt-4o-mini"):
        return "springgreen"
    elif model_name.startswith("gpt-4o"):
        return "mediumseagreen"
    elif model_name.startswith("o4"):
        return "springgreen"
    elif model_name.startswith("o3"):
        return "cyan"
    elif model_name.startswith("deepseek"):
        return "pink"
    elif model_name.startswith("gemini-2.0-flash-lite"):
        return "silver"
    elif model_name.startswith("gemini-2.0-flash"):
        return "lightgray"
    elif model_name.startswith("gemini-2.5-flash"):
        return "darkgray"
    elif model_name.startswith("gemini-2.5-pro"):
        return "gray"
    elif model_name.startswith("gemini-3-flash-preview"):
        return "steelblue"
    elif model_name.startswith("gemma"):
        return "wheat"
    elif model_name.startswith("claude-opus-4-1"):
        return "mediumpurple"
    elif model_name.startswith("claude-opus-4"):
        return "blueviolet"
    elif model_name.startswith("claude-sonnet-4-5"):
        return "orchid"
    elif model_name.startswith("claude-sonnet-4"):
        return "mediumorchid"
    elif model_name.startswith("gpt-5-mini"):
        if "medium" in model_name:
            return "limegreen"
        elif "minimal+low" in model_name:
            return "darkseagreen"
        else:
            return "forestgreen"
    elif model_name.startswith("gpt-5-nano"):
        return "darkgreen"
    elif model_name.startswith("gpt-5"):
        if "medium" in model_name:
            return "green"
        elif "minimal+low" in model_name:
            return "lightseagreen"
        else:
            return "seagreen"
    elif model_name.startswith("gpt-oss"):
        return "olive"
    elif model_name.startswith("llm-jp-3.1-13b"):
        return "coral" if "_hiragana" not in model_name else "lightsalmon"
    elif model_name.startswith("llm-jp"):
        return "salmon"
    else:
        return "steelblue"

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description="Plot mean response time (95th percentile) & error rate (v2.4.0)")
    parser.add_argument("-o", "--output", help="Output image file path")
    args = parser.parse_args()

    # ---------------------------------------------------------------------------
    # データ準備
    # ---------------------------------------------------------------------------

    # JSONファイルから動的に読み込み
    results = load_all_results()

    models = list(results.keys())
    mean_cer_list = [results[m][0] for m in models]
    mean_elapsed_p95_list = [results[m][1] for m in models]
    bar_colors = [get_color_for_model(m) for m in models]

    # 平均応答時間が長い順にソート
    sorted_idx = sorted(range(len(models)), key=lambda i: mean_elapsed_p95_list[i], reverse=True)

    models_sorted = [models[i] for i in sorted_idx]
    elapsed_v24_sorted = [mean_elapsed_p95_list[i] for i in sorted_idx]
    cer_v24_sorted = [mean_cer_list[i] for i in sorted_idx]
    bar_colors_sorted = [bar_colors[i] for i in sorted_idx]

    # ---------------------------------------------------------------------------
    # 描画
    # ---------------------------------------------------------------------------

    fig, ax1 = plt.subplots(figsize=(10, 18))

    # -- v2.4.0: 横棒バー (95パーセンタイル)
    bars = ax1.barh(models_sorted, elapsed_v24_sorted, color=bar_colors_sorted)

    ax1.set_xlabel("Mean Response Time (sec, 95th percentile)")
    ax1.set_ylabel("Model")
    ax1.grid(axis="x", linestyle="--", alpha=0.7)

    # バーの中心 y 座標
    y_centers = [bar.get_y() + bar.get_height() / 2 for bar in bars]

    # -------- 副軸 (上): CER --------
    ax2 = ax1.twiny()

    # Error Rate v2.4.0: Filled circle
    cer_pct_v24 = [c * 100 for c in cer_v24_sorted]
    ax2.plot(
        cer_pct_v24,
        y_centers,
        linestyle="--",
        marker="o",
        markerfacecolor="teal",
        markeredgecolor="teal",
        color="teal",
        label="Error Rate (v2.4.0)",
    )

    ax2.set_xlabel("Error Rate (%)")
    ax2.set_xlim(0, 100)
    ax2.set_xticks(range(0, 101, 10))
    ax2.xaxis.set_label_position("top")
    ax2.xaxis.set_ticks_position("top")

    # -- 凡例（Error Rate のみ表示）
    ax2.legend(loc="upper right")

    # -- バーに数値を注釈（v2.4.0, 95パーセンタイル）
    for bar, val in zip(bars, elapsed_v24_sorted):
        x = bar.get_width()
        y = bar.get_y() + bar.get_height() / 2
        ax1.annotate(
            f"{val:.2f}s",
            xy=(x, y),
            xytext=(5, 0),
            textcoords="offset points",
            ha="left",
            va="center",
        )

    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=300, bbox_inches="tight")
    else:
        plt.show()


if __name__ == "__main__":
    main()
