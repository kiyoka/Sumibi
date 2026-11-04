# -*- coding: utf-8 -*-
"""plot_errorrate_vs_cost_v1_vs_v2.py

JWTD_v1 (200件) と JWTD_v2 (628件) の両方でベンチマークを取得したモデルを
同一グラフ上で比較する散布図（バブルチャート）を生成する。

- X軸: Cost Per Request ($)
- Y軸: Error Rate / MinCER (%)
- 円サイズ: Mean Response Time (秒)
- 区別の工夫:
  - v1 (200件): 枠線のみの中空円 (solid border)
  - v2 (628件): 塗りつぶし円 (solid fill)
  - 同一モデルの v1 から v2 への変化を示す点線矢印
- 1000x600 px 出力
"""

from __future__ import annotations

import argparse
import json
import subprocess
from pathlib import Path
from typing import Dict, List, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# コスト (入力500トークン、出力200トークン想定の1リクエストあたりカタログコスト)
MASTER_COST: Dict[str, float] = {
    "gemini-3.5-flash-lite": 0.000650,
    "gemini-3.6-flash":      0.001125,
    "gemini-3.8-flash":      0.001125,
    "gemini-4-argon":        0.003000,
    "gpt-6.1-terra":         0.001500,
    "gpt-6.1-sol":           0.003000,
}

COLOR_MAP: Dict[str, str] = {
    "gemini-3.5-flash-lite": "darkcyan",
    "gemini-3.6-flash":      "royalblue",
    "gemini-3.8-flash":      "darkviolet",
    "gemini-4-argon":        "firebrick",
    "gpt-6.1-terra":         "teal",
    "gpt-6.1-sol":           "mediumblue",
}

# v1 (JWTD_v2/v1・200件・ローマ字入力) の実測値
DATA_V1: Dict[str, Dict[str, float]] = {
    "gemini-3.5-flash-lite": {"cer": 0.128534, "elapsed": 0.899951},
    "gemini-3.6-flash":      {"cer": 0.048211, "elapsed": 1.226725},
    "gemini-3.8-flash":      {"cer": 0.036695, "elapsed": 2.023744},
    "gemini-4-argon":        {"cer": 0.027495, "elapsed": 3.386980},
    "gpt-6.1-terra":         {"cer": 0.036960, "elapsed": 1.464286},
    "gpt-6.1-sol":           {"cer": 0.023608, "elapsed": 2.081238},
}

SCALE = 50  # size = elapsed * SCALE
LABEL_INK = "#1a1a1a"
SIZE_CAP_SEC = 15.0


def _darken_for_readability(color_name: str) -> str:
    override = {
        "palegreen": "darkgreen",
        "lightgreen": "green",
        "cyan": "darkcyan",
        "pink": "deeppink",
        "yellow": "darkgoldenrod",
        "gold": "darkgoldenrod",
    }
    return override.get(color_name, color_name)


def load_v2_data(summary_path: Path, mode: str = "romaji_direct_input") -> Dict[str, Dict[str, float]]:
    with open(summary_path, "r", encoding="utf-8") as f:
        summary = json.load(f)
    v2_data = {}
    for m in DATA_V1:
        if m in summary["models"]:
            q = summary["models"][m]["modes"][mode]["quality"]["overall"]
            lat = summary["models"][m]["modes"][mode]["latency"]
            v2_data[m] = {
                "cer": q["MinCER"],
                "elapsed": lat["mean"],
            }
    return v2_data


def plot_v1_vs_v2(
    summary_path: Path,
    output_path: Path,
    range_mode: int = 1,  # 0: standard (0-70%), 1: zoomed (0-25%)
    mode: str = "romaji_direct_input",
):
    v2_data = load_v2_data(summary_path, mode)

    fig, ax = plt.subplots(figsize=(8, 6), dpi=150)

    # 1. まず各モデルの v1 から v2 への変遷矢印を描画 (最背面)
    for model in DATA_V1:
        if model not in v2_data:
            continue
        cost = MASTER_COST[model]
        v1_cer = DATA_V1[model]["cer"] * 100
        v2_cer = v2_data[model]["cer"] * 100
        color = COLOR_MAP.get(model, "gray")

        # 同一コストでCERが変化する矢印
        ax.annotate(
            "",
            xy=(cost, v2_cer),
            xytext=(cost, v1_cer),
            arrowprops=dict(
                arrowstyle="->,head_width=0.25,head_length=0.4",
                color=color,
                lw=1.5,
                linestyle="--",
                alpha=0.65,
            ),
            zorder=2,
        )

    # 2. v1 (中空・枠線のみの円) を描画
    for idx, model in enumerate(DATA_V1):
        cost = MASTER_COST[model]
        cer_pct = DATA_V1[model]["cer"] * 100
        elapsed = DATA_V1[model]["elapsed"]
        size = min(elapsed, SIZE_CAP_SEC) * SCALE
        color = COLOR_MAP.get(model, "blue")

        ax.scatter(
            cost,
            cer_pct,
            s=size,
            facecolors="none",
            edgecolors=color,
            linewidths=1.8,
            linestyle="-",
            alpha=0.85,
            zorder=3 + idx / 20.0,
        )

    # 3. v2 (塗りつぶし円) を描画
    for idx, model in enumerate(v2_data):
        cost = MASTER_COST[model]
        cer_pct = v2_data[model]["cer"] * 100
        elapsed = v2_data[model]["elapsed"]
        size = min(elapsed, SIZE_CAP_SEC) * SCALE
        color = COLOR_MAP.get(model, "blue")

        ax.scatter(
            cost,
            cer_pct,
            s=size,
            facecolors=color,
            edgecolors="white",
            linewidths=1.2,
            alpha=0.75,
            zorder=4 + idx / 20.0,
        )

    # 4. ラベルの配置
    # v1とv2の両方を識別できるようモデル名ラベルを配置
    label_positions = {
        "gemini-3.5-flash-lite": (0.000650 + 0.0004, 12.82 + 1.2),
        "gemini-3.6-flash":      (0.001125 - 0.0008, 5.95 + 1.2),
        "gemini-3.8-flash":      (0.001125 + 0.0004, 8.51 + 1.5),
        "gemini-4-argon":        (0.003000 + 0.0004, 3.12 + 1.2),
        "gpt-6.1-terra":         (0.001500 + 0.0004, 4.63 + 1.2),
        "gpt-6.1-sol":           (0.003000 + 0.0004, 3.68 - 1.8),
    }

    for model, (lx, ly) in label_positions.items():
        if model in v2_data:
            cost = MASTER_COST[model]
            v2_cer = v2_data[model]["cer"] * 100
            color = COLOR_MAP.get(model, "blue")
            ax.text(lx, ly, model, fontsize=8, color=LABEL_INK, zorder=6, fontweight="bold")
            ax.plot(
                [cost, lx],
                [v2_cer, ly],
                color=_darken_for_readability(color),
                lw=0.8,
                alpha=0.7,
                zorder=2,
            )

    ax.set_xlabel("Cost Per Request ($)", fontsize=10, fontweight="bold")
    ax.set_ylabel("Error Rate - MinCER (%)", fontsize=10, fontweight="bold")

    if range_mode == 1:
        ax.set_title("Error Rate vs Cost: v1 (200) vs v2 (628) Comparison - Zoomed",
                     fontsize=11, fontweight="bold", pad=12)
        ax.set_ylim(0, 18)
        ax.set_xlim(0, 0.0045)
    else:
        ax.set_title("Error Rate vs Cost: v1 (200) vs v2 (628) Comparison",
                     fontsize=11, fontweight="bold", pad=12)
        ax.set_ylim(0, 70)
        ax.set_xlim(-0.0005, 0.024)

    ax.grid(True, which="both", linestyle=":", linewidth=0.5)

    # 凡例 1: データセット凡例 (v1 vs v2)
    legend_dataset_handles = [
        Line2D([], [], marker="o", color="gray", linestyle="None",
               markerfacecolor="none", markeredgecolor="gray", markeredgewidth=1.8, markersize=10,
               label="v1 (200 cases)"),
        Line2D([], [], marker="o", color="gray", linestyle="None",
               markerfacecolor="gray", markeredgecolor="gray", markersize=10,
               label="v2 (628 cases)"),
        Line2D([], [], color="gray", linestyle="--", lw=1.5,
               label="v1 → v2 shift"),
    ]
    leg1 = ax.legend(
        handles=legend_dataset_handles,
        loc="upper left",
        title="Dataset Version",
        fontsize=8,
        title_fontsize=8.5,
        framealpha=0.85,
    )
    ax.add_artist(leg1)

    # 凡例 2: 平均応答時間凡例 (円サイズ)
    seconds_legend = [1, 2, 3, 5]
    legend_size_handles = []
    for sec in seconds_legend:
        s = min(sec, SIZE_CAP_SEC) * SCALE
        legend_size_handles.append(
            Line2D([], [], marker="o", color="gray", linestyle="None",
                   markersize=s ** 0.5, label=f"{sec} sec")
        )
    ax.legend(
        handles=legend_size_handles,
        loc="upper right",
        title="Mean Response Time",
        fontsize=8,
        title_fontsize=8.5,
        framealpha=0.85,
    )

    plt.tight_layout()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print("Saved:", output_path)

    # 1000x600 バージョンの生成
    if "_1000x600" not in output_path.stem:
        resized_path = output_path.parent / f"{output_path.stem}_1000x600.png"
        try:
            subprocess.run(["convert", str(output_path), "-resize", "1000x600", str(resized_path)], check=True)
            print("Resized:", resized_path)
        except Exception as e:
            print(f"Warning: Failed to resize image ({e})")


def main():
    parser = argparse.ArgumentParser(description="Plot v1 vs v2 error rate vs cost comparison")
    parser.add_argument(
        "summary",
        type=Path,
        nargs="?",
        default=Path("benchmark/issue205/summary-6models.json"),
        help="Path to summary-6models.json",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("benchmark/issue205/figures-6models"),
        help="Output directory",
    )
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)

    # 通常版
    raw_path_0 = args.output_dir / "plot_errorrate_vs_cost_v1_vs_v2.png"
    plot_v1_vs_v2(args.summary, raw_path_0, range_mode=0)

    # Zoomed版
    raw_path_1 = args.output_dir / "plot_errorrate_vs_cost_v1_vs_v2_zoomed.png"
    plot_v1_vs_v2(args.summary, raw_path_1, range_mode=1)


if __name__ == "__main__":
    main()
