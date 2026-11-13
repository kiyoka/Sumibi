# -*- coding: utf-8 -*-
"""plot_errorrate_vs_cost_jwtd_v2.py

JWTD_v2 ベンチマーク結果を散布図で表示する。
plot_errorrate_vs_cost.py と同一のスタイル・レイアウトを採用し、
データセットが JWTD_v2 であることを示す。

各LLMモデルのエラー率（CER）とコスト、平均応答時間を可視化する。
円のサイズは平均応答時間を表し、大きいほど応答時間が長い。
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from plot_jwtd_v2 import load_summary

# ---------------------------------------------------------------------------
# マスタ情報 (コスト & 色) - plot_errorrate_vs_cost.py と同一定義
# ---------------------------------------------------------------------------
MASTER_COST: Dict[str, float] = {
    "gemini-3.5-flash-lite": 0.000650,  # Standard $0.30 input / $2.50 output per 1M; fixed 500/200 tokens
    "gemini-3.6-flash": 0.001125,       # $0.75 input + $3.75 output → (500×0.75 + 200×3.75)/1M = $1.125/1K
    "gemini-3.8-flash": 0.001125,       # Standard introductory price through 2026-12-31; fixed 500/200 tokens
    "gemini-4-argon": 0.003000,         # Standard introductory $2 input / $10 output per 1M through 2026-11-30
    "gemini-4-hydrogen": 0.002250,      # Standard $1.50 input / $7.50 output per 1M
    "gpt-6.1-terra": 0.001500,          # Standard $1 input / $5 output per 1M; fixed 500/200 tokens
    "gpt-6.1-sol": 0.003000,            # Standard $2 input / $10 output per 1M; fixed 500/200 tokens
}

COLOR_MAP: Dict[str, str] = {
    "gemini-3.5-flash-lite": "darkcyan",
    "gemini-3.6-flash": "royalblue",
    "gemini-3.8-flash": "darkviolet",
    "gemini-4-argon": "firebrick",
    "gemini-4-hydrogen": "darkorange",
    "gpt-6.1-terra": "teal",
    "gpt-6.1-sol": "mediumblue",
}

# ---------------------------------------------------------------------------
# JWTD_v2 (628件・ローマ字直接入力) の測定結果
# ---------------------------------------------------------------------------
def load_v2_data(summary_path: Path, mode: str = "romaji_direct_input") -> Dict[str, Dict[str, float]]:
    return {
        model: {"cer": info["modes"][mode]["quality"]["overall"]["MinCER"],
                "elapsed": info["modes"][mode]["latency"]["mean"]}
        for model, info in load_summary(summary_path)["models"].items()
        if model in MASTER_COST
    }

SCALE = 50  # size = elapsed * SCALE
LABEL_INK = "#1a1a1a"
SIZE_CAP_SEC = 15.0


def _darken_for_readability(color_name: str) -> str:
    """白背景で読みやすくするため、明るすぎる色は暗めの同系色に置き換える。"""
    override = {
        "palegreen": "darkgreen",
        "lightgreen": "green",
        "mediumspringgreen": "darkgreen",
        "springgreen": "darkgreen",
        "lightgray": "dimgray",
        "silver": "dimgray",
        "yellow": "darkgoldenrod",
        "gold": "darkgoldenrod",
        "khaki": "olive",
        "lime": "darkgreen",
        "chartreuse": "darkgreen",
        "cyan": "darkcyan",
        "pink": "deeppink",
        "wheat": "saddlebrown",
    }
    return override.get(color_name, color_name)


def build_legend(range_mode: int = 0):
    """平均応答時間の凡例を描画"""
    if range_mode == 1:
        seconds_legend = [1, 2, 3, 5]
    else:
        seconds_legend = [1, 5, 10, SIZE_CAP_SEC]
    legend_size_handles: List[Line2D] = []
    for sec in seconds_legend:
        size = sec * SCALE
        capped = sec >= SIZE_CAP_SEC
        legend_size_handles.append(
            Line2D(
                [],
                [],
                marker="o",
                color="gray",
                linestyle="None",
                markersize=size ** 0.5,
                label=f"{sec:.0f}+ sec" if capped else f"{sec:.0f} sec",
            )
        )

    ax = plt.gca()
    ax.legend(
        handles=legend_size_handles,
        loc="upper right",
        title="Mean Response Time",
        framealpha=0.8,
    )


def main():
    parser = argparse.ArgumentParser(description="Plot error rate vs cost (JWTD_v2)")
    parser.add_argument("summary", type=Path, nargs="?",
                        default=Path("benchmark/JWTD_v2/summary-7models.json"))
    parser.add_argument("-o", "--output", help="Output image file path")
    parser.add_argument(
        "--range",
        type=int,
        default=0,
        choices=[0, 1],
        help="Axis range mode: 0=default (0-70%%, auto x), 1=zoomed (1.5-14.5%%, 0.0003-0.0038$)",
    )
    args = parser.parse_args()

    plt.figure(figsize=(8, 6))

    plottable = list(load_v2_data(args.summary).items())
    plottable.sort(key=lambda kv: kv[1]["elapsed"], reverse=True)

    if args.range == 0:
        label_positions = {
            "gemini-3.5-flash-lite": (0.0012, 24),
            "gemini-3.8-flash":      (0.0022, 19),
            "gemini-3.6-flash":      (0.0036, 15),
            "gemini-4-hydrogen":     (0.0049, 11),
            "gpt-6.1-terra":         (0.0049, 7.5),
            "gpt-6.1-sol":           (0.0049, 4.0),
            "gemini-4-argon":       (0.0049, 0.8),
        }
    else:
        label_positions = {
            "gemini-3.5-flash-lite": (0.000650 + 0.00015, 12.82 + 0.6),
            "gemini-3.8-flash":      (0.001125 + 0.00015, 8.51 + 0.6),
            "gemini-3.6-flash":      (0.001125 - 0.00045, 5.95 - 0.5),
            "gpt-6.1-terra":         (0.001500 + 0.00015, 4.63 + 0.8),
            "gpt-6.1-sol":           (0.003000 + 0.00015, 3.68 + 0.8),
            "gemini-4-argon":        (0.003000 + 0.00015, 3.12 - 0.9),
            "gemini-4-hydrogen":     (0.00235, 7.35),
        }

    ax = plt.gca()

    for idx, (model, metrics) in enumerate(plottable):
        cost = MASTER_COST[model]
        cer_pct = metrics["cer"] * 100
        size = min(metrics["elapsed"], SIZE_CAP_SEC) * SCALE
        color = COLOR_MAP.get(model, "blue")
        z = 3 + idx / (len(plottable) + 1)

        plt.scatter(
            cost,
            cer_pct,
            s=size,
            facecolors=color,
            edgecolors="white",
            linewidths=1.2,
            alpha=0.75,
            zorder=z,
        )

        # ラベルと接続線
        lx, ly = label_positions.get(model, (cost + 0.0005, cer_pct + 1.0))
        plt.text(lx, ly, model, fontsize=8, color=LABEL_INK, clip_on=False, zorder=5)

        ax.plot(
            [cost, lx],
            [cer_pct, ly],
            color=_darken_for_readability(color),
            lw=1.0,
            alpha=0.9,
            zorder=2,
        )

    plt.xlabel("Reference Cost Per Request ($; 500 input / 200 output tokens)")
    plt.ylabel("Error Rate (%)")

    # タイトル設定
    if args.range == 1:
        plt.title("Error Rate vs Cost of LLM Model (JWTD_v2) - Zoomed")
    else:
        plt.title("Error Rate vs Cost of LLM Model (JWTD_v2)")

    plt.grid(True, which="both", linestyle=":", linewidth=0.5)

    # 軸範囲設定
    if args.range == 0:
        plt.ylim(0, 70)
        plt.xlim(-0.0005, 0.024)  # v1 と同じX軸スケール感
    elif args.range == 1:
        plt.ylim(1.5, 14.5)
        plt.xlim(0.0003, 0.0038)

    build_legend(args.range)

    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=300, bbox_inches="tight")
    else:
        plt.show()


if __name__ == "__main__":
    main()
