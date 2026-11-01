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
from typing import Dict, List, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# ---------------------------------------------------------------------------
# マスタ情報 (コスト & 色) - plot_errorrate_vs_cost.py と同一定義
# ---------------------------------------------------------------------------
MASTER_COST: Dict[str, float] = {
    "gemini-3.5-flash-lite": 0.000650,  # Standard $0.30 input / $2.50 output per 1M; fixed 500/200 tokens
    "gemini-3.6-flash": 0.001125,       # $0.75 input + $3.75 output → (500×0.75 + 200×3.75)/1M = $1.125/1K
    "gemini-3.8-flash": 0.001125,       # Standard introductory price through 2026-12-31; fixed 500/200 tokens
    "gemini-4-argon": 0.003000,         # Standard introductory $2 input / $10 output per 1M through 2026-11-30
    "gpt-6.1-terra": 0.001500,          # Standard $1 input / $5 output per 1M; fixed 500/200 tokens
    "gpt-6.1-sol": 0.003000,            # Standard $2 input / $10 output per 1M; fixed 500/200 tokens
}

COLOR_MAP: Dict[str, str] = {
    "gemini-3.5-flash-lite": "darkcyan",
    "gemini-3.6-flash": "royalblue",
    "gemini-3.8-flash": "darkviolet",
    "gemini-4-argon": "firebrick",
    "gpt-6.1-terra": "teal",
    "gpt-6.1-sol": "mediumblue",
}

# ---------------------------------------------------------------------------
# JWTD_v2 (628件・ローマ字直接入力) の測定結果
# ---------------------------------------------------------------------------
DATA_JWTD_V2: Dict[str, Dict[str, float]] = {
    "gemini-3.5-flash-lite": {"cer": 0.1282, "elapsed": 0.851},
    "gemini-3.6-flash":      {"cer": 0.0595, "elapsed": 1.106},
    "gemini-3.8-flash":      {"cer": 0.0851, "elapsed": 2.068},
    "gemini-4-argon":        {"cer": 0.0312, "elapsed": 3.321},
    "gpt-6.1-terra":         {"cer": 0.0463, "elapsed": 1.305},
    "gpt-6.1-sol":           {"cer": 0.0368, "elapsed": 1.602},
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


def build_legend():
    """平均応答時間の凡例を描画"""
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
    parser.add_argument("-o", "--output", help="Output image file path")
    parser.add_argument(
        "--range",
        type=int,
        default=0,
        choices=[0, 1],
        help="Axis range mode: 0=default (0-70%%, auto x), 1=zoomed (0-40%%, 0-0.010$)",
    )
    args = parser.parse_args()

    plt.figure(figsize=(8, 6))

    plottable = [
        (model, metrics)
        for model, metrics in DATA_JWTD_V2.items()
        if MASTER_COST.get(model) is not None
    ]
    plottable.sort(key=lambda kv: kv[1]["elapsed"], reverse=True)

    label_positions = {
        "gemini-3.5-flash-lite": (0.000650 + 0.0005, 12.82 + 1.2),
        "gemini-3.8-flash":      (0.001125 + 0.0005, 8.51 + 1.2),
        "gemini-3.6-flash":      (0.001125 - 0.0008, 5.95 - 2.5),
        "gemini-4-argon":        (0.003000 + 0.0006, 3.12 + 2.5),
        "gpt-6.1-terra":         (0.001500 + 0.0005, 4.63 + 1.5),
        "gpt-6.1-sol":           (0.003000 + 0.0006, 3.68 - 2.5),
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

    plt.xlabel("Cost Per Request ($)")
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
        plt.ylim(0, 40)
        plt.xlim(0, 0.010)

    build_legend()

    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=300, bbox_inches="tight")
    else:
        plt.show()


if __name__ == "__main__":
    main()
