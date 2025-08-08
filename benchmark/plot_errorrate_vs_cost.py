# -*- coding: utf-8 -*-
"""plot_errorrate_vs_cost.py

v2.4.0 ベンチマーク結果を散布図で表示する。

各LLMモデルのエラー率（CER）とコスト、平均応答時間を可視化する。
円のサイズは平均応答時間を表し、大きいほど応答時間が長い。

→ JSON を直接パースするのではなく、aggregate_results.py の出力値を
  そのまま転記した定数で実装している（従来スクリプトと同一方針）。
"""

from __future__ import annotations

import argparse
from typing import Dict, List

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# ---------------------------------------------------------------------------
# マスタ情報 (コスト & 色)
# ---------------------------------------------------------------------------
# 価格は sumibi_typical_convert_client のプロンプト (入力500トークン、出力200トークン) を想定し、
# OpenAI公式価格表 (2025年1月) およびAnthropic公式価格表に基づき計算
# OpenAI: https://platform.openai.com/docs/pricing
# Anthropic Claude: Input $15/1M, Output $75/1M (Opus), Input $3/1M, Output $15/1M (Sonnet)
MASTER_COST: Dict[str, float] = {
    "gpt-3.5-turbo": 0.00055,  # $0.50 input + $1.50 output → (500×0.5 + 200×1.5)/1M = $0.55/1K
    "gpt-4.1-mini": 0.00052,   # 推定値 (GPT-4o-miniベース)
    "gpt-4.1": 0.00650,        # $10 input + $30 output → (500×10 + 200×30)/1M = $6.50/1K
    "gpt-4o-mini": 0.000195,   # $0.15 input + $0.60 output → (500×0.15 + 200×0.60)/1M = $0.195/1K
    "gpt-4o": 0.0065,          # $5 input + $20 output → (500×5 + 200×20)/1M = $6.50/1K
    "o3": 0.0065,              # 推定値 (高性能モデルとしてGPT-4oベース)
    "o4-mini": 0.00143,        # 推定値
    "deepseek-v3": 0.000355,
    "gemini-2.0-flash": 0.00013,
    "gemini-2.0-flash-lite": 0.0000975,
    "gemini-2.5-flash-preview-04-17": 0.000195,
    "gemini-2.5-pro-preview-05-06": 0.002625,
    "claude-opus-4-1-20250805": 0.0225,
    "claude-opus-4-20250514": 0.0225,
    "claude-sonnet-4-20250514": 0.0045,
    "gpt-5-mini(medium)": 0.000525,
    "gpt-5-mini(minimal)": 0.000525,
    "gpt-5-nano(minimal)": 0.000105,
    "gpt-5(minimal)": 0.002625,
    "gpt-5(medium)": 0.002625,
}

# モデルごとの基本色。matplotlib の named color もしくは hex
# 同一モデルではバージョンが違っても同じ色で塗り、透明度で区別する。
COLOR_MAP: Dict[str, str] = {
    "gpt-3.5-turbo": "palegreen",
    "gpt-4.1-mini": "lightgreen",
    "gpt-4.1": "mediumspringgreen",
    "gpt-4o-mini": "springgreen",
    "gpt-4o": "mediumseagreen",
    "o4-mini": "springgreen",
    "o3": "cyan",
    "deepseek-v3": "pink",
    "gemini-2.0-flash": "lightgray",
    "gemini-2.0-flash-lite": "silver",
    "gemini-2.5-flash-preview-04-17": "darkgray",
    "gemini-2.5-pro-preview-05-06": "gray",
    "claude-opus-4-1-20250805": "mediumpurple",
    "claude-opus-4-20250514": "blueviolet",
    "claude-sonnet-4-20250514": "mediumorchid",
    "gpt-5-mini(medium)": "limegreen",
    "gpt-5-mini(minimal)": "forestgreen",
    "gpt-5-nano(minimal)": "darkgreen",
    "gpt-5(minimal)": "seagreen",
    "gpt-5(medium)": "green",
}

# ---------------------------------------------------------------------------
# バージョン別の結果 (CER, mean_elapsed_sec)
# ---------------------------------------------------------------------------
# v2.4.0
DATA_V24: Dict[str, Dict[str, float]] = {
    "gpt-3.5-turbo": {"cer": 0.645358, "elapsed": 0.827224},
    "gemini-2.0-flash-lite": {"cer": 0.322322, "elapsed": 0.590890},
    "gemini-2.0-flash": {"cer": 0.192571, "elapsed": 0.609305},
    "gemini-2.5-flash-preview-04-17": {"cer": 0.088799, "elapsed": 4.177074},
    "gemini-2.5-pro-preview-05-06": {"cer": 0.061644, "elapsed": 17.666037},
    "gpt-4.1-mini": {"cer": 0.308427, "elapsed": 0.979534},
    "gpt-4.1": {"cer": 0.117064, "elapsed": 1.276582},
    "gpt-4o-mini": {"cer": 0.514936, "elapsed": 1.059017},
    "gpt-4o": {"cer": 0.130022, "elapsed": 0.983554},

    "deepseek-v3": {"cer": 0.296198, "elapsed": 5.139222},
    "o3":           {"cer": 0.077915, "elapsed": 12.771444},
    "o4-mini":     {"cer": 0.196260, "elapsed": 14.309153},
    "claude-opus-4-1-20250805": {"cer": 0.119479, "elapsed": 2.616859},
    "claude-opus-4-20250514": {"cer": 0.110761, "elapsed": 2.476723},
    "claude-sonnet-4-20250514": {"cer": 0.125210, "elapsed": 2.790457},
    "gpt-5-mini(medium)": {"cer": 0.250286, "elapsed": 15.645264},
    "gpt-5-mini(minimal)": {"cer": 0.359281, "elapsed": 1.278602},
    "gpt-5-nano(minimal)": {"cer": 0.894475, "elapsed": 1.140173},
    "gpt-5(minimal)": {"cer": 0.127556, "elapsed": 1.858604},
    "gpt-5(medium)": {"cer": 0.046352, "elapsed": 34.859140},
}

# ---------------------------------------------------------------------------
# 描画処理
# ---------------------------------------------------------------------------

# 適用する円のサイズスケール
SCALE = 50  # size = elapsed * SCALE


def plot_version(
    data: Dict[str, Dict[str, float]],
    version_label: str,
    alpha: float,
    face_filled: bool,
    zorder: int,
    annotate: bool = True,
):
    """一つのバージョンの散布図を描く

    annotate が True のときのみモデル名ラベルを表示する。
    """
    for model, metrics in data.items():
        cost = MASTER_COST.get(model)
        if cost is None:
            # コスト情報が無ければ描かない
            continue

        cer_pct = metrics["cer"] * 100  # 0.25 -> 25%
        size = metrics["elapsed"] * SCALE
        color = COLOR_MAP.get(model, "blue")

        if face_filled:
            # v2.4.0 (濃い塗りつぶし)
            plt.scatter(
                cost,
                cer_pct,
                s=size,
                color=color,
                alpha=alpha,
                label=version_label if model == next(iter(data)) else None,  # 最初の一度だけ凡例
                zorder=zorder,
            )
        else:
            # v2.3.0 (薄い枠線のみ or塗り無し)
            plt.scatter(
                cost,
                cer_pct,
                s=size,
                facecolors="none",
                edgecolors=color,
                alpha=alpha,
                label=version_label if model == next(iter(data)) else None,
                zorder=zorder,
            )

        # モデル名の注釈
        if annotate:
            plt.annotate(
                model,
                xy=(cost, cer_pct),
                xytext=(5, 5),
                textcoords="offset points",
                ha="left",
                va="bottom",
                clip_on=False,
                fontsize=8,
            )




def build_legend():
    """平均応答時間の凡例を描画"""

    # 平均応答時間凡例（円サイズ）
    seconds_legend = [1, 5, 10]
    legend_size_handles: List[Line2D] = []
    for sec in seconds_legend:
        size = sec * SCALE
        legend_size_handles.append(
            Line2D(
                [],
                [],
                marker="o",
                color="gray",
                linestyle="None",
                markersize=size ** 0.5,  # matplotlib は points 単位。scatter の s は points^2
                label=f"{sec} sec",
            )
        )

    # drawing
    ax = plt.gca()
    ax.legend(
        handles=legend_size_handles,
        loc="upper right",
        title="Mean Response Time",
        framealpha=0.8,
    )


def main():
    parser = argparse.ArgumentParser(description="Plot error rate vs cost (v2.4.0)")
    parser.add_argument("-o", "--output", help="Output image file path")
    parser.add_argument("--range", type=int, default=0, choices=[0, 1],
                        help="Axis range mode: 0=default (0-70%%, auto x), 1=zoomed (0-40%%, 0-0.008$)")
    args = parser.parse_args()

    plt.figure(figsize=(8, 6))

    # v2.4.0 — 濃い塗りつぶし円（ラベルあり）
    plot_version(
        DATA_V24,
        "v2.4.0",
        alpha=1.0,
        face_filled=True,
        zorder=3,
        annotate=True,
    )


    # 軸設定
    plt.xlabel("Cost Per Request ($)")
    plt.ylabel("Error Rate (%)")
    
    # タイトル設定
    if args.range == 1:
        plt.title("Error Rate vs Cost of LLM Model (v2.4.0) - Zoomed")
    else:
        plt.title("Error Rate vs Cost of LLM Model (v2.4.0)")
    
    plt.grid(True, which="both", linestyle=":", linewidth=0.5)

    # 軸範囲設定
    if args.range == 0:
        # デフォルト範囲
        plt.ylim(0, 70)
        plt.margins(x=0.05)
    elif args.range == 1:
        # ズーム範囲
        plt.ylim(0, 40)
        plt.xlim(0, 0.008)

    # 座標スケールを線形のまま維持（必要に応じて変更可）

    # 凡例
    build_legend()

    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=300, bbox_inches="tight")
    else:
        plt.show()


if __name__ == "__main__":
    main()
