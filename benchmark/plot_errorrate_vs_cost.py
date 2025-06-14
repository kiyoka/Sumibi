# -*- coding: utf-8 -*-
"""plot_errorrate_vs_cost.py

v2.3.0 と v2.4.0 の 2 つのベンチマーク結果を同一グラフに描画する。

要件（Issue #??）
1. バージョンごとに色の透明度を変えて区別する。
   ・v2.3.0: 同系色で α を落とした（薄い）円。
   ・v2.4.0: 同じ色だが α=1 の塗りつぶし円。
2. 同一モデルについては v2.3.0 → v2.4.0 の向きを示す矢印を描画する。

既存スクリプトはハードコードされた配列を扱っていたが、今回の要件を
満たすために以下の方針を採用した。

* “モデルごとのマスタ情報” (cost, カラー) を dict で保持。
* v2.3.0, v2.4.0 それぞれについて CER・平均応答時間を dict に格納。
* 共通処理で散布図を描いた後、両バージョンに存在するモデルだけ矢印。

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
# Text tokens per 1M tokens: Input $2.00, Cached input $0.50, Output $8.00 に基づき計算
MASTER_COST: Dict[str, float] = {
    "gpt-3.5-turbo": 0.00055,
    "gpt-4.1-mini": 0.00052,
    "gpt-4.1": 0.00260,
    "gpt-4o-mini": 0.000195,
    "gpt-4o": 0.00550,
    "o3": 0.00260,
    "o4-mini": 0.00143,
    "deepseek-v3": 0.000355,
    "gemini-2.0-flash": 0.00013,
    "gemini-2.0-flash-lite": 0.0000975,
    "gemini-2.5-flash-preview-04-17": 0.000195,
    "gemini-2.5-pro-preview-05-06": 0.002625,
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
}

# ---------------------------------------------------------------------------
# バージョン別の結果 (CER, mean_elapsed_sec)
# ---------------------------------------------------------------------------
# v2.3.0
DATA_V23: Dict[str, Dict[str, float]] = {
    "gpt-3.5-turbo": {"cer": 0.721777, "elapsed": 0.836779},
    "gpt-4.1-mini": {"cer": 0.430172, "elapsed": 0.908558},
    "gpt-4.1": {"cer": 0.218878, "elapsed": 0.985278},
    "gpt-4o-mini": {"cer": 0.735027, "elapsed": 0.912621},
    "gpt-4o": {"cer": 0.197426, "elapsed": 0.821817},
    "o4-mini": {"cer": 0.332583, "elapsed": 21.780880},
    "deepseek-v3": {"cer": 0.592557, "elapsed": 6.444244},
    "gemini-2.0-flash": {"cer": 0.266489, "elapsed": 0.651836},
    "gemini-2.0-flash-lite": {"cer": 0.356546, "elapsed": 0.709727},
    "gemini-2.5-flash-preview-04-17": {"cer": 0.137815, "elapsed": 6.640661},
    "gemini-2.5-pro-preview-05-06": {"cer": 0.093759, "elapsed": 28.210514},
}

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


def draw_improvement_arrows():
    """両バージョンに存在するモデルに矢印を描画"""
    common_models = set(DATA_V23) & set(DATA_V24)
    for model in common_models:
        p0 = (MASTER_COST[model], DATA_V23[model]["cer"] * 100)
        p1 = (MASTER_COST[model], DATA_V24[model]["cer"] * 100)

        color = COLOR_MAP.get(model, "blue")
        plt.annotate(
            "",
            xy=p1,
            xytext=p0,
            arrowprops=dict(arrowstyle="->", color=color, lw=1, alpha=0.8),
            zorder=1,
        )


def build_legend():
    """凡例を 2 つ (バージョン, 平均応答時間) にまとめて描画"""

    # バージョン凡例（マーカーの形状・透明度）
    legend_version_handles = [
        Line2D(
            [],
            [],
            marker="o",
            color="black",
            markersize=6,
            linestyle="None",
            markerfacecolor="none",
            alpha=0.4,
            label="v2.3.0",
        ),
        Line2D(
            [],
            [],
            marker="o",
            color="black",
            markersize=6,
            linestyle="None",
            label="v2.4.0",
        ),
    ]

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
    # ---------- Legend: Version ----------
    ax = plt.gca()
    first_legend = ax.legend(
        handles=legend_version_handles,
        loc="upper right",
        title="Version",
        framealpha=0.8,
    )
    # ---------- Legend: Mean Response Time ----------
    # 上記 legend と同じ右上領域に配置するが、少しだけ下にオフセットを入れる
    # bbox_to_anchor=(x, y) は (0,0) が左下, (1,1) が右上
    # y を 0.58 あたりに設定して少し下に下げる
    ax.add_artist(first_legend)

    ax.legend(
        handles=legend_size_handles,
        loc="upper right",
        bbox_to_anchor=(1, 0.62),  # y を少し下げて Version の下に重ねる
        title="Mean Response Time",
        framealpha=0.8,
    )


def main():
    parser = argparse.ArgumentParser(description="Plot error rate vs cost (v2.3.0 & v2.4.0)")
    parser.add_argument("-o", "--output", help="Output image file path")
    args = parser.parse_args()

    plt.figure(figsize=(8, 6))

    # v2.3.0 — 薄い枠円（ラベル無し）
    plot_version(
        DATA_V23,
        "v2.3.0",
        alpha=0.4,
        face_filled=False,
        zorder=2,
        annotate=False,
    )

    # v2.4.0 — 濃い塗りつぶし円（ラベルあり）
    plot_version(
        DATA_V24,
        "v2.4.0",
        alpha=1.0,
        face_filled=True,
        zorder=3,
        annotate=True,
    )

    # 改善矢印
    draw_improvement_arrows()

    # 軸設定
    plt.xlabel("Cost Per Request ($)")
    plt.ylabel("Error Rate (%)")
    plt.title("Error Rate vs Cost of LLM Model (v2.3.0 → v2.4.0)")
    plt.grid(True, which="both", linestyle=":", linewidth=0.5)

    # Y 軸 0–80 %
    plt.ylim(0, 80)
    # 余白 (x 軸)
    plt.margins(x=0.05)

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
