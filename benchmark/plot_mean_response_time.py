#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""plot_mean_response_time.py

グラフ構成
============
1. X 軸 (主軸, ax1)
   • 各モデルの平均応答時間 mean_elapsed_sec (v2.4.0)
     -> 横向きバーで表示

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
from typing import List

import matplotlib.pyplot as plt

# ---------------------------------------------------------------------------
# 元データ（aggregate_results.py の出力を転記）
# ---------------------------------------------------------------------------

MODELS: List[str] = [
    "gpt-3.5-turbo",
    "gpt-4.1-mini",
    "gpt-4.1",
    "gpt-4o-mini",
    "gpt-4o",
    "o4-mini",
    "deepseek-v3",
    "gemini-2.0-flash",
    "gemini-2.0-flash-lite",
    "gemini-2.5-flash-preview-04-17",
    "gemini-2.5-pro-preview-05-06",
    "o3",
]

# 平均応答時間 (sec) v2.4.0
MEAN_ELAPSED_V24 = [
    0.827224,
    0.979534,
    1.276582,
    1.059017,
    0.983554,
    14.309153,
    5.139222,
    0.609305,
    0.590890,
    4.177074,
    17.666037,
    12.771444,
]

MEAN_CER_V24 = [
    0.645358,
    0.308427,
    0.117064,
    0.514936,
    0.130022,
    0.196260,
    0.296198,
    0.192571,
    0.322322,
    0.088799,
    0.061644,
    0.077915,
]

# カラー (plot_errorrate_vs_cost.py と合わせている)
BAR_COLORS = [
    "palegreen",
    "lightgreen",
    "mediumspringgreen",
    "springgreen",
    "mediumseagreen",
    "springgreen",
    "pink",
    "lightgray",
    "silver",
    "darkgray",
    "gray",
    "cyan",
]

# ---------------------------------------------------------------------------
# 並び替え : v2.4.0 の mean_elapsed_sec が長い順
sorted_idx = sorted(range(len(MODELS)), key=lambda i: MEAN_ELAPSED_V24[i], reverse=True)

models_sorted = [MODELS[i] for i in sorted_idx]
elapsed_v24_sorted = [MEAN_ELAPSED_V24[i] for i in sorted_idx]
cer_v24_sorted = [MEAN_CER_V24[i] for i in sorted_idx]
bar_colors_sorted = [BAR_COLORS[i] for i in sorted_idx]

# バー中心の y 座標を後で使うために計算する

# ---------------------------------------------------------------------------
# 描画
# ---------------------------------------------------------------------------

fig, ax1 = plt.subplots(figsize=(10, 6))

# -- v2.4.0: 横棒バー
bars = ax1.barh(models_sorted, elapsed_v24_sorted, color=bar_colors_sorted)

ax1.set_xlabel("Mean Response Time (sec)")
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

# -- バーに数値を注釈（v2.3.0）
# -- バーに数値を注釈（v2.4.0）
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

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description="Plot mean response time & error rate (v2.3.0 / v2.4.0)")
    parser.add_argument("-o", "--output", help="Output image file path")
    args = parser.parse_args()

    if args.output:
        plt.savefig(args.output, dpi=300, bbox_inches="tight")
    else:
        plt.show()


if __name__ == "__main__":
    main()
