#!/usr/bin/env python3
"""Render the recorded Jev experiment as PNG and SVG; makes no API calls."""

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager


ROOT = Path(__file__).resolve().parent


def main():
    data = json.loads((ROOT / "plot_data.json").read_text(encoding="utf-8"))
    available = {f.name for f in font_manager.fontManager.ttflist}
    font = next((name for name in ("Hiragino Sans", "Noto Sans CJK JP", "IPAexGothic")
                 if name in available), "DejaVu Sans")
    plt.rcParams.update({
        "font.family": font, "font.size": 12, "axes.unicode_minus": False,
        "svg.fonttype": "path", "axes.spines.top": False,
        "axes.spines.right": False, "axes.spines.left": False,
        "axes.edgecolor": "#cbd5e1", "text.color": "#172b4d",
        "axes.labelcolor": "#475569", "xtick.color": "#475569",
        "ytick.color": "#172b4d",
    })
    blue, amber, coral, gray = "#2878b5", "#e9b44c", "#c86658", "#e2e8f0"
    methods = data["methods"]
    labels = [m["label"] for m in methods]
    y = list(range(len(methods)))
    fig, axes = plt.subplots(2, 2, figsize=(16, 10.8))
    fig.patch.set_facecolor("#f6f8fb")
    fig.suptitle("Jev の変換トリガー評価", x=0.055, y=0.965,
                 ha="left", fontsize=25, fontweight="bold")
    fig.text(0.055, 0.922, "自然文セット 33ケース・627打鍵  |  jev-1.13.0 / 質問文 v2  |  2026-10-03",
             fontsize=12, color="#64748b")

    def style(ax, title, xmax):
        ax.set_facecolor("white")
        ax.set_title(title, loc="left", fontsize=16, pad=20, fontweight="bold")
        ax.set_xlim(0, xmax)
        ax.grid(axis="x", color="#edf1f5", zorder=0)
        ax.set_axisbelow(True)
        ax.tick_params(axis="y", length=0)

    ax = axes[0, 0]
    style(ax, "① 好ましい変換位置を拾えた数", 16)
    hits = [m["preferred"] for m in methods]
    ax.barh(y, hits, color=blue, height=0.48)
    ax.barh(y, [14 - n for n in hits], left=hits, color=gray, height=0.48)
    for i, n in enumerate(hits):
        ax.text(14.3, i, f"{n}/14", va="center", fontsize=13)
    ax.set_yticks(y, labels)
    ax.invert_yaxis()
    ax.set_xticks([0, 4, 8, 12, 14])
    ax.set_xlabel("好ましい位置の件数（灰色は見逃し）")

    ax = axes[0, 1]
    style(ax, "② 早めに発動した数", 20)
    early = [m["acceptable_early"] for m in methods]
    wait = [m["wait_preferred"] for m in methods]
    ax.barh(y, early, color=amber, height=0.48, label="許容できる早めの発動")
    ax.barh(y, wait, left=early, color=coral, height=0.48, label="ラベル上は待ちたい位置")
    for i, (a, b) in enumerate(zip(early, wait)):
        for start, n in ((0, a), (a, b)):
            if n:
                ax.text(start + n / 2, i, str(n), va="center", ha="center", color="#172b4d")
        if not a + b:
            ax.text(0.3, i, "0", va="center")
    ax.set_yticks(y, labels)
    ax.invert_yaxis()
    ax.set_xticks([0, 5, 10, 15, 20])
    ax.set_xlabel("発動回数（①とは別の入力位置）")
    ax.legend(loc="lower left", bbox_to_anchor=(0, -0.33), ncol=1,
              frameon=False, fontsize=11)

    ax = axes[1, 0]
    style(ax, "③ 英語入力中に発動した数", 9)
    counts = [m["english_triggers"] for m in methods]
    ax.barh(y, counts, color=coral, height=0.48)
    for i, m in enumerate(methods):
        ax.text(m["english_triggers"] + 0.2, i,
                f"{m['english_triggers']}回（{m['english_affected']}/10英文）", va="center")
    ax.set_yticks(y, labels)
    ax.invert_yaxis()
    ax.set_xticks([0, 2, 4, 6, 8])
    ax.set_xlabel("英文10ケース・240打鍵での発動回数")

    ax = axes[1, 1]
    style(ax, "④ Jev の通信時間と概算費用", 400)
    vals = [data["latency_p50_ms"], data["latency_p95_ms"]]
    ax.barh([0, 1], vals, color=[blue, "#83b6d9"], height=0.42)
    for i, value in enumerate(vals):
        ax.text(value + 8, i, f"{value:.1f} ms", va="center", fontsize=13)
    ax.set_yticks([0, 1], ["中央値（p50）", "95%点（p95）"])
    ax.set_ylim(2.2, -0.65)
    ax.set_xlabel("API応答時間（ms）")
    ax.text(0, 1.9, f"595回のAPI呼び出しで 約 ${data['estimated_usd']:.4f}",
            fontsize=14, fontweight="bold", va="center")

    fig.subplots_adjust(left=0.13, right=0.965, top=0.855, bottom=0.14,
                        wspace=0.4, hspace=0.64)
    fig.text(0.055, 0.055,
             "各打鍵は独立に判定。現行ルールはLisp本体の近似。英文0回は、この10ケース内の観測結果です。\n"
             "費用は公表単価による推計。『待ちたい位置』には好ましさの注釈を再確認する余地があります。",
             fontsize=10.5, color="#64748b", linespacing=1.7)
    for ext in ("png", "svg"):
        output = ROOT / f"jev_benchmark_summary.{ext}"
        fig.savefig(output, dpi=160, facecolor=fig.get_facecolor())
        print(output)
    plt.close(fig)


if __name__ == "__main__":
    main()
