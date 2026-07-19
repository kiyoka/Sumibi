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
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from adjustText import adjust_text

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
    "gemini-2.5-flash": 0.00065,
    "gemini-2.5-pro": 0.001625,
    "gemini-3-pro-preview": 0.001625,  # 推定値 (gemini-2.5-proベース、プレビュー価格未公開)
    "gemini-3-flash-preview": 0.00085,  # $0.50 input + $3.00 output → (500×0.50 + 200×3.00)/1M = $0.85/1K
    "gemini-3.1-flash-lite-preview": 0.000425,  # $0.25 input + $1.50 output → (500×0.25 + 200×1.50)/1M = $0.425/1K
    "gemma-3-12b-it-qat": 0.0001,
    "claude-opus-4-1-20250805": 0.0225,
    "claude-opus-4-20250514": 0.0225,
    "claude-sonnet-4-20250514": 0.0045,
    "claude-sonnet-4-5-20250929": 0.0045,
    "gpt-5-mini": 0.000525,
    "gpt-5-nano": 0.000105,
    "gpt-5": 0.002625,
    "gpt-5.1": 0.002625,
    "gpt-5.2": 0.003675,  # $1.75 input + $14 output → (500×1.75 + 200×14)/1M = $3.675/1K (GPT-5.1より40%高い)
    "gpt-5.4": 0.004250,  # $2.50 input + $15 output → (500×2.50 + 200×15)/1M = $4.250/1K
    "gpt-5.5": 0.008500,  # $5.00 input + $30 output → (500×5.00 + 200×30)/1M = $8.500/1K
    "gpt-5.6-terra": 0.004250,  # $2.50 input + $15 output → (500×2.50 + 200×15)/1M = $4.250/1K
    "gpt-5.6-luna": 0.001700,   # $1.00 input + $6.00 output → (500×1.00 + 200×6.00)/1M = $1.700/1K
    "gpt-oss-120b(low)": 0.00196,
    "llm-jp-3.1-13b-instruct4": 0.0001,
    "llm-jp-3.1-8x13b-instruct4": 0.0002,
    "google_ime_native": 0.0,     # ローカルIME（無料）
    "google_ime_cgi": 0.0,        # Google CGI API（無料）
    "mozc": 0.0,                  # ローカルIME（無料）
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
    "gemini-2.5-flash": "darkgray",
    "gemini-2.5-pro": "gray",
    "gemini-3-pro-preview": "dimgray",
    "gemini-3-flash-preview": "steelblue",
    "gemini-3.1-flash-lite-preview": "lightsteelblue",
    "gemma-3-12b-it-qat": "wheat",
    "claude-opus-4-1-20250805": "mediumpurple",
    "claude-opus-4-20250514": "blueviolet",
    "claude-sonnet-4-20250514": "mediumorchid",
    "claude-sonnet-4-5-20250929": "orchid",
    "gpt-5-mini": "forestgreen",
    "gpt-5-nano": "darkgreen",
    "gpt-5": "seagreen",
    "gpt-5.1": "limegreen",
    "gpt-5.2": "chartreuse",
    "gpt-5.4": "lime",
    "gpt-5.5": "yellow",
    "gpt-5.6-terra": "gold",
    "gpt-5.6-luna": "khaki",
    "gpt-oss-120b(low)": "olive",
    "llm-jp-3.1-13b-instruct4": "coral",
    "llm-jp-3.1-8x13b-instruct4": "salmon",
    "google_ime_native": "darkorange",
    "google_ime_cgi": "gold",
    "mozc": "orange",
}

# ---------------------------------------------------------------------------
# バージョン別の結果 (CER, mean_elapsed_sec)
# ---------------------------------------------------------------------------
# v2.4.0 (updated with aggregate_results.py p95_elapsed_sec)
DATA_V24: Dict[str, Dict[str, float]] = {
    "gpt-3.5-turbo": {"cer": 0.645358, "elapsed": 1.344801},
    "gemini-2.0-flash-lite": {"cer": 0.330811, "elapsed": 0.944913},
    "gemini-2.0-flash": {"cer": 0.212067, "elapsed": 1.053327},
    "gemini-2.5-flash": {"cer": 0.109744, "elapsed": 14.765599},
    "gemini-2.5-pro": {"cer": 0.048225, "elapsed": 78.868088},
    "gemini-3-pro-preview": {"cer": 0.016123, "elapsed": 43.236321},
    "gemini-3-flash-preview": {"cer": 0.041088, "elapsed": 9.291274},
    "gemini-3.1-flash-lite-preview": {"cer": 0.132824, "elapsed": 3.103779},
    "gemma-3-12b-it-qat": {"cer": 0.730364, "elapsed": 2.018562},
    "gpt-4.1-mini": {"cer": 0.308427, "elapsed": 1.597734},
    "gpt-4.1": {"cer": 0.117064, "elapsed": 2.704047},
    "gpt-4o-mini": {"cer": 0.514936, "elapsed": 1.695342},
    "gpt-4o": {"cer": 0.130022, "elapsed": 1.710582},

    "deepseek-v3": {"cer": 0.296198, "elapsed": 7.947079},
    "o3":           {"cer": 0.077915, "elapsed": 30.028979},
    "o4-mini":     {"cer": 0.196260, "elapsed": 35.874159},
    "claude-opus-4-1-20250805": {"cer": 0.119479, "elapsed": 3.763010},
    "claude-opus-4-20250514": {"cer": 0.110761, "elapsed": 3.639393},
    "claude-sonnet-4-20250514": {"cer": 0.125210, "elapsed": 4.037896},
    "claude-sonnet-4-5-20250929": {"cer": 0.115957, "elapsed": 4.071400},
    "gpt-5-mini": {"cer": 0.343922, "elapsed": 2.572683},
    "gpt-5-nano": {"cer": 0.879506, "elapsed": 2.366005},
    "gpt-5": {"cer": 0.130744, "elapsed": 3.197243},
    "gpt-5.1": {"cer": 0.114609, "elapsed": 2.965748},
    "gpt-5.2": {"cer": 0.106882, "elapsed": 1.071368},
    "gpt-5.4": {"cer": 0.076598, "elapsed": 2.215574},
    "gpt-5.5": {"cer": 0.026855, "elapsed": 4.234000},
    "gpt-5.6-terra": {"cer": 0.073874, "elapsed": 0.985698},
    "gpt-5.6-luna": {"cer": 0.150871, "elapsed": 0.933539},
    "gpt-oss-120b(low)": {"cer": 0.591938, "elapsed": 17.565630},
    "llm-jp-3.1-13b-instruct4": {"cer": 0.914891, "elapsed": 2.577443},
    "llm-jp-3.1-8x13b-instruct4": {"cer": 0.735276, "elapsed": 12.738874},
    "google_ime_native": {"cer": 0.065977, "elapsed": 0.082499},
    "google_ime_cgi": {"cer": 0.061108, "elapsed": 0.296260},
    "mozc": {"cer": 0.081243, "elapsed": 0.035803},
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
    x_range: Tuple[float, float] = None,
    y_range: Tuple[float, float] = None,
):
    """一つのバージョンの散布図を描く

    annotate が True のときのみモデル名ラベルを表示する。
    x_range / y_range を指定すると、範囲外の点はラベルを付けない
    （散布図マーカー自体はプロットするが、adjustText の対象から外す）。
    ラベルは adjustText で重なりを回避する。
    戻り値: (texts, label_info) — label_info は [(text, marker_x, marker_y, color), ...] で、
    adjust_text 後にマーカーとラベルを結ぶ接続線をモデル色で描画するために使う。
    """
    texts: List = []
    label_info: List = []
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

        # モデル名の注釈（adjustText で後で位置調整するため、Text オブジェクトを蓄積）
        # 表示範囲外の点はラベルを付けない
        # ラベル色は円と同色にし、adjustText 後に描く接続線もモデル色にすることで
        # どのラベルがどの円に対応するか一目でわかるようにする
        if annotate:
            in_range = True
            if x_range is not None and not (x_range[0] <= cost <= x_range[1]):
                in_range = False
            if y_range is not None and not (y_range[0] <= cer_pct <= y_range[1]):
                in_range = False
            if in_range:
                text_color = _darken_for_readability(color)
                t = plt.text(cost, cer_pct, model, fontsize=8, color=text_color, clip_on=False)
                texts.append(t)
                label_info.append((t, cost, cer_pct, text_color))

    return texts, label_info


# ---------------------------------------------------------------------------
# 色ヘルパー
# ---------------------------------------------------------------------------
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
                        help="Axis range mode: 0=default (0-70%%, auto x), 1=zoomed (0-40%%, 0-0.010$)")
    args = parser.parse_args()

    plt.figure(figsize=(8, 6))

    # 軸範囲を先に決定してラベル配置に反映
    if args.range == 1:
        x_range = (0.0, 0.010)
        y_range = (0.0, 40.0)
    else:
        x_range = None  # 自動
        y_range = (0.0, 70.0)

    # v2.4.0 — 濃い塗りつぶし円（ラベルあり）
    texts, label_info = plot_version(
        DATA_V24,
        "v2.4.0",
        alpha=1.0,
        face_filled=True,
        zorder=3,
        annotate=True,
        x_range=x_range,
        y_range=y_range,
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
        plt.xlim(0, 0.010)

    # 座標スケールを線形のまま維持（必要に応じて変更可）

    # 凡例
    build_legend()

    # ラベルの重なりを adjustText で回避
    # 軸範囲を確定してから呼ぶ必要があるため、この位置で実行
    # adjustText の arrowprops は全ラベル共通なので使わず、後で自前で色付き接続線を描画する
    adjust_text(
        texts,
        expand_points=(1.4, 1.4),
        expand_text=(1.2, 1.2),
        force_text=(0.5, 0.5),
        force_points=(0.3, 0.3),
    )

    # マーカーと移動後のラベルを結ぶ接続線をモデル色で描画
    # （どのラベルがどの円に対応するか一目で分かるようにするため）
    ax = plt.gca()
    for t, marker_x, marker_y, color in label_info:
        text_x, text_y = t.get_position()
        # ラベルとマーカーの位置がほぼ同じなら線は不要
        # データ座標での距離ではなく、matplotlib の座標系で判定するのは複雑なので
        # データ座標での差分で十分（重ならないラベルは adjustText が離すはず）
        if abs(text_x - marker_x) < 1e-9 and abs(text_y - marker_y) < 1e-9:
            continue
        ax.plot(
            [marker_x, text_x],
            [marker_y, text_y],
            color=color,
            lw=0.6,
            alpha=0.7,
            zorder=2,
        )

    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=300, bbox_inches="tight")
    else:
        plt.show()


if __name__ == "__main__":
    main()
