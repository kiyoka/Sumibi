# -*- coding: utf-8 -*-
"""plot_errorrate_vs_inputtype.py

入力タイプ別（romaji_direct_input、hiragana_input、katakana_input）の
エラー率（CER）を比較する棒グラフを生成する。

3つの入力タイプすべてのデータが存在するモデルのみを対象とする。
"""

from __future__ import annotations

import argparse
import glob
import json
import os
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
import numpy as np


def calculate_mean_cer(data: List[Dict]) -> float:
    """
    CERの平均値を計算する（aggregate_results.pyと同じ方法）。
    cerが1.0を超える場合は1.0に丸めてから平均を計算。

    Args:
        data: ベンチマーク結果のリスト

    Returns:
        mean_cer: CERの平均値
    """
    if not data:
        return 0.0

    cer_sum = 0.0
    for rec in data:
        cer = rec.get('cer', 0.0)
        # cerが1.0を超える場合は丸める（aggregate_results.pyと同じ）
        if cer > 1.0:
            cer = 1.0
        cer_sum += cer

    return cer_sum / len(data)


def load_model_data(result_dir: str = "result_ver2.4.0") -> List[Dict[str, float]]:
    """
    3つの入力タイプすべてのデータが存在するモデルのCERを取得する。
    aggregate_results.pyと同じ方法でmean_cerを計算する。

    Returns:
        List of dicts with keys: model, romaji, hiragana, katakana
    """
    hiragana_files = glob.glob(f"{result_dir}/*_hiragana.json")
    models_with_variants = []

    for h_file in hiragana_files:
        base_name = h_file.replace("_hiragana.json", "")
        model_name = base_name.replace(f"{result_dir}/", "")

        romaji_file = f"{base_name}.json"
        katakana_file = f"{base_name}_katakana.json"

        # 3つ全てが存在するか確認
        if os.path.exists(romaji_file) and os.path.exists(katakana_file):
            # CERを計算（aggregate_results.pyと同じ方法）
            with open(romaji_file, encoding='utf-8') as f:
                romaji_data = json.load(f)
                romaji_cer = calculate_mean_cer(romaji_data)

            with open(h_file, encoding='utf-8') as f:
                hiragana_data = json.load(f)
                hiragana_cer = calculate_mean_cer(hiragana_data)

            with open(katakana_file, encoding='utf-8') as f:
                katakana_data = json.load(f)
                katakana_cer = calculate_mean_cer(katakana_data)

            models_with_variants.append({
                "model": model_name,
                "romaji": romaji_cer,
                "hiragana": hiragana_cer,
                "katakana": katakana_cer
            })

    # ひらがな入力のエラー率でソート（小さい順）
    models_with_variants.sort(key=lambda x: x['hiragana'])

    return models_with_variants


def plot_errorrate_comparison(
    models_data: List[Dict[str, float]],
    output_path: str,
    figsize: Tuple[int, int] = (14, 8)
):
    """
    入力タイプ別のエラー率比較グラフを生成する。

    Args:
        models_data: モデルとCERのデータ
        output_path: 出力ファイルパス
        figsize: 図のサイズ
    """
    # データの準備
    models = [d['model'] for d in models_data]
    romaji_cers = [d['romaji'] * 100 for d in models_data]  # パーセント表示
    hiragana_cers = [d['hiragana'] * 100 for d in models_data]
    katakana_cers = [d['katakana'] * 100 for d in models_data]

    # グラフの設定
    fig, ax = plt.subplots(figsize=figsize)

    x = np.arange(len(models))
    width = 0.25

    # 棒グラフの作成（ローマ字、カタカナ、ひらがなの順）
    # 色はplot_errorrate_vs_paramsize.pyと同じ設定
    bars1 = ax.bar(x - width, romaji_cers, width, label='Romaji Input',
                   color='tab:blue', alpha=0.8)
    bars2 = ax.bar(x, katakana_cers, width, label='Katakana Input',
                   color='tab:green', alpha=0.8)
    bars3 = ax.bar(x + width, hiragana_cers, width, label='Hiragana Input',
                   color='tab:red', alpha=0.8)

    # ラベルと装飾
    ax.set_xlabel('Model', fontsize=12, fontweight='bold')
    ax.set_ylabel('Character Error Rate (%)', fontsize=12, fontweight='bold')
    ax.set_title('Error Rate Comparison by Input Type\n(Blue: Romaji, Green: Katakana, Red: Hiragana)',
                 fontsize=14, fontweight='bold', pad=20)
    ax.set_xticks(x)
    ax.set_xticklabels(models, rotation=45, ha='right')
    ax.legend(loc='upper left', fontsize=10)
    ax.grid(axis='y', alpha=0.3, linestyle='--')

    # Y軸の範囲を0から開始
    ax.set_ylim(bottom=0)

    # レイアウト調整
    plt.tight_layout()

    # 保存
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    print(f"Graph saved to: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Plot error rate comparison by input type"
    )
    parser.add_argument(
        "-o", "--output",
        default="../images/plot_errorrate_vs_inputtype.png",
        help="Output file path (default: ../images/plot_errorrate_vs_inputtype.png)"
    )
    parser.add_argument(
        "-d", "--result-dir",
        default="result_ver2.4.0",
        help="Result directory (default: result_ver2.4.0)"
    )

    args = parser.parse_args()

    # データ読み込み
    print(f"Loading data from {args.result_dir}...")
    models_data = load_model_data(args.result_dir)
    print(f"Found {len(models_data)} models with all 3 input types")

    # グラフ生成
    print("Generating graph...")
    plot_errorrate_comparison(models_data, args.output)

    # 統計情報を表示
    print("\n=== Error Rate Summary (%) ===")
    print(f"{'Model':<35} {'Romaji':>8} {'Hiragana':>8} {'Katakana':>8} {'H/R Ratio':>10}")
    print("-" * 80)
    for d in models_data:
        ratio = (d['hiragana'] / d['romaji']) if d['romaji'] > 0 else 0
        print(f"{d['model']:<35} {d['romaji']*100:>7.1f}% {d['hiragana']*100:>7.1f}% "
              f"{d['katakana']*100:>7.1f}% {ratio:>9.1%}")


if __name__ == "__main__":
    main()
