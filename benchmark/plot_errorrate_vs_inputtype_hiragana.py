# -*- coding: utf-8 -*-
"""plot_errorrate_vs_inputtype_hiragana.py

ひらがな入力モードのエラー率（CER）をモデル別に比較する棒グラフを生成する。
ひらがな入力データが存在するモデルのみを対象とする。
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


def load_hiragana_data(result_dir: str = "result_ver2.4.0") -> List[Dict[str, float]]:
    """
    ひらがな入力モードのデータが存在するモデルのCERを取得する。
    aggregate_results.pyと同じ方法でmean_cerを計算する。

    Returns:
        List of dicts with keys: model, hiragana
    """
    hiragana_files = glob.glob(f"{result_dir}/*_hiragana.json")
    models_data = []

    for h_file in hiragana_files:
        base_name = h_file.replace("_hiragana.json", "")
        model_name = base_name.replace(f"{result_dir}/", "")

        # CERを計算（aggregate_results.pyと同じ方法）
        with open(h_file, encoding='utf-8') as f:
            hiragana_data = json.load(f)
            hiragana_cer = calculate_mean_cer(hiragana_data)

        models_data.append({
            "model": model_name,
            "hiragana": hiragana_cer
        })

    # ひらがな入力のエラー率でソート（小さい順）
    models_data.sort(key=lambda x: x['hiragana'])

    return models_data


def plot_hiragana_errorrate(
    models_data: List[Dict[str, float]],
    output_path: str,
    figsize: Tuple[int, int] = (14, 8)
):
    """
    ひらがな入力モードのエラー率グラフを生成する。

    Args:
        models_data: モデルとCERのデータ
        output_path: 出力ファイルパス
        figsize: 図のサイズ
    """
    # データの準備
    models = [d['model'] for d in models_data]
    hiragana_cers = [d['hiragana'] * 100 for d in models_data]  # パーセント表示

    # グラフの設定
    fig, ax = plt.subplots(figsize=figsize)

    x = np.arange(len(models))
    width = 0.6

    # 棒グラフの作成（ひらがな入力のみ）
    # 色はplot_errorrate_vs_paramsize.pyと同じ設定（赤色）
    bars = ax.bar(x, hiragana_cers, width, label='Hiragana Input',
                  color='tab:red', alpha=0.8)

    # ラベルと装飾
    ax.set_xlabel('Model', fontsize=12, fontweight='bold')
    ax.set_ylabel('Character Error Rate (%)', fontsize=12, fontweight='bold')
    ax.set_title('Error Rate - Hiragana Input Mode',
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
        description="Plot error rate for hiragana input mode"
    )
    parser.add_argument(
        "-o", "--output",
        default="../images/plot_errorrate_vs_inputtype_hiragana.png",
        help="Output file path (default: ../images/plot_errorrate_vs_inputtype_hiragana.png)"
    )
    parser.add_argument(
        "-d", "--result-dir",
        default="result_ver2.4.0",
        help="Result directory (default: result_ver2.4.0)"
    )

    args = parser.parse_args()

    # データ読み込み
    print(f"Loading hiragana data from {args.result_dir}...")
    models_data = load_hiragana_data(args.result_dir)
    print(f"Found {len(models_data)} models with hiragana input mode")

    # グラフ生成
    print("Generating graph...")
    plot_hiragana_errorrate(models_data, args.output)

    # 統計情報を表示
    print("\n=== Hiragana Error Rate Summary (%) ===")
    print(f"{'Model':<35} {'Hiragana':>8}")
    print("-" * 50)
    for d in models_data:
        print(f"{d['model']:<35} {d['hiragana']*100:>7.1f}%")


if __name__ == "__main__":
    main()
