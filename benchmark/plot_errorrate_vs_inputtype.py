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
import sys
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
import numpy as np

# aggregate_results.pyのsummarize関数をインポート
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from aggregate_results import summarize


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
    3つの入力タイプすべてのデータが存在するモデルのCERとレスポンス時間を取得する。
    aggregate_results.pyと同じ方法でmean_cerとmean_elapsed_sec_p95を計算する。

    Returns:
        List of dicts with keys: model, romaji, hiragana, katakana,
                                 romaji_time, hiragana_time, katakana_time
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
            # CERとレスポンス時間を計算
            with open(romaji_file, encoding='utf-8') as f:
                romaji_data = json.load(f)
                romaji_cer = calculate_mean_cer(romaji_data)
                (_, _, _, _, _, _, romaji_time) = summarize(romaji_data)

            with open(h_file, encoding='utf-8') as f:
                hiragana_data = json.load(f)
                hiragana_cer = calculate_mean_cer(hiragana_data)
                (_, _, _, _, _, _, hiragana_time) = summarize(hiragana_data)

            with open(katakana_file, encoding='utf-8') as f:
                katakana_data = json.load(f)
                katakana_cer = calculate_mean_cer(katakana_data)
                (_, _, _, _, _, _, katakana_time) = summarize(katakana_data)

            models_with_variants.append({
                "model": model_name,
                "romaji": romaji_cer,
                "hiragana": hiragana_cer,
                "katakana": katakana_cer,
                "romaji_time": romaji_time,
                "hiragana_time": hiragana_time,
                "katakana_time": katakana_time
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
    入力タイプ別のエラー率とレスポンス時間の比較グラフを生成する。

    Args:
        models_data: モデルとCER、レスポンス時間のデータ
        output_path: 出力ファイルパス
        figsize: 図のサイズ
    """
    # データの準備
    models = [d['model'] for d in models_data]
    romaji_cers = [d['romaji'] * 100 for d in models_data]  # パーセント表示
    hiragana_cers = [d['hiragana'] * 100 for d in models_data]
    katakana_cers = [d['katakana'] * 100 for d in models_data]

    # 3つの入力方式のレスポンス時間の平均を計算
    avg_times = [(d['romaji_time'] + d['hiragana_time'] + d['katakana_time']) / 3
                 for d in models_data]

    # グラフの設定
    fig, ax1 = plt.subplots(figsize=figsize)

    x = np.arange(len(models))
    width = 0.25

    # 棒グラフの作成（ローマ字、カタカナ、ひらがなの順）- 左Y軸
    bars1 = ax1.bar(x - width, romaji_cers, width, label='Romaji Input (Error Rate)',
                   color='tab:blue', alpha=0.7)
    bars2 = ax1.bar(x, katakana_cers, width, label='Katakana Input (Error Rate)',
                   color='tab:green', alpha=0.7)
    bars3 = ax1.bar(x + width, hiragana_cers, width, label='Hiragana Input (Error Rate)',
                   color='tab:red', alpha=0.7)

    # 左Y軸のラベルと設定
    ax1.set_xlabel('Model', fontsize=12, fontweight='bold')
    ax1.set_ylabel('Character Error Rate (%)', fontsize=12, fontweight='bold', color='black')
    ax1.set_xticks(x)
    ax1.set_xticklabels(models, rotation=45, ha='right')
    ax1.set_ylim(bottom=0)
    ax1.tick_params(axis='y', labelcolor='black')
    ax1.grid(axis='y', alpha=0.3, linestyle='--')

    # 右Y軸の作成（レスポンス時間用）
    ax2 = ax1.twinx()

    # 折れ線グラフの作成（平均レスポンス時間）- 右Y軸
    line = ax2.plot(x, avg_times, marker='o', linestyle='-',
                    color='black', linewidth=2.5, markersize=7,
                    label='Average Response Time', zorder=5)

    # 右Y軸のラベルと設定
    ax2.set_ylabel('Mean Response Time (sec, 95th percentile)',
                   fontsize=12, fontweight='bold', color='black')
    ax2.set_ylim(bottom=0)
    ax2.tick_params(axis='y', labelcolor='black')

    # タイトル
    ax1.set_title('Error Rate and Response Time Comparison by Input Type\n' +
                  '(Bars: Error Rate by Input Type, Line: Average Response Time)',
                  fontsize=14, fontweight='bold', pad=20)

    # 凡例の統合
    lines_labels = [ax1.get_legend_handles_labels(), ax2.get_legend_handles_labels()]
    lines, labels = [sum(lol, []) for lol in zip(*lines_labels)]

    # 凡例を表示（エラー率3つ + レスポンス時間1つ = 計4つ）
    ax1.legend(lines, labels, loc='upper left', fontsize=9)

    # レイアウト調整
    plt.tight_layout()

    # 保存
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    print(f"Graph saved to: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Plot error rate and response time comparison by input type"
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
    parser.add_argument(
        "-s", "--size",
        default="1000x600",
        help="Figure size in pixels (default: 1000x600)"
    )

    args = parser.parse_args()

    # サイズをパース
    if 'x' in args.size:
        width_px, height_px = map(int, args.size.split('x'))
        figsize = (width_px / 100, height_px / 100)  # 100 DPI換算
    else:
        figsize = (10, 6)

    # データ読み込み
    print(f"Loading data from {args.result_dir}...")
    models_data = load_model_data(args.result_dir)
    print(f"Found {len(models_data)} models with all 3 input types")

    # グラフ生成
    print("Generating graph...")
    plot_errorrate_comparison(models_data, args.output, figsize)

    # 統計情報を表示
    print("\n=== Error Rate and Response Time Summary ===")
    print(f"{'Model':<35} {'Romaji':>15} {'Hiragana':>15} {'Katakana':>15} {'Avg Time':>10}")
    print(f"{'':35} {'CER% / Time(s)':>15} {'CER% / Time(s)':>15} {'CER% / Time(s)':>15} {'(sec)':>10}")
    print("-" * 105)
    for d in models_data:
        avg_time = (d['romaji_time'] + d['hiragana_time'] + d['katakana_time']) / 3
        print(f"{d['model']:<35} "
              f"{d['romaji']*100:>6.1f}% / {d['romaji_time']:>5.2f}s "
              f"{d['hiragana']*100:>6.1f}% / {d['hiragana_time']:>5.2f}s "
              f"{d['katakana']*100:>6.1f}% / {d['katakana_time']:>5.2f}s "
              f"{avg_time:>9.2f}s")


if __name__ == "__main__":
    main()
