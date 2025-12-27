# -*- coding: utf-8 -*-
"""plot_errorrate_vs_inputtype_hiragana.py

Ver4.0以降を利用するユーザー向けのグラフ。
hiragana_inputのデータのみを表示する。

- 左Y軸: エラー率（CER）- 棒グラフ
- 右Y軸: レスポンス時間 - 折れ線グラフ
"""

from __future__ import annotations

import argparse
import glob
import json
import os
import sys
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
import matplotlib.patches as patches
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
    hiragana入力のデータが存在するモデルのCERとレスポンス時間を取得する。

    Returns:
        List of dicts with keys: model, hiragana_cer, hiragana_time
    """
    hiragana_files = glob.glob(f"{result_dir}/*_hiragana.json")
    models_data = []

    for h_file in hiragana_files:
        base_name = h_file.replace("_hiragana.json", "")
        model_name = base_name.replace(f"{result_dir}/", "")

        # CERとレスポンス時間を計算
        with open(h_file, encoding='utf-8') as f:
            hiragana_data = json.load(f)
            hiragana_cer = calculate_mean_cer(hiragana_data)
            (_, _, _, _, _, _, hiragana_time) = summarize(hiragana_data)

        models_data.append({
            "model": model_name,
            "hiragana_cer": hiragana_cer,
            "hiragana_time": hiragana_time
        })

    # ひらがな入力のエラー率でソート（小さい順）
    models_data.sort(key=lambda x: x['hiragana_cer'])

    return models_data


def plot_hiragana_errorrate_and_responsetime(
    models_data: List[Dict[str, float]],
    output_path: str,
    figsize: Tuple[int, int] = (10, 6)
):
    """
    hiragana入力のエラー率とレスポンス時間のグラフを生成する。

    Args:
        models_data: モデルとCER、レスポンス時間のデータ
        output_path: 出力ファイルパス
        figsize: 図のサイズ
    """
    # データの準備
    models = [d['model'] for d in models_data]
    hiragana_cers = [d['hiragana_cer'] * 100 for d in models_data]  # パーセント表示
    hiragana_times = [d['hiragana_time'] for d in models_data]

    # グラフの設定
    fig, ax1 = plt.subplots(figsize=figsize)

    x = np.arange(len(models))
    width = 0.6  # 1種類だけなので幅を広く

    # 棒グラフの作成（ひらがな入力）- 左Y軸
    bars = ax1.bar(x, hiragana_cers, width, label='Hiragana Input (Error Rate)',
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

    # 折れ線グラフの作成（レスポンス時間）- 右Y軸
    line = ax2.plot(x, hiragana_times, marker='o', linestyle='-',
                    color='black', linewidth=2.5, markersize=7,
                    label='Response Time', zorder=5)

    # 右Y軸のラベルと設定
    ax2.set_ylabel('Mean Response Time (sec, 95th percentile)',
                   fontsize=12, fontweight='bold', color='black')
    ax2.set_ylim(bottom=0)
    ax2.tick_params(axis='y', labelcolor='black')

    # タイトル
    ax1.set_title('Error Rate and Response Time for Hiragana Input (Ver4.0+)\n' +
                  '(Bar: Error Rate, Line: Response Time)',
                  fontsize=14, fontweight='bold', pad=20)

    # 凡例の統合
    lines_labels = [ax1.get_legend_handles_labels(), ax2.get_legend_handles_labels()]
    lines, labels = [sum(lol, []) for lol in zip(*lines_labels)]

    # 凡例を表示
    ax1.legend(lines, labels, loc='upper left', fontsize=10)

    # レスポンス時間1.2秒以内かつエラー率20%以内のモデルを青枠で囲む
    # 条件を満たすモデルのインデックスを抽出
    target_indices = []
    for i, d in enumerate(models_data):
        if d['hiragana_time'] <= 1.2 and d['hiragana_cer'] <= 0.20:
            target_indices.append(i)

    # 連続した範囲ごとにグループ化して青枠を描画
    if target_indices:
        # 連続した範囲を見つける
        ranges = []
        start = target_indices[0]
        end = target_indices[0]

        for i in range(1, len(target_indices)):
            if target_indices[i] == end + 1:
                end = target_indices[i]
            else:
                ranges.append((start, end))
                start = target_indices[i]
                end = target_indices[i]
        ranges.append((start, end))

        # 各範囲に青枠を描画
        for start_idx, end_idx in ranges:
            # 範囲の左端と右端を計算（バーの幅を考慮）
            x_left = start_idx - width/2 - 0.1
            x_right = end_idx + width/2 + 0.1
            rect_width = x_right - x_left

            # Y軸の範囲（エラー率の最大値を取得）
            y_max = max(hiragana_cers[start_idx:end_idx+1])
            rect_height = y_max + 5  # 少し余裕を持たせる

            # 青い四角形を描画
            rect = patches.Rectangle(
                (x_left, 0), rect_width, rect_height,
                linewidth=3, edgecolor='blue', facecolor='none',
                linestyle='-', zorder=10
            )
            ax1.add_patch(rect)

    # レイアウト調整
    plt.tight_layout()

    # 保存
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    print(f"Graph saved to: {output_path}")
    print(f"\nHighlighted models (Response time ≤ 1.2s AND Error rate ≤ 20%):")
    for idx in target_indices:
        d = models_data[idx]
        print(f"  - {d['model']}: {d['hiragana_cer']*100:.1f}%, {d['hiragana_time']:.2f}s")


def main():
    parser = argparse.ArgumentParser(
        description="Plot error rate and response time for hiragana input (Ver4.0+)"
    )
    parser.add_argument(
        "-o", "--output",
        default="../images/plot_errorrate_vs_inputtype_hiragana_1000x600.png",
        help="Output file path (default: ../images/plot_errorrate_vs_inputtype_hiragana_1000x600.png)"
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
    print(f"Loading hiragana input data from {args.result_dir}...")
    models_data = load_model_data(args.result_dir)
    print(f"Found {len(models_data)} models with hiragana input data")

    # グラフ生成
    print("Generating graph...")
    plot_hiragana_errorrate_and_responsetime(models_data, args.output, figsize)

    # 統計情報を表示
    print("\n=== Hiragana Input: Error Rate and Response Time Summary ===")
    print(f"{'Model':<35} {'CER (%)':>10} {'Time (sec)':>12}")
    print("-" * 60)
    for d in models_data:
        print(f"{d['model']:<35} {d['hiragana_cer']*100:>9.1f}% {d['hiragana_time']:>11.2f}s")


if __name__ == "__main__":
    main()
