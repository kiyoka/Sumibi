#!/usr/bin/env python3
"""
ベンチマーク結果の棒グラフ描画ツール

resultsディレクトリのJSONファイルを読み込んで、
モデル別のLLM精度、Mozc精度、改善率を棒グラフで表示します。
"""

import json
import os
import sys
from pathlib import Path
from typing import Dict, List
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

# 日本語フォント設定
plt.rcParams['font.family'] = ['DejaVu Sans', 'Hiragino Sans', 'Yu Gothic', 'Meiryo', 'Takao', 'IPAexGothic', 'IPAPGothic', 'VL PGothic', 'Noto Sans CJK JP']


def load_benchmark_results(results_dir: str = "results") -> Dict[str, Dict]:
    """resultsディレクトリからJSONファイルを読み込んで結果を返す"""
    results = {}
    results_path = Path(results_dir)

    if not results_path.exists():
        print(f"Results directory '{results_dir}' not found")
        return results

    json_files = list(results_path.glob("*.json"))
    if not json_files:
        print(f"No JSON files found in '{results_dir}'")
        return results

    for json_file in json_files:
        try:
            with open(json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)

            # ファイル名からモデル名を抽出（.jsonを除く）
            model_name = json_file.stem
            # '--' を '/' に戻す
            display_name = model_name.replace('--', '/')

            # サマリー情報を抽出
            if 'summary' in data:
                results[display_name] = {
                    'llm_accuracy': data['summary'].get('llm_accuracy', 0),
                    'mozc_accuracy': data['summary'].get('mozc_accuracy', 0),
                    'llm_better_rate': data['summary'].get('llm_better_rate', 0),
                    'mozc_better_rate': data['summary'].get('mozc_better_rate', 0),
                    'both_correct_rate': data['summary'].get('both_correct_rate', 0),
                    'both_wrong_rate': data['summary'].get('both_wrong_rate', 0),
                    'total_cases': data['summary'].get('total_cases', 0),
                    'model': data['metadata'].get('model', model_name),
                    'timestamp': data['metadata'].get('timestamp', 'unknown')
                }
                print(f"Loaded: {display_name} ({data['summary']['total_cases']} cases)")
            else:
                print(f"Warning: No summary found in {json_file}")

        except Exception as e:
            print(f"Error loading {json_file}: {e}")

    return results


def plot_benchmark_comparison(results: Dict[str, Dict], output_file: str = "benchmark_comparison.png"):
    """ベンチマーク結果の比較棒グラフを作成"""
    if not results:
        print("No data to plot")
        return

    # データの準備（精度の高い順にソート）
    sorted_items = sorted(results.items(), key=lambda x: x[1]['llm_accuracy'], reverse=True)
    models = [item[0] for item in sorted_items]
    llm_accuracies = [item[1]['llm_accuracy'] * 100 for item in sorted_items]

    # グラフの設定（単一グラフに変更）
    fig, ax = plt.subplots(figsize=(12, 8))
    fig.suptitle('LLM変換候補選択ベンチマーク結果', fontsize=16, fontweight='bold')

    # LLM精度グラフ（精度順にソート済み）
    x = range(len(models))
    width = 0.6

    ax.bar(x, llm_accuracies, width,
            label='LLM精度', color='#2E8B57', alpha=0.8)

    ax.set_ylabel('精度 (%)')
    ax.set_title('LLM変換精度（精度順）')
    ax.set_xticks(x)
    ax.set_xticklabels(models, rotation=45, ha='right')
    ax.legend()
    ax.grid(axis='y', alpha=0.3)
    ax.set_ylim(0, 100)

    # 値をバーの上に表示
    for i, llm_acc in enumerate(llm_accuracies):
        ax.text(i, llm_acc + 1, f'{llm_acc:.1f}%',
                ha='center', va='bottom', fontsize=10)

    # レイアウト調整
    plt.tight_layout()

    # ファイル保存
    plt.savefig(output_file, dpi=300, bbox_inches='tight',
                facecolor='white', edgecolor='none')
    print(f"Graph saved to: {output_file}")

    # 表示
    plt.show()


def print_summary_table(results: Dict[str, Dict]):
    """結果のサマリーテーブルを表示（精度順）"""
    print("\n=== ベンチマーク結果サマリー（精度順） ===")
    print(f"{'モデル':<25} {'LLM精度':<8}")
    print("-" * 40)

    # 精度の高い順にソート
    sorted_items = sorted(results.items(), key=lambda x: x[1]['llm_accuracy'], reverse=True)

    for model, data in sorted_items:
        llm_acc = data['llm_accuracy'] * 100
        print(f"{model:<25} {llm_acc:>6.1f}%")


def main():
    """メイン関数"""
    # 引数処理
    if len(sys.argv) > 1:
        results_dir = sys.argv[1]
    else:
        results_dir = "results"

    if len(sys.argv) > 2:
        output_file = sys.argv[2]
    else:
        output_file = "benchmark_comparison.png"

    print(f"Loading benchmark results from: {results_dir}")

    # 結果の読み込み
    results = load_benchmark_results(results_dir)

    if not results:
        print("No valid benchmark results found.")
        sys.exit(1)

    # サマリーテーブル表示
    print_summary_table(results)

    # グラフ作成
    plot_benchmark_comparison(results, output_file)


if __name__ == "__main__":
    main()