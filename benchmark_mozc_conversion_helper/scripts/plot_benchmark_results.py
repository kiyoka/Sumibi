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
    """resultsディレクトリとそのサブディレクトリからJSONファイルを読み込んで結果を返す"""
    results = {}
    results_path = Path(results_dir)

    if not results_path.exists():
        print(f"Results directory '{results_dir}' not found")
        return results

    # results/ 直下とサブディレクトリ（hardware1, hardware2など）からJSONファイルを取得
    json_files = list(results_path.glob("*.json")) + list(results_path.glob("*/*.json"))
    if not json_files:
        print(f"No JSON files found in '{results_dir}' or its subdirectories")
        return results

    for json_file in json_files:
        try:
            with open(json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)

            # ファイル名からモデル名を抽出（.jsonを除く）
            model_name = json_file.stem
            # '--' を '/' に戻す
            display_name = model_name.replace('--', '/')

            # サブディレクトリ名を取得
            hardware_tag = None
            if json_file.parent.name != results_path.name:
                hardware_tag = json_file.parent.name
                display_name = f"[{hardware_tag}] {display_name}"

            # サマリー情報を抽出
            if 'summary' in data:
                # API応答時間情報を取得（存在する場合）
                api_time_data = data['summary'].get('api_response_time', {})
                avg_response_time = api_time_data.get('average', None) if api_time_data else None

                results[display_name] = {
                    'llm_accuracy': data['summary'].get('llm_accuracy', 0),
                    'mozc_accuracy': data['summary'].get('mozc_accuracy', 0),
                    'llm_better_rate': data['summary'].get('llm_better_rate', 0),
                    'mozc_better_rate': data['summary'].get('mozc_better_rate', 0),
                    'both_correct_rate': data['summary'].get('both_correct_rate', 0),
                    'both_wrong_rate': data['summary'].get('both_wrong_rate', 0),
                    'total_cases': data['summary'].get('total_cases', 0),
                    'model': data['metadata'].get('model', model_name),
                    'timestamp': data['metadata'].get('timestamp', 'unknown'),
                    'avg_response_time': avg_response_time,
                    'hardware_tag': hardware_tag,
                    'base_model': model_name
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

    # hardware1のデータを基準にソート（hardware1がない場合は全データでソート）
    hw1_results = {k: v for k, v in results.items() if v.get('hardware_tag') == 'hardware1' or v.get('hardware_tag') is None}
    if not hw1_results:
        hw1_results = results

    sorted_hw1 = sorted(hw1_results.items(), key=lambda x: x[1]['llm_accuracy'], reverse=True)

    # hardware1のモデル順序を基準に全データを整理
    model_order = [item[1]['base_model'] for item in sorted_hw1]

    # 各モデルについて、hardware1とhardware2のデータを収集
    model_data = {}
    for base_model in model_order:
        model_data[base_model] = {'hw1': None, 'hw2': None}
        for key, value in results.items():
            if value['base_model'] == base_model:
                hw_tag = value.get('hardware_tag')
                if hw_tag == 'hardware1':
                    model_data[base_model]['hw1'] = value
                elif hw_tag == 'hardware2':
                    model_data[base_model]['hw2'] = value
                elif hw_tag is None:  # results/直下のファイル
                    model_data[base_model]['hw1'] = value

    # 表示用データの準備
    models = []
    hw1_accuracies = []
    hw2_accuracies = []
    hw1_times = []
    hw2_times = []

    for base_model in model_order:
        data = model_data[base_model]
        if data['hw1'] or data['hw2']:
            # モデル名は[hardware1]タグなしで表示
            models.append(base_model.replace('--', '/'))

            # 精度
            hw1_accuracies.append(data['hw1']['llm_accuracy'] * 100 if data['hw1'] else None)
            hw2_accuracies.append(data['hw2']['llm_accuracy'] * 100 if data['hw2'] else None)

            # レスポンスタイム
            hw1_times.append(data['hw1']['avg_response_time'] if data['hw1'] else None)
            hw2_times.append(data['hw2']['avg_response_time'] if data['hw2'] else None)

    # 2つのサブプロットを作成
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    fig.suptitle('LLM変換候補選択ベンチマーク結果', fontsize=16, fontweight='bold')

    # LLM精度グラフ（hardware1とhardware2を重ねて表示）
    bar_width_acc = 0.35
    x_pos_acc = range(len(models))

    # 各バーの位置を計算
    hw1_acc_positions = [i - bar_width_acc/2 for i in x_pos_acc]
    hw2_acc_positions = [i + bar_width_acc/2 for i in x_pos_acc]

    # hardware1のバー（緑色）
    hw1_display_acc = [a if a is not None else 0 for a in hw1_accuracies]
    bars_hw1_acc = ax1.bar(hw1_acc_positions, hw1_display_acc, bar_width_acc,
                           label='Hardware1', color='#2E8B57', alpha=0.8)

    # hardware2のバー（紫色）
    hw2_display_acc = [a if a is not None else 0 for a in hw2_accuracies]
    bars_hw2_acc = ax1.bar(hw2_acc_positions, hw2_display_acc, bar_width_acc,
                           label='Hardware2', color='#9B59B6', alpha=0.8)

    ax1.set_ylabel('精度 (%)')
    ax1.set_title('LLM変換精度（精度順）')
    ax1.set_xticks(x_pos_acc)
    ax1.set_xticklabels(models, rotation=45, ha='right')
    ax1.legend()
    ax1.grid(axis='y', alpha=0.3)
    ax1.set_ylim(0, 105)

    # 精度値をバーの上に表示
    for i, (hw1_acc, hw2_acc) in enumerate(zip(hw1_accuracies, hw2_accuracies)):
        if hw1_acc is not None:
            ax1.text(hw1_acc_positions[i], hw1_acc + 1, f'{hw1_acc:.1f}%',
                    ha='center', va='bottom', fontsize=8)
        if hw2_acc is not None:
            ax1.text(hw2_acc_positions[i], hw2_acc + 1, f'{hw2_acc:.1f}%',
                    ha='center', va='bottom', fontsize=8)

    # 平均レスポンスタイムグラフ（hardware1とhardware2を重ねて表示）
    bar_width = 0.35
    x_pos = range(len(models))

    # 各バーの位置を計算
    hw1_positions = [i - bar_width/2 for i in x_pos]
    hw2_positions = [i + bar_width/2 for i in x_pos]

    # hardware1のバー（青色）
    hw1_display_times = [t if t is not None else 0 for t in hw1_times]
    hw1_colors = ['#4472C4' if t is not None else '#CCCCCC' for t in hw1_times]
    bars_hw1 = ax2.bar(hw1_positions, hw1_display_times, bar_width,
                       label='Hardware1', color='#4472C4', alpha=0.8)

    # hardware2のバー（オレンジ色）
    hw2_display_times = [t if t is not None else 0 for t in hw2_times]
    hw2_colors = ['#ED7D31' if t is not None else '#CCCCCC' for t in hw2_times]
    bars_hw2 = ax2.bar(hw2_positions, hw2_display_times, bar_width,
                       label='Hardware2', color='#ED7D31', alpha=0.8)

    ax2.set_ylabel('平均レスポンスタイム (秒)')
    ax2.set_title('API平均レスポンスタイム')
    ax2.set_xticks(x_pos)
    ax2.set_xticklabels(models, rotation=45, ha='right')
    ax2.legend()
    ax2.grid(axis='y', alpha=0.3)

    # Y軸の最大値を設定
    all_valid_times = [t for t in hw1_times + hw2_times if t is not None]
    if all_valid_times:
        max_time = max(all_valid_times)
        ax2.set_ylim(0, max_time * 1.15)

        # レスポンスタイム値をバーの上に表示
        for i, (hw1_t, hw2_t) in enumerate(zip(hw1_times, hw2_times)):
            if hw1_t is not None:
                ax2.text(hw1_positions[i], hw1_t + max_time * 0.02, f'{hw1_t:.2f}s',
                        ha='center', va='bottom', fontsize=8)
            if hw2_t is not None:
                ax2.text(hw2_positions[i], hw2_t + max_time * 0.02, f'{hw2_t:.2f}s',
                        ha='center', va='bottom', fontsize=8)

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
    print(f"{'モデル':<25} {'LLM精度':<8} {'平均レスポンス':<12}")
    print("-" * 50)

    # 精度の高い順にソート
    sorted_items = sorted(results.items(), key=lambda x: x[1]['llm_accuracy'], reverse=True)

    for model, data in sorted_items:
        llm_acc = data['llm_accuracy'] * 100
        avg_time = data['avg_response_time']

        if avg_time is None:
            time_str = "データなし"
        else:
            time_str = f"{avg_time:.3f}s"

        print(f"{model:<25} {llm_acc:>6.1f}% {time_str:>11}")


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