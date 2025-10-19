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

            # base_modelを取得（-optimizeサフィックスを除去）
            base_model_name = model_name.replace('-optimize', '')

            # サブディレクトリ名を取得
            hardware_tag = None
            if json_file.parent.name != results_path.name:
                hardware_tag = json_file.parent.name
                display_name = f"[{hardware_tag}] {display_name}"
            else:
                # results/ 直下のファイルはCloudタグを付ける
                hardware_tag = "cloud"

            # サマリー情報を抽出
            if 'summary' in data:
                # API応答時間情報を取得（存在する場合）
                api_time_data = data['summary'].get('api_response_time', {})
                avg_response_time = api_time_data.get('average', None) if api_time_data else None

                # prompt_modeを取得（デフォルトは "rerank-all"）
                prompt_mode = data['metadata'].get('prompt_mode', 'rerank-all')

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
                    'base_model': base_model_name,
                    'prompt_mode': prompt_mode
                }
                print(f"Loaded: {display_name} ({data['summary']['total_cases']} cases, {prompt_mode})")
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

    # すべてのモデルを精度順にソート（hardware1優先、なければcloud、なければhardware2、なければhardware3）
    all_models = {}
    for key, value in results.items():
        base_model = value['base_model']
        if base_model not in all_models:
            all_models[base_model] = value
        else:
            # hardware1を優先、次にcloud、次にhardware2、最後にhardware3
            current_tag = all_models[base_model].get('hardware_tag')
            new_tag = value.get('hardware_tag')
            priority = {'hardware1': 0, 'cloud': 1, 'hardware2': 2, 'hardware3': 3}
            if priority.get(new_tag, 99) < priority.get(current_tag, 99):
                all_models[base_model] = value

    sorted_models = sorted(all_models.items(), key=lambda x: x[1]['llm_accuracy'], reverse=True)
    model_order = [item[1]['base_model'] for item in sorted_models]

    # 各モデルについて、hardware1, hardware2, hardware3, cloud、およびprompt_modeのデータを収集
    model_data = {}
    for base_model in model_order:
        model_data[base_model] = {
            'hw1_rerank': None, 'hw1_optimize': None,
            'hw2_rerank': None, 'hw2_optimize': None,
            'hw3_rerank': None, 'hw3_optimize': None,
            'cloud_rerank': None, 'cloud_optimize': None
        }
        for key, value in results.items():
            if value['base_model'] == base_model:
                hw_tag = value.get('hardware_tag')
                prompt_mode = value.get('prompt_mode', 'rerank-all')
                mode_suffix = '_optimize' if prompt_mode == 'optimize' else '_rerank'

                if hw_tag == 'hardware1':
                    model_data[base_model][f'hw1{mode_suffix}'] = value
                elif hw_tag == 'hardware2':
                    model_data[base_model][f'hw2{mode_suffix}'] = value
                elif hw_tag == 'hardware3':
                    model_data[base_model][f'hw3{mode_suffix}'] = value
                elif hw_tag == 'cloud':
                    model_data[base_model][f'cloud{mode_suffix}'] = value

    # 表示用データの準備
    models = []
    hw1_rerank_accuracies = []
    hw1_optimize_accuracies = []
    hw2_rerank_accuracies = []
    hw2_optimize_accuracies = []
    hw3_rerank_accuracies = []
    hw3_optimize_accuracies = []
    cloud_rerank_accuracies = []
    cloud_optimize_accuracies = []

    hw1_rerank_times = []
    hw1_optimize_times = []
    hw2_rerank_times = []
    hw2_optimize_times = []
    hw3_rerank_times = []
    hw3_optimize_times = []
    cloud_rerank_times = []
    cloud_optimize_times = []

    for base_model in model_order:
        data = model_data[base_model]
        # いずれかのデータが存在する場合のみ追加
        if any(data.values()):
            # モデル名は[hardware1]タグなしで表示
            models.append(base_model.replace('--', '/'))

            # 精度データ
            hw1_rerank_accuracies.append(data['hw1_rerank']['llm_accuracy'] * 100 if data['hw1_rerank'] else None)
            hw1_optimize_accuracies.append(data['hw1_optimize']['llm_accuracy'] * 100 if data['hw1_optimize'] else None)
            hw2_rerank_accuracies.append(data['hw2_rerank']['llm_accuracy'] * 100 if data['hw2_rerank'] else None)
            hw2_optimize_accuracies.append(data['hw2_optimize']['llm_accuracy'] * 100 if data['hw2_optimize'] else None)
            hw3_rerank_accuracies.append(data['hw3_rerank']['llm_accuracy'] * 100 if data['hw3_rerank'] else None)
            hw3_optimize_accuracies.append(data['hw3_optimize']['llm_accuracy'] * 100 if data['hw3_optimize'] else None)
            cloud_rerank_accuracies.append(data['cloud_rerank']['llm_accuracy'] * 100 if data['cloud_rerank'] else None)
            cloud_optimize_accuracies.append(data['cloud_optimize']['llm_accuracy'] * 100 if data['cloud_optimize'] else None)

            # レスポンスタイムデータ
            hw1_rerank_times.append(data['hw1_rerank']['avg_response_time'] if data['hw1_rerank'] else None)
            hw1_optimize_times.append(data['hw1_optimize']['avg_response_time'] if data['hw1_optimize'] else None)
            hw2_rerank_times.append(data['hw2_rerank']['avg_response_time'] if data['hw2_rerank'] else None)
            hw2_optimize_times.append(data['hw2_optimize']['avg_response_time'] if data['hw2_optimize'] else None)
            hw3_rerank_times.append(data['hw3_rerank']['avg_response_time'] if data['hw3_rerank'] else None)
            hw3_optimize_times.append(data['hw3_optimize']['avg_response_time'] if data['hw3_optimize'] else None)
            cloud_rerank_times.append(data['cloud_rerank']['avg_response_time'] if data['cloud_rerank'] else None)
            cloud_optimize_times.append(data['cloud_optimize']['avg_response_time'] if data['cloud_optimize'] else None)

    # 2つのサブプロットを作成
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    fig.suptitle('LLM変換候補選択ベンチマーク結果', fontsize=16, fontweight='bold')

    # LLM精度グラフ（hardware1, hardware2, hardware3, cloud、rerank-all, optimizeを分けて表示）
    # 各ハードウェアのrerank-allとoptimizeを隣接して配置
    bar_width_acc = 0.10
    x_pos_acc = range(len(models))

    # 各バーの位置を計算（8本のバーを並べる：HW1-rerank, HW1-opt, HW2-rerank, HW2-opt, HW3-rerank, HW3-opt, Cloud-rerank, Cloud-opt）
    hw1_rerank_pos = [i - 3.5*bar_width_acc for i in x_pos_acc]
    hw1_optimize_pos = [i - 3.5*bar_width_acc + bar_width_acc for i in x_pos_acc]
    hw2_rerank_pos = [i - 1.5*bar_width_acc for i in x_pos_acc]
    hw2_optimize_pos = [i - 1.5*bar_width_acc + bar_width_acc for i in x_pos_acc]
    hw3_rerank_pos = [i + 0.5*bar_width_acc for i in x_pos_acc]
    hw3_optimize_pos = [i + 0.5*bar_width_acc + bar_width_acc for i in x_pos_acc]
    cloud_rerank_pos = [i + 2.5*bar_width_acc for i in x_pos_acc]
    cloud_optimize_pos = [i + 2.5*bar_width_acc + bar_width_acc for i in x_pos_acc]

    # hardware1のバー（緑系: rerank-all=濃い緑、optimize=明るい緑）
    hw1_rerank_display = [a if a is not None else 0 for a in hw1_rerank_accuracies]
    hw1_optimize_display = [a if a is not None else 0 for a in hw1_optimize_accuracies]
    ax1.bar(hw1_rerank_pos, hw1_rerank_display, bar_width_acc,
            label='HW1-rerank-all', color='#2E8B57', alpha=0.9)
    ax1.bar(hw1_optimize_pos, hw1_optimize_display, bar_width_acc,
            label='HW1-optimize', color='#90EE90', alpha=0.9)

    # hardware2のバー（紫系: rerank-all=濃い紫、optimize=明るい紫）
    hw2_rerank_display = [a if a is not None else 0 for a in hw2_rerank_accuracies]
    hw2_optimize_display = [a if a is not None else 0 for a in hw2_optimize_accuracies]
    ax1.bar(hw2_rerank_pos, hw2_rerank_display, bar_width_acc,
            label='HW2-rerank-all', color='#9B59B6', alpha=0.9)
    ax1.bar(hw2_optimize_pos, hw2_optimize_display, bar_width_acc,
            label='HW2-optimize', color='#DDA0DD', alpha=0.9)

    # hardware3のバー（オレンジ系: rerank-all=濃いオレンジ、optimize=明るいオレンジ）
    hw3_rerank_display = [a if a is not None else 0 for a in hw3_rerank_accuracies]
    hw3_optimize_display = [a if a is not None else 0 for a in hw3_optimize_accuracies]
    ax1.bar(hw3_rerank_pos, hw3_rerank_display, bar_width_acc,
            label='HW3-rerank-all', color='#FF8C00', alpha=0.9)
    ax1.bar(hw3_optimize_pos, hw3_optimize_display, bar_width_acc,
            label='HW3-optimize', color='#FFD700', alpha=0.9)

    # cloudのバー（青系: rerank-all=濃い青、optimize=明るい青）
    cloud_rerank_display = [a if a is not None else 0 for a in cloud_rerank_accuracies]
    cloud_optimize_display = [a if a is not None else 0 for a in cloud_optimize_accuracies]
    ax1.bar(cloud_rerank_pos, cloud_rerank_display, bar_width_acc,
            label='Cloud-rerank-all', color='#4169E1', alpha=0.9)
    ax1.bar(cloud_optimize_pos, cloud_optimize_display, bar_width_acc,
            label='Cloud-optimize', color='#87CEEB', alpha=0.9)

    ax1.set_ylabel('精度 (%)')
    ax1.set_title('LLM変換精度（精度順）')
    ax1.set_xticks(x_pos_acc)
    ax1.set_xticklabels(models, rotation=45, ha='right', fontsize=8)
    ax1.legend(fontsize=7, loc='lower left')
    ax1.grid(axis='y', alpha=0.3)
    ax1.set_ylim(0, 105)

    # 精度値をバーの上に表示（optimizeのみ表示してグラフを見やすくする）
    all_acc_data = [
        (hw1_optimize_pos, hw1_optimize_accuracies),
        (hw2_optimize_pos, hw2_optimize_accuracies),
        (hw3_optimize_pos, hw3_optimize_accuracies),
        (cloud_optimize_pos, cloud_optimize_accuracies)
    ]
    for positions, accuracies in all_acc_data:
        for i, acc in enumerate(accuracies):
            if acc is not None and acc > 0:
                ax1.text(positions[i], acc + 1, f'{acc:.0f}',
                        ha='center', va='bottom', fontsize=5)

    # 平均レスポンスタイムグラフ（hardware1, hardware2, hardware3, cloud、rerank-all, optimizeを分けて表示）
    # 各ハードウェアのrerank-allとoptimizeを隣接して配置
    bar_width = 0.10
    x_pos = range(len(models))

    # 各バーの位置を計算（8本のバーを並べる：HW1-rerank, HW1-opt, HW2-rerank, HW2-opt, HW3-rerank, HW3-opt, Cloud-rerank, Cloud-opt）
    hw1_rerank_time_pos = [i - 3.5*bar_width for i in x_pos]
    hw1_optimize_time_pos = [i - 3.5*bar_width + bar_width for i in x_pos]
    hw2_rerank_time_pos = [i - 1.5*bar_width for i in x_pos]
    hw2_optimize_time_pos = [i - 1.5*bar_width + bar_width for i in x_pos]
    hw3_rerank_time_pos = [i + 0.5*bar_width for i in x_pos]
    hw3_optimize_time_pos = [i + 0.5*bar_width + bar_width for i in x_pos]
    cloud_rerank_time_pos = [i + 2.5*bar_width for i in x_pos]
    cloud_optimize_time_pos = [i + 2.5*bar_width + bar_width for i in x_pos]

    # hardware1のバー（緑系: rerank-all=濃い緑、optimize=明るい緑）
    hw1_rerank_time_display = [t if t is not None else 0 for t in hw1_rerank_times]
    hw1_optimize_time_display = [t if t is not None else 0 for t in hw1_optimize_times]
    ax2.bar(hw1_rerank_time_pos, hw1_rerank_time_display, bar_width,
            label='HW1-rerank-all', color='#2E8B57', alpha=0.9)
    ax2.bar(hw1_optimize_time_pos, hw1_optimize_time_display, bar_width,
            label='HW1-optimize', color='#90EE90', alpha=0.9)

    # hardware2のバー（紫系: rerank-all=濃い紫、optimize=明るい紫）
    hw2_rerank_time_display = [t if t is not None else 0 for t in hw2_rerank_times]
    hw2_optimize_time_display = [t if t is not None else 0 for t in hw2_optimize_times]
    ax2.bar(hw2_rerank_time_pos, hw2_rerank_time_display, bar_width,
            label='HW2-rerank-all', color='#9B59B6', alpha=0.9)
    ax2.bar(hw2_optimize_time_pos, hw2_optimize_time_display, bar_width,
            label='HW2-optimize', color='#DDA0DD', alpha=0.9)

    # hardware3のバー（オレンジ系: rerank-all=濃いオレンジ、optimize=明るいオレンジ）
    hw3_rerank_time_display = [t if t is not None else 0 for t in hw3_rerank_times]
    hw3_optimize_time_display = [t if t is not None else 0 for t in hw3_optimize_times]
    ax2.bar(hw3_rerank_time_pos, hw3_rerank_time_display, bar_width,
            label='HW3-rerank-all', color='#FF8C00', alpha=0.9)
    ax2.bar(hw3_optimize_time_pos, hw3_optimize_time_display, bar_width,
            label='HW3-optimize', color='#FFD700', alpha=0.9)

    # cloudのバー（青系: rerank-all=濃い青、optimize=明るい青）
    cloud_rerank_time_display = [t if t is not None else 0 for t in cloud_rerank_times]
    cloud_optimize_time_display = [t if t is not None else 0 for t in cloud_optimize_times]
    ax2.bar(cloud_rerank_time_pos, cloud_rerank_time_display, bar_width,
            label='Cloud-rerank-all', color='#4169E1', alpha=0.9)
    ax2.bar(cloud_optimize_time_pos, cloud_optimize_time_display, bar_width,
            label='Cloud-optimize', color='#87CEEB', alpha=0.9)

    ax2.set_ylabel('平均レスポンスタイム (秒)')
    ax2.set_title('API平均レスポンスタイム')
    ax2.set_xticks(x_pos)
    ax2.set_xticklabels(models, rotation=45, ha='right', fontsize=8)
    ax2.legend(fontsize=7, loc='upper left')
    ax2.grid(axis='y', alpha=0.3)

    # Y軸の最大値を設定
    all_valid_times = [t for t in (hw1_rerank_times + hw1_optimize_times +
                                    hw2_rerank_times + hw2_optimize_times +
                                    hw3_rerank_times + hw3_optimize_times +
                                    cloud_rerank_times + cloud_optimize_times) if t is not None]
    if all_valid_times:
        max_time = max(all_valid_times)
        ax2.set_ylim(0, max_time * 1.15)

        # レスポンスタイム値をバーの上に表示（optimizeのみ表示してグラフを見やすくする）
        all_time_data = [
            (hw1_optimize_time_pos, hw1_optimize_times),
            (hw2_optimize_time_pos, hw2_optimize_times),
            (hw3_optimize_time_pos, hw3_optimize_times),
            (cloud_optimize_time_pos, cloud_optimize_times)
        ]
        for positions, times in all_time_data:
            for i, t in enumerate(times):
                if t is not None and t > 0:
                    ax2.text(positions[i], t + max_time * 0.02, f'{t:.1f}',
                            ha='center', va='bottom', fontsize=5)

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