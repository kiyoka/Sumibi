#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
plot_errorrate_vs_paramsize.py

各モデルの変換エラー率とパラメータ数(単位: ビリオン)をプロットする
romaji_direct と katakana_to_hiragana の両方のデータをプロット可能

Usage:
  python3 plot_errorrate_vs_paramsize.py -o output.png
"""
import matplotlib.pyplot as plt
import argparse
import json
import glob
import os

# モデル名とパラメータ数のマッピング
MODEL_PARAM_SIZES = {
    'gemma-3-12b-it-qat': 12,
    'gemma-3-27b-it-qat': 27,
    'gemma-3-27b-it-Q8_0': 27,
    'gemma-3n-e4b-it-mlx': 4,
    'gemma-3n-e2b-it-mlx': 2,
    'japanese-stablelm-instruct-gamma-7b': 7,
    'hermes-3-llama-3.2-3b': 3,
    'llama-3-elyza-jp-8b': 8,
    'llama-4-scout-17b-16e-instruct': 17,
    'llama-3.3-70b-instruct': 70,
    'stockmark-2-100b-instruct-beta@q3_k_m': 100,
    'llm-jp-3.1-8x13b-instruct4': 104,
    'llm-jp-3.1-13b-instruct4': 13,
    'gpt-oss-120b(low)': 120,
    'sarashina2.2-3b-instruct-v0.1': 3,
    'openai/gpt-oss-20b': 20,
    'gpt-oss-20b': 20,
}

# モデルタイプごとのマーカー設定
MODEL_MARKERS = {
    'gemma': 's',
    'llm-jp': 'o',
    'japanese-stablelm': 'o',
    'hermes': 'o',
    'llama-3-elyza': 'o',  # 日本語特化型モデル
    'llama': 's',
    'stockmark': 's',
    'gpt-oss': 'o',
    'sarashina': 'o',
    'openai': 'o',
    'rakutenai': 'o',
}

def get_marker(model_name):
    """モデル名からマーカーを取得"""
    for key, marker in MODEL_MARKERS.items():
        if model_name.startswith(key):
            return marker
    return 'o'

def load_results(json_path):
    """JSONファイルからCERの平均値と平均レスポンス時間を計算"""
    try:
        with open(json_path, 'r', encoding='utf-8') as f:
            results = json.load(f)

        if not results:
            return None, None

        # CERの平均を計算（1.0を超える場合は1.0に丸める）
        cer_sum = 0.0
        elapsed_times = []
        for rec in results:
            cer = rec.get('cer', 0.0)
            if cer > 1.0:
                cer = 1.0
            cer_sum += cer

            # elapsed_secを収集
            elapsed = rec.get('elapsed_sec', 0.0)
            elapsed_times.append(elapsed)

        mean_cer = cer_sum / len(results)

        # 平均レスポンス時間を計算（95パーセンタイル）
        if elapsed_times:
            elapsed_times_sorted = sorted(elapsed_times)
            p95_index = int(len(elapsed_times_sorted) * 0.95)
            mean_response_time = sum(elapsed_times_sorted[:p95_index]) / p95_index if p95_index > 0 else sum(elapsed_times_sorted) / len(elapsed_times_sorted)
        else:
            mean_response_time = 0.0

        return mean_cer, mean_response_time
    except Exception as e:
        print(f"Warning: Could not load {json_path}: {e}")
        return None, None

def collect_data(result_dirs):
    """結果ディレクトリからデータを収集（サブディレクトリも含む）"""
    data_romaji_direct_input = []
    data_hiragana_input = []
    data_katakana_input = []

    for result_dir in result_dirs:
        if not os.path.exists(result_dir):
            continue

        # romaji_direct_input データ（通常の.jsonファイル、サブディレクトリも含む）
        for json_file in glob.glob(os.path.join(result_dir, '**', '*.json'), recursive=True):
            # 相対パスを取得してモデル名を決定
            rel_path = os.path.relpath(json_file, result_dir)

            # _hiragana.json と _katakana.json は除外
            if rel_path.endswith('_hiragana.json') or rel_path.endswith('_katakana.json'):
                continue

            # モデル名を構築（サブディレクトリを含む）
            model_name = rel_path.replace('.json', '')

            # パラメータ数が定義されているモデルのみ
            if model_name in MODEL_PARAM_SIZES:
                mean_cer, mean_response_time = load_results(json_file)
                if mean_cer is not None:
                    data_romaji_direct_input.append({
                        'name': model_name,
                        'cer': mean_cer,
                        'param_size': MODEL_PARAM_SIZES[model_name],
                        'marker': get_marker(model_name),
                        'response_time': mean_response_time
                    })

        # hiragana_input データ（_hiragana.jsonファイル、サブディレクトリも含む）
        for json_file in glob.glob(os.path.join(result_dir, '**', '*_hiragana.json'), recursive=True):
            # 相対パスを取得してモデル名を決定
            rel_path = os.path.relpath(json_file, result_dir)
            model_name = rel_path.replace('_hiragana.json', '')

            # パラメータ数が定義されているモデルのみ
            if model_name in MODEL_PARAM_SIZES:
                mean_cer, mean_response_time = load_results(json_file)
                if mean_cer is not None:
                    data_hiragana_input.append({
                        'name': model_name,
                        'cer': mean_cer,
                        'param_size': MODEL_PARAM_SIZES[model_name],
                        'marker': get_marker(model_name),
                        'response_time': mean_response_time
                    })

        # katakana_input データ（_katakana.jsonファイル、サブディレクトリも含む）
        for json_file in glob.glob(os.path.join(result_dir, '**', '*_katakana.json'), recursive=True):
            # 相対パスを取得してモデル名を決定
            rel_path = os.path.relpath(json_file, result_dir)
            model_name = rel_path.replace('_katakana.json', '')

            # パラメータ数が定義されているモデルのみ
            if model_name in MODEL_PARAM_SIZES:
                mean_cer, mean_response_time = load_results(json_file)
                if mean_cer is not None:
                    data_katakana_input.append({
                        'name': model_name,
                        'cer': mean_cer,
                        'param_size': MODEL_PARAM_SIZES[model_name],
                        'marker': get_marker(model_name),
                        'response_time': mean_response_time
                    })

    return data_romaji_direct_input, data_hiragana_input, data_katakana_input

def plot_data(data_romaji_direct_input, data_hiragana_input, data_katakana_input, output_path=None, figsize=(10, 6), dpi=100, ylim=None, xlim=None):
    """データをプロット"""
    plt.figure(figsize=figsize)

    # 全データからresponse_timeの範囲を取得
    all_data = data_romaji_direct_input + data_hiragana_input + data_katakana_input
    response_times = [item['response_time'] for item in all_data if item.get('response_time', 0) > 0]

    if response_times:
        min_time = min(response_times)
        max_time = max(response_times)
    else:
        min_time = 1.0
        max_time = 5.0

    def get_point_size(response_time):
        """レスポンス時間からポイントサイズを計算（秒数に厳密に比例）"""
        if response_time <= 0:
            return 300  # デフォルトサイズ
        # 秒数に厳密に比例（1秒=200ポイント）
        return response_time * 200

    # romaji_direct_input データを辞書に変換（名前でアクセスしやすくする）
    romaji_direct_input_dict = {item['name']: item for item in data_romaji_direct_input}

    # 対応するモデル間に点線を引く（hiragana）
    for item_hiragana_input in data_hiragana_input:
        if item_hiragana_input['name'] in romaji_direct_input_dict:
            item_romaji_direct_input = romaji_direct_input_dict[item_hiragana_input['name']]
            # 点線を引く
            plt.plot([item_romaji_direct_input['param_size'], item_hiragana_input['param_size']],
                    [item_romaji_direct_input['cer'] * 100, item_hiragana_input['cer'] * 100],
                    linestyle='--', color='gray', alpha=0.5, linewidth=1, zorder=1)

    # 対応するモデル間に点線を引く（katakana）
    for item_katakana_input in data_katakana_input:
        if item_katakana_input['name'] in romaji_direct_input_dict:
            item_romaji_direct_input = romaji_direct_input_dict[item_katakana_input['name']]
            # 点線を引く
            plt.plot([item_romaji_direct_input['param_size'], item_katakana_input['param_size']],
                    [item_romaji_direct_input['cer'] * 100, item_katakana_input['cer'] * 100],
                    linestyle='--', color='gray', alpha=0.5, linewidth=1, zorder=1)

    # romaji_direct_input データをプロット（青系）
    for item in data_romaji_direct_input:
        pct = item['cer'] * 100
        point_size = get_point_size(item.get('response_time', 0))
        plt.scatter(item['param_size'], pct, s=point_size, color='tab:blue',
                   marker=item['marker'], alpha=0.7, label='romaji_direct_input' if item == data_romaji_direct_input[0] else '', zorder=3)
        plt.annotate(item['name'],
                    xy=(item['param_size'], pct),
                    xytext=(5, 5),
                    textcoords='offset points',
                    ha='left', va='bottom', clip_on=False, fontsize=8)

    # hiragana_input データをプロット（赤系）
    for item in data_hiragana_input:
        pct = item['cer'] * 100
        point_size = get_point_size(item.get('response_time', 0))
        plt.scatter(item['param_size'], pct, s=point_size, color='tab:red',
                   marker=item['marker'], alpha=0.7, label='hiragana_input' if item == data_hiragana_input[0] else '', zorder=3)

    # katakana_input データをプロット（緑系）
    for item in data_katakana_input:
        pct = item['cer'] * 100
        point_size = get_point_size(item.get('response_time', 0))
        plt.scatter(item['param_size'], pct, s=point_size, color='tab:green',
                   marker=item['marker'], alpha=0.7, label='katakana_input' if item == data_katakana_input[0] else '', zorder=3)

    plt.xlabel('Parameter Size (Billion)')
    plt.ylabel('Error Rate (%)')
    plt.title('Error Rate vs Parameter Size for Local LLMs\n(Blue: romaji_direct_input, Red: hiragana_input, Green: katakana_input)')
    plt.grid(True, alpha=0.3)

    # 軸の範囲を設定
    if ylim:
        plt.ylim(ylim[0], ylim[1])
    else:
        plt.ylim(0, 110)

    if xlim:
        plt.xlim(xlim[0], xlim[1])
    else:
        plt.margins(x=0.05)

    # 凡例を追加
    legend_elements = []
    if data_romaji_direct_input:
        legend_elements.append(plt.Line2D([0], [0], marker='o', color='w',
                                         markerfacecolor='tab:blue', markersize=10,
                                         label='romaji_direct_input', alpha=0.7))
    if data_katakana_input:
        legend_elements.append(plt.Line2D([0], [0], marker='o', color='w',
                                         markerfacecolor='tab:green', markersize=10,
                                         label='katakana_input', alpha=0.7))
    if data_hiragana_input:
        legend_elements.append(plt.Line2D([0], [0], marker='o', color='w',
                                         markerfacecolor='tab:red', markersize=10,
                                         label='hiragana_input', alpha=0.7))

    # 平均応答時間の凡例を追加
    if response_times:
        legend_elements.append(plt.Line2D([0], [0], color='none', label=''))  # 空行
        legend_elements.append(plt.Line2D([0], [0], color='none', label='Response Time:'))

        # 1秒、2秒、3秒、4秒のサンプルポイント
        sample_times = [1.0, 2.0, 3.0, 4.0]
        for time in sample_times:
            size = get_point_size(time)
            # scatterのsパラメータ（面積）をマーカーサイズ（直径相当）に変換
            # markersize = sqrt(s) / 調整係数
            marker_size = (size ** 0.5) / 2.5
            legend_elements.append(plt.Line2D([0], [0], marker='o', color='w',
                                             markerfacecolor='gray', markersize=marker_size,
                                             label=f'{time:.1f}s', alpha=0.7))

    if legend_elements:
        plt.legend(handles=legend_elements, loc='lower right', framealpha=0.9)

    plt.tight_layout()

    if output_path:
        plt.savefig(output_path, dpi=dpi, bbox_inches='tight')
        print(f"Plot saved to {output_path}")
    else:
        plt.show()

def main():
    parser = argparse.ArgumentParser(description='Plot error rate vs parameter size')
    parser.add_argument('-o', '--output', help='Output image file path')
    parser.add_argument('-d', '--dirs', nargs='+', default=['result_ver2.3.0', 'result_ver2.4.0'],
                       help='Result directories to scan (default: result_ver2.3.0 result_ver2.4.0)')
    parser.add_argument('--width', type=int, default=1000, help='Image width in pixels (default: 1000)')
    parser.add_argument('--height', type=int, default=600, help='Image height in pixels (default: 600)')
    parser.add_argument('--dpi', type=int, default=100, help='DPI for output image (default: 100)')
    args = parser.parse_args()

    # データ収集
    data_romaji_direct_input, data_hiragana_input, data_katakana_input = collect_data(args.dirs)

    print(f"Found {len(data_romaji_direct_input)} romaji_direct_input results")
    print(f"Found {len(data_hiragana_input)} hiragana_input results")
    print(f"Found {len(data_katakana_input)} katakana_input results")

    if not data_romaji_direct_input and not data_hiragana_input and not data_katakana_input:
        print("No data found to plot")
        return

    # Calculate figsize from pixel dimensions and DPI
    figsize = (args.width / args.dpi, args.height / args.dpi)

    # プロット（通常版）
    plot_data(data_romaji_direct_input, data_hiragana_input, data_katakana_input, args.output, figsize=figsize, dpi=args.dpi)

    # ズーム版も生成
    if args.output:
        # 出力パスからズーム版のパスを生成
        import os
        base, ext = os.path.splitext(args.output)
        # _1000x600 が含まれている場合は置き換え、そうでない場合は _zoomed を追加
        if '_1000x600' in base:
            zoomed_output = base.replace('_1000x600', '_zoomed_1000x600') + ext
        else:
            zoomed_output = base + '_zoomed' + ext

        # ズーム版をプロット（error rate: 35-110%, parameter size: 0-25）
        plot_data(data_romaji_direct_input, data_hiragana_input, data_katakana_input,
                 zoomed_output, figsize=figsize, dpi=args.dpi, ylim=(35, 110), xlim=(0, 25))

if __name__ == '__main__':
    main()
