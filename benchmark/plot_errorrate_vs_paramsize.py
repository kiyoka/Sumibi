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
    'japanese-stablelm-instruct-gamma-7b': 7,
    'hermes-3-llama-3.2-3b': 3,
    'llama-4-scout-17b-16e-instruct': 17,
    'llama-3.3-70b-instruct': 70,
    'stockmark-2-100b-instruct-beta@q3_k_m': 100,
    'llm-jp-3.1-8x13b-instruct4': 104,
    'llm-jp-3.1-13b-instruct4': 13,
    'gpt-oss-120b(low)': 120,
    'sarashina2.2-3b-instruct-v0.1': 3,
    'openai/gpt-oss-20b': 20,
}

# モデルタイプごとのマーカー設定
MODEL_MARKERS = {
    'gemma': 's',
    'llm-jp': 'o',
    'japanese-stablelm': 'o',
    'hermes': 'o',
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
    """JSONファイルからCERの平均値を計算"""
    try:
        with open(json_path, 'r', encoding='utf-8') as f:
            results = json.load(f)

        if not results:
            return None

        # CERの平均を計算（1.0を超える場合は1.0に丸める）
        cer_sum = 0.0
        for rec in results:
            cer = rec.get('cer', 0.0)
            if cer > 1.0:
                cer = 1.0
            cer_sum += cer
        mean_cer = cer_sum / len(results)
        return mean_cer
    except Exception as e:
        print(f"Warning: Could not load {json_path}: {e}")
        return None

def collect_data(result_dirs):
    """結果ディレクトリからデータを収集（サブディレクトリも含む）"""
    data_romaji_direct_input = []
    data_hiragana_input = []

    for result_dir in result_dirs:
        if not os.path.exists(result_dir):
            continue

        # romaji_direct_input データ（通常の.jsonファイル、サブディレクトリも含む）
        for json_file in glob.glob(os.path.join(result_dir, '**', '*.json'), recursive=True):
            # 相対パスを取得してモデル名を決定
            rel_path = os.path.relpath(json_file, result_dir)

            # _hiragana.json は除外
            if rel_path.endswith('_hiragana.json'):
                continue

            # モデル名を構築（サブディレクトリを含む）
            model_name = rel_path.replace('.json', '')

            # パラメータ数が定義されているモデルのみ
            if model_name in MODEL_PARAM_SIZES:
                mean_cer = load_results(json_file)
                if mean_cer is not None:
                    data_romaji_direct_input.append({
                        'name': model_name,
                        'cer': mean_cer,
                        'param_size': MODEL_PARAM_SIZES[model_name],
                        'marker': get_marker(model_name)
                    })

        # hiragana_input データ（_hiragana.jsonファイル、サブディレクトリも含む）
        for json_file in glob.glob(os.path.join(result_dir, '**', '*_hiragana.json'), recursive=True):
            # 相対パスを取得してモデル名を決定
            rel_path = os.path.relpath(json_file, result_dir)
            model_name = rel_path.replace('_hiragana.json', '')

            # パラメータ数が定義されているモデルのみ
            if model_name in MODEL_PARAM_SIZES:
                mean_cer = load_results(json_file)
                if mean_cer is not None:
                    data_hiragana_input.append({
                        'name': model_name,
                        'cer': mean_cer,
                        'param_size': MODEL_PARAM_SIZES[model_name],
                        'marker': get_marker(model_name)
                    })

    return data_romaji_direct_input, data_hiragana_input

def plot_data(data_romaji_direct_input, data_hiragana_input, output_path=None, figsize=(10, 6), dpi=100):
    """データをプロット"""
    plt.figure(figsize=figsize)

    # romaji_direct_input データを辞書に変換（名前でアクセスしやすくする）
    romaji_direct_input_dict = {item['name']: item for item in data_romaji_direct_input}

    # 対応するモデル間に点線を引く
    for item_hiragana_input in data_hiragana_input:
        if item_hiragana_input['name'] in romaji_direct_input_dict:
            item_romaji_direct_input = romaji_direct_input_dict[item_hiragana_input['name']]
            # 点線を引く
            plt.plot([item_romaji_direct_input['param_size'], item_hiragana_input['param_size']],
                    [item_romaji_direct_input['cer'] * 100, item_hiragana_input['cer'] * 100],
                    linestyle='--', color='gray', alpha=0.5, linewidth=1, zorder=1)

    # romaji_direct_input データをプロット（青系）
    for item in data_romaji_direct_input:
        pct = item['cer'] * 100
        plt.scatter(item['param_size'], pct, s=150, color='tab:blue',
                   marker=item['marker'], alpha=0.7, label='romaji_direct_input' if item == data_romaji_direct_input[0] else '', zorder=3)
        plt.annotate(item['name'],
                    xy=(item['param_size'], pct),
                    xytext=(5, 5),
                    textcoords='offset points',
                    ha='left', va='bottom', clip_on=False, fontsize=8)

    # hiragana_input データをプロット（赤系）
    for item in data_hiragana_input:
        pct = item['cer'] * 100
        plt.scatter(item['param_size'], pct, s=150, color='tab:red',
                   marker=item['marker'], alpha=0.7, label='hiragana_input' if item == data_hiragana_input[0] else '', zorder=3)
        plt.annotate(item['name'] + ' (hiragana)',
                    xy=(item['param_size'], pct),
                    xytext=(5, -15),
                    textcoords='offset points',
                    ha='left', va='top', clip_on=False, fontsize=8, color='tab:red')

    plt.xlabel('Parameter Size (Billion)')
    plt.ylabel('Error Rate (%)')
    plt.title('Error Rate vs Parameter Size for Local LLMs\n(Blue: romaji_direct_input, Red: hiragana_input)')
    plt.grid(True, alpha=0.3)
    plt.ylim(0, 110)
    plt.margins(x=0.05)

    # 凡例を追加
    if data_romaji_direct_input or data_hiragana_input:
        plt.legend(loc='upper right')

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
    data_romaji_direct_input, data_hiragana_input = collect_data(args.dirs)

    print(f"Found {len(data_romaji_direct_input)} romaji_direct_input results")
    print(f"Found {len(data_hiragana_input)} hiragana_input results")

    if not data_romaji_direct_input and not data_hiragana_input:
        print("No data found to plot")
        return

    # Calculate figsize from pixel dimensions and DPI
    figsize = (args.width / args.dpi, args.height / args.dpi)

    # プロット
    plot_data(data_romaji_direct_input, data_hiragana_input, args.output, figsize=figsize, dpi=args.dpi)

if __name__ == '__main__':
    main()
