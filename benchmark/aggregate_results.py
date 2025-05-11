#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
aggregate_results.py

集計ライブラリ:
  - JSON 形式のベンチマーク結果ファイルを読み込み
  - cer の平均値 (1.0 を超える場合は 1.0 に丸め)
  - at1 の平均値を計算
  - elapsed_sec の最小値、最大値、平均値を計算

Usage example:
  python3 aggregate_results.py benchmark/result/gpt-4.1.json
"""
import argparse
import json
import sys

def load_results(path):
    """
    JSON ファイルを読み込み、辞書のリストを返す
    """
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def summarize(results):
    """
    results: List[Dict]  各要素に 'cer' (float)、'at1' (int)、'elapsed_sec' (float) を含む
    cer が 1.0 を超える場合は 1.0 に丸めて平均を計算
    at1 の平均値を計算
    elapsed_sec の最小値、最大値、平均値を計算
    戻り値: (mean_cer, mean_at1, min_elapsed_sec, max_elapsed_sec, mean_elapsed_sec)
    """
    n = len(results)
    if n == 0:
        return float('nan'), float('nan'), float('nan'), float('nan'), float('nan')
    cer_sum = 0.0
    at1_sum = 0.0
    for rec in results:
        cer = rec.get('cer', 0.0)
        # cer が 1.0 を超える場合は丸め
        if cer > 1.0:
            cer = 1.0
        cer_sum += cer
        at1_sum += rec.get('at1', 0)
    mean_cer = cer_sum / n
    mean_at1 = at1_sum / n
    # elapsed_sec の最小値、最大値、平均値
    elapsed_vals = [rec.get('elapsed_sec', 0.0) for rec in results]
    min_elapsed_sec = min(elapsed_vals)
    max_elapsed_sec = max(elapsed_vals)
    mean_elapsed_sec = sum(elapsed_vals) / n
    return mean_cer, mean_at1, min_elapsed_sec, max_elapsed_sec, mean_elapsed_sec

def main():
    parser = argparse.ArgumentParser(
        description='Aggregate CER, AT1, and elapsed_sec stats from JSON benchmark results')
    parser.add_argument('files', nargs='+', help='result JSON files')
    args = parser.parse_args()
    for path in args.files:
        try:
            results = load_results(path)
        except Exception as e:
            print(f"Error loading '{path}': {e}", file=sys.stderr)
            continue
        mean_cer, mean_at1, min_elapsed_sec, max_elapsed_sec, mean_elapsed_sec = summarize(results)
        print(
            f"{path}: mean_cer = {mean_cer:.6f}, mean_at1 = {mean_at1:.6f}, "
            f"min_elapsed_sec = {min_elapsed_sec:.6f}, max_elapsed_sec = {max_elapsed_sec:.6f}, "
            f"mean_elapsed_sec = {mean_elapsed_sec:.6f}"
        )

if __name__ == '__main__':
    main()