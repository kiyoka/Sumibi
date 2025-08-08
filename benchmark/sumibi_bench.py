#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
sumibiのローマ字仮名漢字変換のベンチマークを実行して、結果をJSONで出力するプログラムです。
"""

import sys
import os
import json
import time
from katakana_to_romaji_converter import KatakanaToRomajiConverter
from sumibi_typical_convert_client import SumibiTypicalConvertClient
from importlib.machinery import SourceFileLoader
# Dynamically load AJIMEE-Bench/utils.py without modifying it
script_dir = os.path.dirname(os.path.abspath(__file__))
utils_path = os.path.join(script_dir, "AJIMEE-Bench", "utils.py")
ajimee_utils = SourceFileLoader("ajimee_utils", utils_path).load_module()

class SumibiBench:
    """
    Benchmarks sumibi romaji->kana->kanji conversion using
    KatakanaToRomajiConverter and SumibiTypicalConvertClient.
    """
    def __init__(self):
        self.converter = KatakanaToRomajiConverter()
        # Set temperature=1.0 for gpt-5- models
        model = os.getenv("SUMIBI_AI_MODEL", "gpt-4.1")
        temperature = 1.0 if model.startswith("gpt-5-") else None
        self.client = SumibiTypicalConvertClient(temperature=temperature)
        # collect conversion results
        self.result_arr = []

    def henkan(self, expected_output, surrounding_text, henkan_text):
        """
        Perform conversion and print inputs and result.
        """
        # measure conversion time
        start = time.perf_counter()
        result = self.client.convert(surrounding_text, henkan_text)
        end = time.perf_counter()
        elapsed = end - start
        print(f"  => elapsed: {elapsed:.2f} sec")
        print(f"surrounding_text: '{surrounding_text}'")
        print(f"henkan_text:     '{henkan_text}'")
        print(f"expect:          '{expected_output}'")
        print(f"result:          '{result}'\n")
        cer = ajimee_utils.calculate_MinCER(expected_output, result)
        at1 = ajimee_utils.calculate_accuracy_at1(expected_output, result)
        # append to results
        self.result_arr.append({
            'surrounding_text': surrounding_text,
            'henkan_text': henkan_text,
            'expect': expected_output,
            'result': result,
            'cer': cer,
            'at1': at1,
            'elapsed_sec': elapsed
        })

    def benchmark(self, evaluation_data):
        """
        Iterate over evaluation_data entries, convert katakana to romaji,
        then perform henkan on combined context and print results.
        """
        for entry in evaluation_data:
            expected_output = entry.get('expected_output', [])
            context_text = entry.get('context_text', '')
            katakana_text = entry.get('input', '')
            romaji_text = self.converter.convert(katakana_text)
            self.henkan(expected_output, context_text + romaji_text, romaji_text)

def main():
    # 第一引数で指定されたJSONファイルを読み込み、第二引数で指定されたファイルに結果を保存
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <evaluation_json_file> <output_json_file>")
        sys.exit(1)
    input_path = sys.argv[1]
    output_path = sys.argv[2]
    with open(input_path, 'r', encoding='utf-8') as f:
        evaluation_data = json.load(f)
    # evaluation_data に dict 型で読み込まれたデータを保持
    # 確認用出力
    if False:
        print("Loaded evaluation_data:")  
        print(json.dumps(evaluation_data, ensure_ascii=False, indent=2))
    # ベンチマーク用データの取得開始
    bench = SumibiBench()
    bench.benchmark(evaluation_data)
    # benchmark 完了後、結果を output_path に JSON 形式で保存
    with open(output_path, 'w', encoding='utf-8') as fo:
        json.dump(bench.result_arr, fo, ensure_ascii=False, indent=2)
    
if __name__ == "__main__":
    main()
