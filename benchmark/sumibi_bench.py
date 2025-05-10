#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
sumibiのローマ字仮名漢字変換のベンチマークを実行して、結果をJSONで出力するプログラムです。
"""

import sys
import json
from katakana_to_romaji_converter import KatakanaToRomajiConverter
from sumibi_typical_convert_client import SumibiTypicalConvertClient
from importlib.machinery import SourceFileLoader
ajimee_utils = SourceFileLoader(
    "ajimee_utils", "./AJIMEE-Bench/utils.py"
).load_module()

class SumibiBench:
    """
    Benchmarks sumibi romaji->kana->kanji conversion using
    KatakanaToRomajiConverter and SumibiTypicalConvertClient.
    """
    def __init__(self):
        self.converter = KatakanaToRomajiConverter()
        self.client = SumibiTypicalConvertClient()

    def henkan(self, surrounding_text, henkan_text):
        """
        Perform conversion and print inputs and result.
        """
        result = self.client.convert(surrounding_text, henkan_text)
        print(f"surrounding_text: '{surrounding_text}'")
        print(f"henkan_text:     '{henkan_text}'")
        print(f"result:          '{result}'\n")

    def benchmark(self, evaluation_data):
        """
        Iterate over evaluation_data entries, convert katakana to romaji,
        then perform henkan on combined context and print results.
        """
        for entry in evaluation_data:
            context_text = entry.get('context_text', '')
            katakana_text = entry.get('input', '')
            romaji_text = self.converter.convert(katakana_text)
            self.henkan(context_text + romaji_text, romaji_text)

def main():
    # 第一引数で指定されたJSONファイルを読み込み、evaluation_dataというdict型に変換
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <evaluation_json_file>")
        sys.exit(1)
    json_path = sys.argv[1]
    with open(json_path, 'r', encoding='utf-8') as f:
        evaluation_data = json.load(f)
    # evaluation_data に dict 型で読み込まれたデータを保持
    # 確認用出力
    if False:
        print("Loaded evaluation_data:")  
        print(json.dumps(evaluation_data, ensure_ascii=False, indent=2))
    # ベンチマーク用データの取得開始
    bench = SumibiBench()
    bench.benchmark(evaluation_data)
    
if __name__ == "__main__":
    main()
