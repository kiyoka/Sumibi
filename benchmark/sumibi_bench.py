#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
sumibiのローマ字仮名漢字変換のベンチマークを実行して、結果をJSONで出力するプログラムです。
"""

import sys
import os
import json
import time
from openai import APITimeoutError
from katakana_to_romaji_converter import KatakanaToRomajiConverter
from katakana_to_hiragana_converter import KatakanaToHiraganaConverter
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
    Supports three modes: romaji_direct_input, hiragana_input, and katakana_input.
    """
    def __init__(self, mode='romaji_direct_input'):
        """
        Args:
            mode: 'romaji_direct_input', 'hiragana_input', or 'katakana_input'
        """
        self.mode = mode
        self.romaji_converter = KatakanaToRomajiConverter()
        self.hiragana_converter = KatakanaToHiraganaConverter() if mode == 'hiragana_input' else None

        # Get model name from environment variable
        # Supported gpt-5 models: gpt-5, gpt-5-mini, gpt-5-nano
        model = os.getenv("SUMIBI_AI_MODEL", "gpt-4.1")

        # Set temperature=1.0 for gpt-5 models
        temperature = 1.0 if model.startswith("gpt-5") else None

        # Set reasoning_effort based on model
        if model in ("gpt-5.1", "gpt-5.2"):
            reasoning_effort = None  # gpt-5.1 and gpt-5.2 use reasoning_effort=none
        elif model.startswith("gpt-5"):
            reasoning_effort = "minimal"  # Fixed to minimal for all gpt-5 models
        elif model.startswith("gpt-oss-") or model.startswith("openai.gpt-oss-"):
            reasoning_effort = "low"
        else:
            reasoning_effort = None

        # Set verbosity for gpt-5 models
        verbosity = None
        if model.startswith("gpt-5"):
            verbosity = "low"  # Fixed to low for all gpt-5 models

        self.client = SumibiTypicalConvertClient(model=model, temperature=temperature, reasoning_effort=reasoning_effort, verbosity=verbosity)
        # collect conversion results
        self.result_arr = []

    def henkan(self, expected_output, surrounding_text, henkan_text, katakana_text, context_text, skip_save=False):
        """
        Perform conversion and print inputs and result.
        Supports three modes:
        - 'hiragana_input': convert katakana to hiragana
        - 'katakana_input': use katakana directly
        - 'romaji_direct_input': convert katakana to romaji (default)

        Args:
            skip_save: If True, do not save the result to result_arr (for warmup runs)
        """
        # Determine LLM input based on mode
        if self.mode == 'hiragana_input':
            # Mode 2: katakana -> hiragana -> LLM
            henkan_text_llm = self.hiragana_converter.convert(katakana_text)
            surrounding_text_llm = context_text + henkan_text_llm
        elif self.mode == 'katakana_input':
            # Mode 3: katakana -> katakana (direct) -> LLM
            henkan_text_llm = katakana_text
            surrounding_text_llm = context_text + katakana_text
        else:
            # Mode 1: katakana -> romaji -> LLM (default)
            surrounding_text_llm = surrounding_text
            henkan_text_llm = henkan_text

        # measure conversion time
        start = time.perf_counter()
        try:
            result = self.client.convert(surrounding_text_llm, henkan_text_llm)
            end = time.perf_counter()
            elapsed = end - start
            timeout_occurred = False
        except APITimeoutError as e:
            end = time.perf_counter()
            elapsed = end - start
            result = "[TIMEOUT]"
            timeout_occurred = True
            print(f"  => TIMEOUT ERROR after {elapsed:.2f} sec: {e}")

        warmup_label = " [WARMUP - not saved]" if skip_save else ""
        if not timeout_occurred:
            print(f"  => elapsed: {elapsed:.2f} sec{warmup_label}")
        print(f"mode:            '{self.mode}'")
        if self.mode == 'hiragana_input':
            print(f"katakana_text:                '{katakana_text}'")
            print(f"surrounding_text (romaji):    '{surrounding_text + henkan_text}'")
            print(f"surrounding_text (hiragana):  '{surrounding_text_llm}'")
            print(f"henkan_text (hiragana):       '{henkan_text_llm}'")
        elif self.mode == 'katakana_input':
            print(f"katakana_text:                '{katakana_text}'")
            print(f"surrounding_text (romaji):    '{surrounding_text + henkan_text}'")
            print(f"surrounding_text (katakana):  '{surrounding_text_llm}'")
            print(f"henkan_text (katakana):       '{henkan_text_llm}'")
        else:
            print(f"katakana_text:   '{katakana_text}'")
            print(f"surrounding_text: '{surrounding_text}'")
            print(f"henkan_text:     '{henkan_text}'")
        print(f"expect:          '{expected_output}'")
        print(f"result:          '{result}'\n")

        # Only save results if not a warmup run
        if not skip_save:
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
        Iterate over evaluation_data entries, convert katakana based on mode,
        then perform henkan on combined context and print results.

        The first entry is executed twice as warmup (results not saved) to load
        the LM Studio model, then all entries are executed normally.
        """
        if len(evaluation_data) == 0:
            print("Warning: evaluation_data is empty")
            return

        # Warmup: execute first entry twice without saving results
        print("=" * 80)
        print("WARMUP PHASE: Executing first entry twice to load LM Studio model")
        print("=" * 80)
        first_entry = evaluation_data[0]
        expected_output = first_entry.get('expected_output', [])
        context_text = first_entry.get('context_text', '')
        katakana_text = first_entry.get('input', '')
        romaji_text = self.romaji_converter.convert(katakana_text)

        print("\nWarmup run 1/2:")
        self.henkan(expected_output, context_text + romaji_text, romaji_text, katakana_text, context_text, skip_save=True)

        print("\nWarmup run 2/2:")
        self.henkan(expected_output, context_text + romaji_text, romaji_text, katakana_text, context_text, skip_save=True)

        print("=" * 80)
        print("BENCHMARK PHASE: Executing all entries with result saving")
        print("=" * 80)

        # Normal execution: process all entries and save results
        for idx, entry in enumerate(evaluation_data, 1):
            print(f"\nBenchmark entry {idx}/{len(evaluation_data)}:")
            expected_output = entry.get('expected_output', [])
            context_text = entry.get('context_text', '')
            katakana_text = entry.get('input', '')
            romaji_text = self.romaji_converter.convert(katakana_text)
            self.henkan(expected_output, context_text + romaji_text, romaji_text, katakana_text, context_text, skip_save=False)

def main():
    # Arguments: <evaluation_json_file> <output_json_file> [mode]
    # mode: 'romaji_direct_input' (default), 'hiragana_input', or 'katakana_input'
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <evaluation_json_file> <output_json_file> [mode]")
        print(f"  mode: 'romaji_direct_input' (default), 'hiragana_input', or 'katakana_input'")
        sys.exit(1)
    input_path = sys.argv[1]
    output_path = sys.argv[2]
    mode = sys.argv[3] if len(sys.argv) > 3 else 'romaji_direct_input'
    with open(input_path, 'r', encoding='utf-8') as f:
        evaluation_data = json.load(f)
    # evaluation_data に dict 型で読み込まれたデータを保持
    # 確認用出力
    if False:
        print("Loaded evaluation_data:")  
        print(json.dumps(evaluation_data, ensure_ascii=False, indent=2))
    # ベンチマーク用データの取得開始
    bench = SumibiBench(mode=mode)
    bench.benchmark(evaluation_data)
    # benchmark 完了後、結果を output_path に JSON 形式で保存
    with open(output_path, 'w', encoding='utf-8') as fo:
        json.dump(bench.result_arr, fo, ensure_ascii=False, indent=2)
    
if __name__ == "__main__":
    main()
