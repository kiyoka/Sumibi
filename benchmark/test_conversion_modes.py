#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Test script to demonstrate the difference between romaji_direct_input, hiragana_input, and katakana_input modes.
This doesn't require API keys - just shows the conversion logic.
"""

from katakana_to_romaji_converter import KatakanaToRomajiConverter
from katakana_to_hiragana_converter import KatakanaToHiraganaConverter

def main():
    romaji_converter = KatakanaToRomajiConverter()
    hiragana_converter = KatakanaToHiraganaConverter()

    # Test case from issue 96
    context_text = "こんにちは、西山です。"
    test_katakana = "ワタシノナマエハニシヤマデス。"

    print("=" * 70)
    print("Issue 96: Comparison of three modes")
    print("=" * 70)

    # Mode 1: romaji_direct_input (current approach)
    romaji_text = romaji_converter.convert(test_katakana)
    print("\n[Mode 1: romaji_direct_input]")
    print(f"  Context text: {context_text}")
    print(f"  Katakana input: {test_katakana}")
    print(f"  Step 1 - Katakana to Romaji: {romaji_text}")
    print(f"  LLM surrounding_text: {context_text + romaji_text}")
    print(f"  LLM henkan_text: {romaji_text}")
    print(f"  Expected LLM Output: 私の名前は西山です。")

    # Mode 2: hiragana_input (new approach)
    hiragana_text = hiragana_converter.convert(test_katakana)
    print("\n[Mode 2: hiragana_input]")
    print(f"  Context text: {context_text}")
    print(f"  Katakana input: {test_katakana}")
    print(f"  Step 1 - Katakana to Hiragana: {hiragana_text}")
    print(f"  LLM surrounding_text: {context_text + hiragana_text}")
    print(f"  LLM henkan_text: {hiragana_text}")
    print(f"  Expected LLM Output: 私の名前は西山です。")

    # Mode 3: katakana_input (experimental approach)
    katakana_text = test_katakana  # Use katakana directly
    print("\n[Mode 3: katakana_input]")
    print(f"  Context text: {context_text}")
    print(f"  Katakana input: {test_katakana}")
    print(f"  Step 1 - Katakana (direct): {katakana_text}")
    print(f"  LLM surrounding_text: {context_text + katakana_text}")
    print(f"  LLM henkan_text: {katakana_text}")
    print(f"  Expected LLM Output: 私の名前は西山です。")

    print("\n" + "=" * 70)
    print("Hypotheses:")
    print("  Mode 2 (hiragana_input): May improve LLM accuracy and response time")
    print("    because the model can better understand Japanese context.")
    print("  Mode 3 (katakana_input): Expected to have similar accuracy to Mode 1")
    print("    (romaji_direct_input), as katakana is less natural for LLMs.")
    print("=" * 70)

if __name__ == '__main__':
    main()
