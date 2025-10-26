#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Test script to demonstrate the difference between romaji_direct and katakana_to_hiragana modes.
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
    print("Issue 96: Comparison of two modes")
    print("=" * 70)

    # Mode 1: romaji_direct (current approach)
    romaji_text = romaji_converter.convert(test_katakana)
    print("\n[Mode 1: romaji_direct]")
    print(f"  Context text: {context_text}")
    print(f"  Katakana input: {test_katakana}")
    print(f"  Step 1 - Katakana to Romaji: {romaji_text}")
    print(f"  LLM surrounding_text: {context_text + romaji_text}")
    print(f"  LLM henkan_text: {romaji_text}")
    print(f"  Expected LLM Output: 私の名前は西山です。")

    # Mode 2: katakana_to_hiragana (new approach)
    hiragana_text = hiragana_converter.convert(test_katakana)
    print("\n[Mode 2: katakana_to_hiragana]")
    print(f"  Context text: {context_text}")
    print(f"  Katakana input: {test_katakana}")
    print(f"  Step 1 - Katakana to Hiragana: {hiragana_text}")
    print(f"  LLM surrounding_text: {context_text + hiragana_text}")
    print(f"  LLM henkan_text: {hiragana_text}")
    print(f"  Expected LLM Output: 私の名前は西山です。")

    print("\n" + "=" * 60)
    print("Hypothesis:")
    print("  Hiragana input may improve LLM accuracy and response time")
    print("  because the model can better understand Japanese context.")
    print("=" * 60)

if __name__ == '__main__':
    main()
