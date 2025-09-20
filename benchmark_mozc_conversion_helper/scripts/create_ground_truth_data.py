#!/usr/bin/env python3
"""
青空文庫から抽出したパターンから正解データを作成するスクリプト
"""

import json
import re
import sys
from pathlib import Path
from typing import List, Dict, Tuple

# 親ディレクトリのmozc_helperをインポート
sys.path.append(str(Path(__file__).parent.parent))
from mozc_helper import MozcClient

def clean_ruby_tags(text: str) -> str:
    """
    テキストからrubyタグを削除

    Args:
        text: 処理対象のテキスト

    Returns:
        rubyタグが除去されたテキスト
    """
    # <ruby><rb>漢字</rb><rp>（</rp><rt>ひらがな</rt><rp>）</rp></ruby> → 漢字
    text = re.sub(r'<ruby><rb>([^<]+)</rb><rp>[^<]*</rp><rt>[^<]*</rt><rp>[^<]*</rp></ruby>', r'\1', text)

    # 他のHTMLタグも除去
    text = re.sub(r'<[^>]+>', '', text)

    return text

def extract_reading_and_kanji(pattern_text: str) -> Tuple[str, str, str]:
    """
    パターンテキストから読み、漢字、文脈を抽出

    Args:
        pattern_text: TestCase形式の文字列

    Returns:
        (reading, kanji, context): 読み、漢字、文脈のタプル
    """
    # TestCase(context="...", reading="...", correct_answer="...", source_text="...")形式を解析
    context_match = re.search(r'context="([^"]*)"', pattern_text)
    reading_match = re.search(r'reading="([^"]*)"', pattern_text)
    correct_match = re.search(r'correct_answer="([^"]*)"', pattern_text)

    if not all([context_match, reading_match, correct_match]):
        return None, None, None

    context = context_match.group(1)
    reading = reading_match.group(1)
    correct_answer = correct_match.group(1)

    # rubyタグを除去
    context = clean_ruby_tags(context)
    correct_answer = clean_ruby_tags(correct_answer)

    return reading, correct_answer, context

def create_ground_truth_data(pattern_file: str, output_file: str):
    """
    抽出されたパターンファイルから正解データを作成

    Args:
        pattern_file: extracted_pattern_code.txtのパス
        output_file: 出力JSONファイルのパス
    """
    print(f"Reading patterns from: {pattern_file}")

    try:
        with open(pattern_file, 'r', encoding='utf-8') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: Pattern file not found: {pattern_file}")
        return

    ground_truth_data = []

    # TestCase(...) パターンを検索
    pattern_matches = re.findall(r'TestCase\([^)]+\)', content, re.DOTALL)

    print(f"Found {len(pattern_matches)} test case patterns")

    for i, pattern in enumerate(pattern_matches):
        reading, correct_answer, context = extract_reading_and_kanji(pattern)

        if reading and correct_answer and context:
            ground_truth_entry = {
                "id": i + 1,
                "reading": reading,
                "correct_answer": correct_answer,
                "context": context,
                "source": "aozora_bunko"
            }
            ground_truth_data.append(ground_truth_entry)

            if i < 5:  # 最初の5件を表示
                print(f"  {i+1}. {reading} → {correct_answer}")
        else:
            print(f"  Warning: Could not parse pattern {i+1}")

    # JSON形式で保存
    try:
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(ground_truth_data, f, ensure_ascii=False, indent=2)

        print(f"\nGround truth data saved to: {output_file}")
        print(f"Total entries: {len(ground_truth_data)}")

    except Exception as e:
        print(f"Error saving ground truth data: {e}")

def main():
    """メイン関数"""
    pattern_file = "extracted_pattern_code.txt"
    output_file = "ground_truth_data.json"

    if not Path(pattern_file).exists():
        print(f"Error: Pattern file not found: {pattern_file}")
        print("Please run extract_patterns_from_aozora.py first")
        return

    create_ground_truth_data(pattern_file, output_file)

if __name__ == "__main__":
    main()