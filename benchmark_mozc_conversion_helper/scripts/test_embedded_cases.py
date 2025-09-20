#!/usr/bin/env python3
"""
抽出済みテストケース（extracted_pattern_code.txt）の検証スクリプト
"""

import sys
from pathlib import Path

# 親ディレクトリのmozc_helperをインポート
sys.path.append(str(Path(__file__).parent.parent))

# llm_selection_benchmarkから必要なクラスをインポート
from scripts.llm_selection_benchmark import LLMSelectionBenchmark, TestCase

def test_extracted_cases():
    """extracted_pattern_code.txt から読み込んだテストケースを検証"""

    # ダミーのAPI keyでインスタンス作成（実際にはAPIを呼ばない）
    benchmark = LLMSelectionBenchmark("dummy_key")

    # 抽出済みテストケースを取得
    test_cases = benchmark._load_cases_from_file()
    if not test_cases:
        print("ERROR: extracted_pattern_code.txt が見つからないか、テストケースが0件です。")
        print("先に scripts/extract_patterns_from_corpus.py を実行して extracted_pattern_code.txt を生成してください。")
        return False

    print(f"抽出されたテストケース数: {len(test_cases)}")

    # 最初の5件を表示
    print("\n最初の5件:")
    for i, case in enumerate(test_cases[:5]):
        print(f"{i+1}. 読み: {case.reading}")
        print(f"   正解: {case.correct_answer}")
        print(f"   文脈: {case.context[:50]}...")
        print()

    # 最後の5件を表示
    print("最後の5件:")
    for i, case in enumerate(test_cases[-5:], len(test_cases)-4):
        print(f"{i}. 読み: {case.reading}")
        print(f"   正解: {case.correct_answer}")
        print(f"   文脈: {case.context[:50]}...")
        print()

    # 各フィールドが正しく設定されているかチェック
    for i, case in enumerate(test_cases):
        if not case.reading or not case.correct_answer:
            print(f"ERROR: Case {i+1} has missing reading or correct_answer")
            return False
        if not case.context or not case.source_text:
            print(f"ERROR: Case {i+1} has missing context or source_text")
            return False

    print("✓ 全てのテストケースが正しく設定されています")
    return True

if __name__ == "__main__":
    success = test_extracted_cases()
    if success:
        print("\n✓ テストケースの検証が完了しました")
        sys.exit(0)
    else:
        print("\n✗ テストケースの検証でエラーが発生しました")
        sys.exit(1)
