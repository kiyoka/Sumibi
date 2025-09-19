#!/usr/bin/env python3
"""
LLMによる変換候補選択のベンチマーク
"""

import json
import time
import re
import os
import sys
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass

# 親ディレクトリのmozc_helperをインポート
sys.path.append(str(Path(__file__).parent.parent))
from mozc_helper import MozcClient

try:
    import openai
except ImportError:
    print("openai package not found. Please install: pip install openai")
    sys.exit(1)

@dataclass
class TestCase:
    context: str  # 前後の文脈
    reading: str  # 変換対象の読み
    correct_answer: str  # 正解の変換結果
    source_text: str  # 元のテキスト（参考）

class LLMSelectionBenchmark:
    def __init__(self, api_key: str, model: str = "gpt-4o-mini"):
        """ベンチマーク初期化"""
        self.client = openai.OpenAI(api_key=api_key)
        self.model = model
        self.mozc_client = MozcClient()
        self.results = []

    def extract_test_cases_from_aozora(self, text_file: str, num_cases: int = 50) -> List[TestCase]:
        """青空文庫テキストからテストケースを抽出"""
        try:
            with open(text_file, 'r', encoding='utf-8') as f:
                text = f.read()
        except FileNotFoundError:
            print(f"File not found: {text_file}")
            return []

        test_cases = []
        sentences = self._split_into_sentences(text)

        for sentence in sentences[:num_cases * 2]:  # 余裕を持って抽出
            cases = self._extract_cases_from_sentence(sentence)
            test_cases.extend(cases)

            if len(test_cases) >= num_cases:
                break

        return test_cases[:num_cases]

    def _split_into_sentences(self, text: str) -> List[str]:
        """テキストを文に分割"""
        # 句読点で分割
        sentences = re.split(r'[。！？\n]', text)
        # 短すぎる文や長すぎる文を除外
        return [s.strip() for s in sentences if 10 <= len(s.strip()) <= 100]

    def _extract_cases_from_sentence(self, sentence: str) -> List[TestCase]:
        """1つの文からテストケースを抽出"""
        cases = []

        # よくある同音異義語パターンを検出
        patterns = [
            (r'(納|収|治|修)める', 'おさめる'),
            (r'(測|計|図|量|諮|謀)る', 'はかる'),
            (r'(新|真|心)しい', 'あたらしい'),
            (r'(生|行|逝|往)く', 'いく'),
            (r'(心|志)', 'こころ'),
            (r'(取|撮|採|執)る', 'とる'),
        ]

        for pattern, reading in patterns:
            matches = list(re.finditer(pattern, sentence))
            for match in matches:
                # 前後の文脈を取得
                start = max(0, match.start() - 20)
                end = min(len(sentence), match.end() + 20)
                context = sentence[start:end]

                # 変換対象部分を読みに置き換えた文脈を作成
                context_with_reading = (
                    sentence[:match.start()] +
                    f"[{reading}]" +
                    sentence[match.end():]
                )

                test_case = TestCase(
                    context=context_with_reading,
                    reading=reading,
                    correct_answer=match.group(),
                    source_text=sentence
                )
                cases.append(test_case)

        return cases

    def run_llm_selection(self, test_case: TestCase, candidates: List[Dict[str, str]]) -> str:
        """LLMに候補選択を依頼"""
        if not candidates:
            return test_case.reading

        # 候補リストを文字列として整形
        candidates_text = "\n".join([
            f"{i+1}. {c['candidate']}"
            for i, c in enumerate(candidates)
        ])

        prompt = f"""
以下の文脈で、「{test_case.reading}」を最も適切な漢字に変換してください。

文脈: {test_case.context}

変換候補:
{candidates_text}

回答は候補番号のみ答えてください（例: 1）。
"""

        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "あなたは日本語の文脈に基づいて最適な漢字変換を選択するアシスタントです。"},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.1,
                max_tokens=10
            )

            llm_response = response.choices[0].message.content.strip()

            # 番号を抽出
            match = re.search(r'\d+', llm_response)
            if match:
                selected_idx = int(match.group()) - 1
                if 0 <= selected_idx < len(candidates):
                    return candidates[selected_idx]['candidate']

        except Exception as e:
            print(f"LLM selection error: {e}")

        # デフォルトは最初の候補
        return candidates[0]['candidate'] if candidates else test_case.reading

    def evaluate_single_case(self, test_case: TestCase) -> Dict:
        """単一テストケースの評価"""
        # Mozcから候補を取得
        candidates = self.mozc_client.get_conversion_candidates(
            test_case.reading,
            test_case.context,
            max_candidates=6
        )

        # LLMによる選択
        llm_selection = self.run_llm_selection(test_case, candidates)

        # Mozcのトップ候補
        mozc_top = candidates[0]['candidate'] if candidates else test_case.reading

        # 評価結果
        result = {
            'test_case': {
                'context': test_case.context,
                'reading': test_case.reading,
                'correct_answer': test_case.correct_answer,
                'source_text': test_case.source_text
            },
            'candidates': candidates,
            'llm_selection': llm_selection,
            'mozc_top': mozc_top,
            'llm_correct': llm_selection == test_case.correct_answer,
            'mozc_correct': mozc_top == test_case.correct_answer,
            'improvement': (llm_selection == test_case.correct_answer) and (mozc_top != test_case.correct_answer)
        }

        return result

    def run_benchmark(self, data_files: List[str], output_file: str = "results/benchmark_results.json"):
        """ベンチマーク実行"""
        print("=== LLM Selection Benchmark ===")
        print(f"Model: {self.model}")

        all_test_cases = []

        # テストケースの抽出
        for data_file in data_files:
            print(f"Extracting test cases from: {data_file}")
            cases = self.extract_test_cases_from_aozora(data_file, num_cases=20)
            all_test_cases.extend(cases)
            print(f"  Extracted {len(cases)} cases")

        print(f"\nTotal test cases: {len(all_test_cases)}")

        # 評価実行
        results = []
        for i, test_case in enumerate(all_test_cases):
            print(f"Evaluating case {i+1}/{len(all_test_cases)}: {test_case.reading}")

            result = self.evaluate_single_case(test_case)
            results.append(result)

            # プログレス表示
            if result['llm_correct']:
                print(f"  ✓ LLM correct: {result['llm_selection']}")
            else:
                print(f"  ✗ LLM incorrect: {result['llm_selection']} (correct: {test_case.correct_answer})")

            time.sleep(0.5)  # API制限対策

        # 結果の集計
        self._save_results(results, output_file)
        self._print_summary(results)

    def _save_results(self, results: List[Dict], output_file: str):
        """結果をJSONファイルに保存"""
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)

        summary = self._calculate_summary(results)

        output_data = {
            'metadata': {
                'model': self.model,
                'total_cases': len(results),
                'timestamp': time.strftime('%Y-%m-%d %H:%M:%S')
            },
            'summary': summary,
            'details': results
        }

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(output_data, f, ensure_ascii=False, indent=2)

        print(f"\nResults saved to: {output_file}")

    def _calculate_summary(self, results: List[Dict]) -> Dict:
        """結果のサマリーを計算"""
        total = len(results)
        llm_correct = sum(1 for r in results if r['llm_correct'])
        mozc_correct = sum(1 for r in results if r['mozc_correct'])
        improvements = sum(1 for r in results if r['improvement'])

        return {
            'total_cases': total,
            'llm_accuracy': llm_correct / total if total > 0 else 0,
            'mozc_accuracy': mozc_correct / total if total > 0 else 0,
            'llm_correct_count': llm_correct,
            'mozc_correct_count': mozc_correct,
            'improvements': improvements,
            'improvement_rate': improvements / total if total > 0 else 0
        }

    def _print_summary(self, results: List[Dict]):
        """結果サマリーを表示"""
        summary = self._calculate_summary(results)

        print("\n=== Benchmark Results ===")
        print(f"Total test cases: {summary['total_cases']}")
        print(f"LLM accuracy: {summary['llm_accuracy']:.1%} ({summary['llm_correct_count']}/{summary['total_cases']})")
        print(f"Mozc accuracy: {summary['mozc_accuracy']:.1%} ({summary['mozc_correct_count']}/{summary['total_cases']})")
        print(f"LLM improvements: {summary['improvements']} cases ({summary['improvement_rate']:.1%})")

def main():
    """メイン関数"""
    # 環境変数からAPI設定を取得
    api_key = os.getenv('OPENAI_API_KEY')
    if not api_key:
        print("Please set OPENAI_API_KEY environment variable")
        return

    model = os.getenv('OPENAI_MODEL', 'gpt-4o-mini')

    # データファイルを指定
    data_files = [
        'data/kokoro_natsume.txt',
        'data/rashomon_akutagawa.txt',
        'data/ginga_tetsudo_miyazawa.txt'
    ]

    # ベンチマーク実行
    benchmark = LLMSelectionBenchmark(api_key, model)
    benchmark.run_benchmark(data_files)

if __name__ == "__main__":
    main()