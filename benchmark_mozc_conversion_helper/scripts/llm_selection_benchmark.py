#!/usr/bin/env python3
"""
LLMによる変換候補選択のベンチマーク（外部ファイル必須）

前提:
- extracted_pattern_code.txt に TestCase を埋め込んだコード片が生成済み
  （scripts/extract_patterns_from_corpus.py により作成）
"""

import json
import time
import re
import os
import sys
from pathlib import Path
from typing import List, Dict
from dataclasses import dataclass

# 親ディレクトリの mozc_helper をロード
sys.path.append(str(Path(__file__).parent.parent))
from mozc_helper import MozcClient

try:
    import openai
except ImportError:
    print("openai package not found. Please install: pip install openai")
    sys.exit(1)


@dataclass
class TestCase:
    context: str
    reading: str
    correct_answer: str
    source_text: str


class LLMSelectionBenchmark:
    def __init__(self, api_key: str, model: str = "gpt-5"):
        self.client = openai.OpenAI(api_key=api_key)
        self.model = model
        self.mozc_client = MozcClient()

    def _load_cases_from_file(self, pattern_file: str = "extracted_pattern_code.txt") -> List[TestCase]:
        path = Path(pattern_file)
        if not path.exists():
            return []
        try:
            content = path.read_text(encoding="utf-8")
        except Exception as e:
            print(f"Failed to read {pattern_file}: {e}")
            return []

        patterns = re.findall(r'TestCase\([^)]+\)', content, re.DOTALL)
        cases: List[TestCase] = []
        for pat in patterns:
            m_ctx = re.search(r'context="([^"]*)"', pat)
            m_rd  = re.search(r'reading="([^"]*)"', pat)
            m_ok  = re.search(r'correct_answer="([^"]*)"', pat)
            m_src = re.search(r'source_text="([^"]*)"', pat)
            if not (m_ctx and m_rd and m_ok and m_src):
                continue
            cases.append(TestCase(
                context=m_ctx.group(1),
                reading=m_rd.group(1),
                correct_answer=m_ok.group(1),
                source_text=m_src.group(1)
            ))
        return cases

    def run_llm_selection(self, test_case: TestCase, candidates: List[Dict[str, str]]) -> str:
        if not candidates:
            return test_case.reading

        candidates_text = "\n".join([f"{i+1}. {c['candidate']}" for i, c in enumerate(candidates)])

        prompt = f"""
以下の文脈で、「{test_case.reading}」を最も適切な漢字に変換してください。

要件:
- 文中の [reading] 部分を、候補のいずれかで置換したときに、文全体として自然で正しい現代日本語になるものを選ぶこと。
- 複数の候補が成立する場合は、現代日本語で一般的な表記を優先すること。
- 回答は候補番号のみ（例: 1）。

文脈: {test_case.context}

変換候補:
{candidates_text}

回答は候補番号のみ答えてください（例: 1）。
"""

        try:
            params: Dict = {
                "model": self.model,
                "messages": [
                    {"role": "system", "content": "あなたは日本語の文脈に基づいて最適な漢字変換を選択するアシスタントです。"},
                    {"role": "user", "content": prompt},
                ],
            }
            # gpt-5 系は temperature 指定不可・max_completion_tokens 指定が必要
            if str(self.model).startswith("gpt-5"):
                params["max_completion_tokens"] = 10
            else:
                params["temperature"] = 0.1
                params["max_tokens"] = 10

            response = self.client.chat.completions.create(**params)
            llm_response = response.choices[0].message.content.strip()
            m = re.search(r"\d+", llm_response)
            if m:
                idx = int(m.group()) - 1
                if 0 <= idx < len(candidates):
                    return candidates[idx]["candidate"]
        except Exception as e:
            print(f"LLM selection error: {e}")

        return candidates[0]["candidate"] if candidates else test_case.reading

    def evaluate_single_case(self, test_case: TestCase) -> Dict:
        candidates = self.mozc_client.get_conversion_candidates(
            test_case.reading,
            test_case.context,
            max_candidates=6,
        )

        llm_selection = self.run_llm_selection(test_case, candidates)
        mozc_top = candidates[0]["candidate"] if candidates else test_case.reading

        def _is_hiragana(text: str) -> bool:
            return bool(re.fullmatch(r"[ぁ-ん]+", text))

        llm_is_correct = (llm_selection == test_case.correct_answer) or _is_hiragana(llm_selection)

        return {
            "test_case": {
                "context": test_case.context,
                "reading": test_case.reading,
                "correct_answer": test_case.correct_answer,
                "source_text": test_case.source_text,
            },
            "candidates": candidates,
            "llm_selection": llm_selection,
            "mozc_top": mozc_top,
            "llm_correct": llm_is_correct,
            "mozc_correct": mozc_top == test_case.correct_answer,
            "improvement": (llm_selection == test_case.correct_answer) and (mozc_top != test_case.correct_answer),
        }

    def run_benchmark(self, data_files: List[str] = None, output_file: str = None):
        print("=== LLM Selection Benchmark ===")
        print(f"Model: {self.model}")

        cases = self._load_cases_from_file()
        if not cases:
            print("Error: extracted_pattern_code.txt が見つからないか、テストケースが0件です。")
            print("先に scripts/extract_patterns_from_corpus.py を実行して extracted_pattern_code.txt を生成してください。")
            return
        print(f"Using extracted test cases from file: {len(cases)}")
        print(f"\nTotal test cases: {len(cases)}")

        if output_file is None:
            # モデル名をファイル名に反映
            safe_model_name = self.model.replace("-", "_").replace("/", "_")
            output_file = f"results/{safe_model_name}.json"

        results: List[Dict] = []
        for i, tc in enumerate(cases):
            print(f"Evaluating case {i+1}/{len(cases)}: {tc.reading}")
            r = self.evaluate_single_case(tc)
            results.append(r)
            if r["llm_correct"]:
                print(f"  ✓ LLM correct: {r['llm_selection']}")
            else:
                print(f"  ✗ LLM incorrect: {r['llm_selection']} (correct: {tc.correct_answer})")
            time.sleep(0.5)

        self._save_results(results, output_file)
        self._print_summary(results)

    def _save_results(self, results: List[Dict], output_file: str):
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)
        summary = self._calculate_summary(results)
        data = {
            "metadata": {
                "model": self.model,
                "total_cases": len(results),
                "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            },
            "summary": summary,
            "details": results,
        }
        Path(output_file).write_text(json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8")
        print(f"\nResults saved to: {output_file}")

    def _calculate_summary(self, results: List[Dict]) -> Dict:
        total = len(results)
        llm_correct = sum(1 for r in results if r["llm_correct"]) 
        mozc_correct = sum(1 for r in results if r["mozc_correct"]) 
        improvements = sum(1 for r in results if r["improvement"]) 
        return {
            "total_cases": total,
            "llm_accuracy": llm_correct / total if total else 0,
            "mozc_accuracy": mozc_correct / total if total else 0,
            "llm_correct_count": llm_correct,
            "mozc_correct_count": mozc_correct,
            "improvements": improvements,
            "improvement_rate": improvements / total if total else 0,
        }

    def _print_summary(self, results: List[Dict]):
        s = self._calculate_summary(results)
        print("\n=== Benchmark Results ===")
        print(f"Total test cases: {s['total_cases']}")
        print(f"LLM accuracy: {s['llm_accuracy']:.1%} ({s['llm_correct_count']}/{s['total_cases']})")
        print(f"Mozc accuracy: {s['mozc_accuracy']:.1%} ({s['mozc_correct_count']}/{s['total_cases']})")
        print(f"LLM improvements: {s['improvements']} cases ({s['improvement_rate']:.1%})")


def main():
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        print("Please set OPENAI_API_KEY environment variable")
        return
    model = os.getenv("OPENAI_MODEL", "gpt-5")
    LLMSelectionBenchmark(api_key, model).run_benchmark()


if __name__ == "__main__":
    main()
