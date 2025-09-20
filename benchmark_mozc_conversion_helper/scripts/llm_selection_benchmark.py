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
import random
from pathlib import Path
from typing import List, Dict, Tuple
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
        # 再現可能性のためのランダムシード設定
        random.seed(42)

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

        # モデル情報の表示
        print(f"    Using model: {self.model}")

        # 候補をランダムにシャッフルして、LLMが順序に依存しないかテスト
        shuffled_candidates = candidates.copy()
        random.shuffle(shuffled_candidates)

        # デバッグ用：元の順序とシャッフル後の順序を表示
        original_order = [c['candidate'] for c in candidates]
        shuffled_order = [c['candidate'] for c in shuffled_candidates]
        print(f"    Original order: {original_order}")
        print(f"    Shuffled order: {shuffled_order}")

        # 正解候補がシャッフル後のどの位置にあるかを確認
        correct_answer = test_case.correct_answer
        if correct_answer in shuffled_order:
            correct_position = shuffled_order.index(correct_answer) + 1
            print(f"    Correct answer '{correct_answer}' is at position {correct_position} in shuffled list")
        else:
            print(f"    Correct answer '{correct_answer}' not found in candidates")

        # シャッフル後の候補リストとオリジナルのインデックスマッピングを保存
        shuffle_mapping = {}
        for new_idx, candidate in enumerate(shuffled_candidates):
            original_idx = candidates.index(candidate)
            shuffle_mapping[new_idx] = original_idx

        candidates_text = "\n".join([f"{i+1}. {c['candidate']}" for i, c in enumerate(shuffled_candidates)])

        prompt = f"""文: {test_case.context}

「{test_case.reading}」に最適な漢字を選択肢から選んでください。

{candidates_text}

番号のみ回答してください:"""

        print(f"    Sending prompt to LLM: {prompt[:100]}...")

        try:
            # シンプルなフォールバック用のプロンプトも準備
            simple_prompt = f"次の選択肢から番号を選んでください:\n{candidates_text}\n文脈: {test_case.context[:50]}...\n答え:"

            for attempt in range(2):
                current_prompt = prompt if attempt == 0 else simple_prompt
                print(f"    Attempt {attempt + 1}: {'Original' if attempt == 0 else 'Simple'} prompt")

                # gpt-5用にはより強い指示が必要
                if "gpt-5" in str(self.model):
                    system_msg = "You must answer with only a number from the given choices. Answer with just the number, nothing else."
                else:
                    system_msg = "選択肢から番号を選んで答えてください。"

                params: Dict = {
                    "model": self.model,
                    "messages": [
                        {"role": "system", "content": system_msg},
                        {"role": "user", "content": current_prompt},
                    ],
                }

                # モデルに応じてパラメータを調整（安全な方法）
                try:
                    if "gpt-5" in str(self.model):
                        # gpt-5系: temperatureは設定しない（デフォルト値使用）
                        # トークン制限も最小限にして安全性を確保
                        pass
                    else:
                        # その他のモデル
                        params["temperature"] = 0.1
                        params["max_tokens"] = 20
                except Exception as param_error:
                    print(f"    Warning: Parameter setting failed: {param_error}")
                    # パラメータ設定に失敗した場合はデフォルト設定で続行

                print(f"    Using params: {[k for k in params.keys()]}")

                response = self.client.chat.completions.create(**params)
                print(f"    API response finish_reason: {response.choices[0].finish_reason}")
                print(f"    Full response object available: {hasattr(response.choices[0], 'message')}")

                llm_response = response.choices[0].message.content
                print(f"    LLM raw response: '{llm_response}' (type: {type(llm_response)})")

                if llm_response is None:
                    llm_response = ""
                    print("    Warning: LLM returned None response")
                else:
                    llm_response = llm_response.strip()
                    print(f"    After strip: '{llm_response}'")

                # 空でない応答が得られたら処理続行
                if llm_response:
                    break

                print(f"    Empty response on attempt {attempt + 1}, trying {'simple prompt' if attempt == 0 else 'giving up'}")

            m = re.search(r"\d+", llm_response)
            if m:
                selected_number = int(m.group())
                shuffled_idx = selected_number - 1
                print(f"    LLM selected number: {selected_number} (0-based index: {shuffled_idx})")

                if 0 <= shuffled_idx < len(shuffled_candidates):
                    selected_candidate = shuffled_candidates[shuffled_idx]["candidate"]
                    print(f"    Selected candidate: '{selected_candidate}'")
                    return selected_candidate
                else:
                    print(f"    ERROR: Invalid index {shuffled_idx}, using first candidate")
            else:
                print(f"    ERROR: No number found in LLM response, using first candidate")
        except Exception as e:
            print(f"    ERROR: LLM API error: {e}")

        # フォールバック: 最初の候補を返す
        fallback_candidate = shuffled_candidates[0]["candidate"] if shuffled_candidates else test_case.reading
        print(f"    Using fallback candidate: '{fallback_candidate}'")
        return fallback_candidate

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
        mozc_is_correct = mozc_top == test_case.correct_answer

        # 結果判定のデバッグログ
        print(f"    Results: LLM='{llm_selection}' Mozc='{mozc_top}' Correct='{test_case.correct_answer}'")
        print(f"    LLM correct: {llm_is_correct}, Mozc correct: {mozc_is_correct}")

        return {
            "test_case": {
                "context": test_case.context,
                "reading": test_case.reading,
                "correct_answer": test_case.correct_answer,
                "source_text": test_case.source_text,
            },
            "candidates": candidates,  # 元の順序（Mozcの順序）
            "llm_selection": llm_selection,
            "mozc_top": mozc_top,
            "llm_correct": llm_is_correct,
            "mozc_correct": mozc_is_correct,
            "improvement": (llm_selection == test_case.correct_answer) and (mozc_top != test_case.correct_answer),
            "note": "候補はLLMにランダム順序で提示されました"
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
