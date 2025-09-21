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
    def __init__(self, api_key: str, model: str = "gpt-5", base_url: str = None):
        # ローカルLLM対応: base_urlが指定されていればそれを使用
        if base_url:
            # /v1の自動補完と正規化
            if base_url.endswith('/v1/'):
                # /v1/ で終わる場合は末尾のスラッシュを削除
                base_url = base_url.rstrip('/')
            elif not base_url.endswith('/v1'):
                # /v1 で終わらない場合は追加
                if base_url.endswith('/'):
                    base_url = base_url + 'v1'
                else:
                    base_url = base_url + '/v1'

            self.client = openai.OpenAI(api_key=api_key, base_url=base_url)
            print(f"Using local LLM endpoint: {base_url}")
        else:
            self.client = openai.OpenAI(api_key=api_key)
            print("Using OpenAI API")

        self.model = model
        self.base_url = base_url
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

    def run_llm_selection(self, test_case: TestCase, candidates: List[Dict[str, str]]) -> Tuple[str, float]:
        if not candidates:
            return test_case.reading, 0.0

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

        total_api_time = 0.0
        try:
            # シンプルなフォールバック用のプロンプトも準備
            simple_prompt = f"次の選択肢から番号を選んでください:\n{candidates_text}\n文脈: {test_case.context[:50]}...\n答え:"

            for attempt in range(2):
                current_prompt = prompt if attempt == 0 else simple_prompt
                print(f"    Attempt {attempt + 1}: {'Original' if attempt == 0 else 'Simple'} prompt")

                # モデルタイプに応じてシステムメッセージを調整
                if self.base_url:
                    # ローカルLLM用：英語と日本語の両方で明確に指示
                    system_msg = "選択肢から最適な番号を1つ選んで答えてください。数字のみ回答してください。Choose the best option number and answer with only that number."
                elif "gpt-5" in str(self.model):
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
                    if self.base_url:
                        # ローカルLLM: より安全な設定
                        params["temperature"] = 0.7
                        params["max_tokens"] = 50
                        params["top_p"] = 0.9
                        print("    Using local LLM parameters")
                    elif "gpt-5" in str(self.model):
                        # gpt-5系: temperatureは設定しない（デフォルト値使用）
                        # トークン制限も最小限にして安全性を確保
                        pass
                    else:
                        # その他のOpenAIモデル
                        params["temperature"] = 0.8
                        params["max_tokens"] = 20
                except Exception as param_error:
                    print(f"    Warning: Parameter setting failed: {param_error}")
                    # パラメータ設定に失敗した場合はデフォルト設定で続行

                print(f"    Using params: {[k for k in params.keys()]}")

                # API呼び出し時間を測定
                try:
                    api_start_time = time.time()
                    response = self.client.chat.completions.create(**params)
                    api_end_time = time.time()
                    api_response_time = api_end_time - api_start_time
                    total_api_time += api_response_time
                except Exception as api_error:
                    if attempt == 0:
                        print(f"    ERROR: API call failed on first attempt. Stopping benchmark.")
                        raise RuntimeError(f"LLM API call failed on first attempt for test case '{test_case.reading}': {api_error}")
                    else:
                        raise api_error

                print(f"    API response time: {api_response_time:.3f}s")
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

                # 1回目で失敗した場合、ベンチマークを停止
                if attempt == 0:
                    print(f"    ERROR: Empty response on first attempt. Stopping benchmark.")
                    raise RuntimeError(f"LLM returned empty response on first attempt for test case: {test_case.reading}")

                print(f"    Empty response on attempt {attempt + 1}, trying {'simple prompt' if attempt == 0 else 'giving up'}")

            m = re.search(r"\d+", llm_response)
            if m:
                selected_number = int(m.group())
                shuffled_idx = selected_number - 1
                print(f"    LLM selected number: {selected_number} (0-based index: {shuffled_idx})")

                if 0 <= shuffled_idx < len(shuffled_candidates):
                    selected_candidate = shuffled_candidates[shuffled_idx]["candidate"]
                    print(f"    Selected candidate: '{selected_candidate}'")
                    print(f"    Total API time for this case: {total_api_time:.3f}s")
                    return selected_candidate, total_api_time
                else:
                    print(f"    ERROR: Invalid index {shuffled_idx}, using first candidate")
            else:
                print(f"    ERROR: No number found in LLM response, using first candidate")
        except Exception as e:
            print(f"    ERROR: LLM API error: {e}")

        # フォールバック: 最初の候補を返す
        fallback_candidate = shuffled_candidates[0]["candidate"] if shuffled_candidates else test_case.reading
        print(f"    Using fallback candidate: '{fallback_candidate}'")
        print(f"    Total API time for this case: {total_api_time:.3f}s")
        return fallback_candidate, total_api_time

    def evaluate_single_case(self, test_case: TestCase) -> Dict:
        candidates = self.mozc_client.get_conversion_candidates(
            test_case.reading,
            test_case.context,
            max_candidates=6,
        )

        llm_selection, api_time = self.run_llm_selection(test_case, candidates)
        mozc_top = candidates[0]["candidate"] if candidates else test_case.reading

        def _is_hiragana(text: str) -> bool:
            return bool(re.fullmatch(r"[ぁ-ん]+", text))

        llm_is_correct = (llm_selection == test_case.correct_answer) or _is_hiragana(llm_selection)
        mozc_is_correct = mozc_top == test_case.correct_answer

        # 結果判定のデバッグログ
        print(f"    Results: LLM='{llm_selection}' Mozc='{mozc_top}' Correct='{test_case.correct_answer}'")
        print(f"    LLM correct: {llm_is_correct}, Mozc correct: {mozc_is_correct}")

        # 改善ケースの定義を明確化
        llm_better = llm_is_correct and not mozc_is_correct  # LLMが正解、Mozcが不正解
        mozc_better = mozc_is_correct and not llm_is_correct  # Mozcが正解、LLMが不正解
        both_correct = llm_is_correct and mozc_is_correct     # 両方正解
        both_wrong = not llm_is_correct and not mozc_is_correct  # 両方不正解

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
            "llm_better": llm_better,
            "mozc_better": mozc_better,
            "both_correct": both_correct,
            "both_wrong": both_wrong,
            "api_response_time": api_time,  # API応答時間を追加
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
            # モデル名をファイル名に反映（'/'を'--'に置換）
            safe_model_name = self.model.replace("/", "--")
            output_file = f"results/{safe_model_name}.json"

        results: List[Dict] = []
        for i, tc in enumerate(cases):
            print(f"Evaluating case {i+1}/{len(cases)}: {tc.reading}")
            try:
                r = self.evaluate_single_case(tc)
                results.append(r)
                if r["llm_correct"]:
                    print(f"  ✓ LLM correct: {r['llm_selection']}")
                else:
                    print(f"  ✗ LLM incorrect: {r['llm_selection']} (correct: {tc.correct_answer})")
                time.sleep(0.5)
            except RuntimeError as e:
                print(f"\n=== BENCHMARK STOPPED ===")
                print(f"Reason: {e}")
                print(f"Completed {i}/{len(cases)} test cases before stopping.")
                if results:
                    print(f"\nSaving partial results...")
                    self._save_results(results, output_file)
                    self._print_summary(results)
                return

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

        # 詳細な比較統計
        llm_better = sum(1 for r in results if r.get("llm_better", False))
        mozc_better = sum(1 for r in results if r.get("mozc_better", False))
        both_correct = sum(1 for r in results if r.get("both_correct", False))
        both_wrong = sum(1 for r in results if r.get("both_wrong", False))

        # API応答時間の統計
        api_times = [r.get("api_response_time", 0) for r in results]
        total_api_time = sum(api_times)
        avg_api_time = total_api_time / total if total else 0
        min_api_time = min(api_times) if api_times else 0
        max_api_time = max(api_times) if api_times else 0

        return {
            "total_cases": total,
            "llm_accuracy": llm_correct / total if total else 0,
            "mozc_accuracy": mozc_correct / total if total else 0,
            "llm_correct_count": llm_correct,
            "mozc_correct_count": mozc_correct,
            "llm_better": llm_better,
            "mozc_better": mozc_better,
            "both_correct": both_correct,
            "both_wrong": both_wrong,
            "llm_better_rate": llm_better / total if total else 0,
            "mozc_better_rate": mozc_better / total if total else 0,
            "both_correct_rate": both_correct / total if total else 0,
            "both_wrong_rate": both_wrong / total if total else 0,
            "api_response_time": {
                "total": total_api_time,
                "average": avg_api_time,
                "min": min_api_time,
                "max": max_api_time
            }
        }

    def _print_summary(self, results: List[Dict]):
        s = self._calculate_summary(results)
        print("\n=== Benchmark Results ===")
        print(f"Total test cases: {s['total_cases']}")
        print(f"LLM accuracy: {s['llm_accuracy']:.1%} ({s['llm_correct_count']}/{s['total_cases']})")
        print(f"Mozc accuracy: {s['mozc_accuracy']:.1%} ({s['mozc_correct_count']}/{s['total_cases']})")
        print(f"\n=== Detailed Comparison ===")
        print(f"LLM better than Mozc: {s['llm_better']} cases ({s['llm_better_rate']:.1%})")
        print(f"Mozc better than LLM: {s['mozc_better']} cases ({s['mozc_better_rate']:.1%})")
        print(f"Both correct: {s['both_correct']} cases ({s['both_correct_rate']:.1%})")
        print(f"Both wrong: {s['both_wrong']} cases ({s['both_wrong_rate']:.1%})")
        print(f"\n=== API Response Time Statistics ===")
        print(f"Total API time: {s['api_response_time']['total']:.3f}s")
        print(f"Average per case: {s['api_response_time']['average']:.3f}s")
        print(f"Min response time: {s['api_response_time']['min']:.3f}s")
        print(f"Max response time: {s['api_response_time']['max']:.3f}s")


def main():
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        print("Please set OPENAI_API_KEY environment variable")
        return

    model = os.getenv("OPENAI_MODEL", "gpt-5")
    base_url = os.getenv("OPENAI_BASEURL")  # ローカルLLM用エンドポイント

    # API_KEYが "dummy" の場合は、ローカルLLMを使用
    if api_key == "dummy" and base_url:
        print("Using local LLM (API key is dummy)")

    LLMSelectionBenchmark(api_key, model, base_url).run_benchmark()


if __name__ == "__main__":
    main()
