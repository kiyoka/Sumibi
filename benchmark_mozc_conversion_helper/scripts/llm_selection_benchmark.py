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

# OpenAIライブラリのバージョンを確認
OPENAI_VERSION = tuple(map(int, openai.__version__.split('.')[:2]))
SUPPORTS_REASONING_EFFORT = OPENAI_VERSION >= (1, 50)  # reasoning_effortは1.50.0以降でサポート


@dataclass
class TestCase:
    context: str
    reading: str
    correct_answer: str
    source_text: str

class LLMSelectionBenchmark:
    def __init__(self, api_key: str, model: str = "gpt-5", base_url: str = None, prompt_mode: str = "rerank-all"):
        """
        prompt_mode: "rerank-all" (全候補並び替え) or "optimize" (最適化モード: 1つだけ選択+短縮文脈)
        """
        self.prompt_mode = prompt_mode
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

        # gpt-5の場合はreasoning_effortをminimalに設定（ただしライブラリがサポートしている場合のみ）
        if "gpt-5" in str(model) and SUPPORTS_REASONING_EFFORT:
            self.reasoning_effort = "minimal"
        elif "gpt-5" in str(model) and not SUPPORTS_REASONING_EFFORT:
            self.reasoning_effort = None
            print(f"Warning: openai library version {openai.__version__} does not support reasoning_effort")
            print(f"         Please upgrade to 1.50.0 or later: pip install --upgrade openai")
        else:
            self.reasoning_effort = None

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

    def _parse_llm_ranked_list(self, content: str) -> List[str]:
        """
        LLMの順位出力を頑健に分割（sumibi.elのsumibi--parse-llm-ranked-listと同じロジック）
        カンマ/改行/番号/箇条書きに対応
        """
        # \rを削除
        s = content.replace('\r', '')

        # 箇条書き記号の前にカンマを追加（・を残すため、先に処理）
        s = re.sub(r'([-•])', r',\1', s)

        # 日本語の句読点をカンマに変換（ただし箇条書きの・は除く）
        s = re.sub(r'[、，]', ',', s)
        # 改行をカンマに変換
        s = s.replace('\n', ',')
        # 番号付き記号の前にカンマを追加
        s = re.sub(r'([0-9]+[.)．、:])', r',\1', s)

        # カンマで分割
        items = [item.strip() for item in s.split(',') if item.strip()]

        # 各アイテムから番号や箇条書き記号を削除
        result = []
        for item in items:
            # 先頭の番号や箇条書き記号を削除
            cleaned = re.sub(r'^([0-9]+[.)．、:]|[-•])\s*', '', item)
            # 括弧内の説明を削除（例: "安い (most appropriate)" -> "安い"）
            cleaned = re.sub(r'\s*[\(（].*?[\)）]\s*', '', cleaned)
            # 引用符や鉤括弧を削除
            cleaned = re.sub(r'["\'"「」『』]', '', cleaned)
            cleaned = cleaned.strip()
            if cleaned:
                result.append(cleaned)

        return result

    def run_llm_selection(self, test_case: TestCase, candidates: List[Dict[str, str]]) -> Tuple[str, float]:
        if not candidates:
            return test_case.reading, 0.0

        # モデル情報の表示
        print(f"    Using model: {self.model}")

        # 文脈を変換対象の前方10文字に短縮
        context = test_case.context
        # [reading]のマーカーを探して、その前の10文字を抽出
        marker = f"[{test_case.reading}]"
        if marker in context:
            marker_pos = context.find(marker)
            # マーカーの前方10文字を取得（先頭からの場合は利用可能な範囲で）
            start_pos = max(0, marker_pos - 10)
            shortened_context = context[start_pos:marker_pos + len(marker)]
        else:
            # マーカーが見つからない場合は元の文脈を使用
            shortened_context = context

        print(f"    Original context: {context}")
        print(f"    Shortened context: {shortened_context}")

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

        candidates_list = [c['candidate'] for c in shuffled_candidates]
        candidates_text = ", ".join(candidates_list)
        cand_count = len(candidates_list)

        if self.prompt_mode == "optimize":
            # 最適化モード: 最適な1つだけを選択（短縮した文脈を使用）
            prompt = f"""文脈: {shortened_context}

「{test_case.reading}」の変換候補({cand_count}件): {candidates_text}

出力ルール:
- 文脈に最も適した候補を1つだけ選んで出力する。
- 候補リストの中から選び、内容は一切変更しない。
- 追加の説明や引用符・記号・見出しは書かない。
- 出力形式: 候補1つのみ

では、文脈に最も適した候補を1つ選んでください。"""
        else:
            # モード2 (デフォルト): 全候補を並び替え（短縮した文脈を使用）
            prompt = f"""文脈: {shortened_context}

「{test_case.reading}」の変換候補({cand_count}件): {candidates_text}

出力ルール:
- 次の候補だけを順序付けし、内容は一切変更しない。
- 全ての候補({cand_count}件)を重複なく必ず一度ずつ含める。
- 追加の説明や引用符・記号・見出しは書かない。
- 出力形式: 候補1, 候補2, ..., 候補{cand_count}

では、文脈に最も適した順に並べ替えてください。"""

        print(f"    Sending prompt to LLM: {prompt[:100]}...")

        total_api_time = 0.0
        try:
            # シンプルなフォールバック用のプロンプトも準備（sumibi.elのプロンプトと同じ形式）
            simple_prompt = prompt

            for attempt in range(2):
                current_prompt = prompt if attempt == 0 else simple_prompt
                print(f"    Attempt {attempt + 1}: {'Original' if attempt == 0 else 'Simple'} prompt")

                # モデルタイプとプロンプトモードに応じてシステムメッセージを調整
                if self.prompt_mode == "optimize":
                    # optimizeモード用システムメッセージ
                    if self.base_url:
                        system_msg = "あなたは日本語の変換候補を文脈に応じて選択する専門家です。与えられた候補から最適なものを1つ選んでください。"
                    elif "gpt-5" in str(self.model):
                        system_msg = "You are a Japanese text conversion expert. Select the most appropriate candidate according to the context."
                    else:
                        system_msg = "あなたは日本語の変換候補を文脈に応じて選択する専門家です。"
                else:
                    # rerank-allモード用システムメッセージ
                    if self.base_url:
                        system_msg = "あなたは日本語の変換候補を文脈に応じて並び替える専門家です。与えられた候補を最適な順序に並び替えてください。"
                    elif "gpt-5" in str(self.model):
                        system_msg = "You are a Japanese text conversion expert. Reorder the given candidates according to the context."
                    else:
                        system_msg = "あなたは日本語の変換候補を文脈に応じて並び替える専門家です。"

                params: Dict = {
                    "model": self.model,
                    "messages": [
                        {"role": "system", "content": system_msg},
                        {"role": "user", "content": current_prompt},
                    ],
                }

                # モデルに応じてパラメータを調整（sumibi_typical_convert_client.pyと同じ方法）
                if self.base_url:
                    # ローカルLLM: より安全な設定
                    params["temperature"] = 0.7
                    # optimizeモードではmax_tokensを削減
                    params["max_tokens"] = 20 if self.prompt_mode == "optimize" else 50
                    params["top_p"] = 0.9
                    print("    Using local LLM parameters")
                else:
                    # OpenAI API: その他のモデル用の設定
                    if "gpt-5" not in str(self.model):
                        params["temperature"] = 0.8
                        # optimizeモードではmax_tokensを削減
                        params["max_tokens"] = 10 if self.prompt_mode == "optimize" else 20

                # reasoning_effortが設定されている場合のみ追加（gpt-5の場合）
                if self.reasoning_effort is not None:
                    params["reasoning_effort"] = self.reasoning_effort
                    print(f"    Using reasoning_effort={self.reasoning_effort}")

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
                print(f"    LLM raw response: '{llm_response[:200]}...' (type: {type(llm_response)})")

                if llm_response is None:
                    llm_response = ""
                    print("    Warning: LLM returned None response")
                else:
                    # gpt-5の<reasoning>タグを削除（sumibi_typical_convert_client.pyと同じ処理）
                    llm_response = re.sub(r'<reasoning>.*?</reasoning>', '', llm_response, flags=re.DOTALL).strip()
                    llm_response = llm_response.strip()
                    print(f"    After strip and removing reasoning tags: '{llm_response[:200]}...'")

                # 空でない応答が得られたら処理続行
                if llm_response:
                    break

                # 1回目で失敗した場合、ベンチマークを停止
                if attempt == 0:
                    print(f"    ERROR: Empty response on first attempt. Stopping benchmark.")
                    raise RuntimeError(f"LLM returned empty response on first attempt for test case: {test_case.reading}")

                print(f"    Empty response on attempt {attempt + 1}, trying {'simple prompt' if attempt == 0 else 'giving up'}")

            # sumibi.elの sumibi--parse-llm-ranked-list と同じパースロジック
            ranked_list = self._parse_llm_ranked_list(llm_response)
            print(f"    Parsed ranked list: {ranked_list}")

            if ranked_list and len(ranked_list) > 0:
                # LLMの結果から実際の候補と一致するもののみを順序を保って抽出
                # (sumibi.elの sumibi-local-llm-rank-candidates と同じロジック)
                reordered_candidates = []
                used_candidates = set()

                # LLMが返した順序で候補を追加（重複を避ける）
                for ranked in ranked_list:
                    # 元の候補リストから一致する候補を探す
                    for cand_dict in shuffled_candidates:
                        candidate = cand_dict['candidate']
                        if candidate == ranked and candidate not in used_candidates:
                            reordered_candidates.append(candidate)
                            used_candidates.add(candidate)
                            break

                # LLMで処理されなかった候補を末尾に追加
                for cand_dict in shuffled_candidates:
                    candidate = cand_dict['candidate']
                    if candidate not in used_candidates:
                        reordered_candidates.append(candidate)

                print(f"    LLMが返した候補数: {len(ranked_list)}/{len(shuffled_candidates)}")
                print(f"    Reordered candidates: {reordered_candidates}")

                # 並び替えられた候補の最初を選択
                if reordered_candidates:
                    selected_candidate = reordered_candidates[0]
                    print(f"    Selected candidate (first in reordered list): '{selected_candidate}'")
                    print(f"    Total API time for this case: {total_api_time:.3f}s")
                    return selected_candidate, total_api_time
                else:
                    print(f"    ERROR: Reordered list is empty")
            else:
                print(f"    ERROR: Failed to parse ranked list from LLM response")
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

    def run_benchmark(self, data_files: List[str] = None, output_file: str = None, hardware: str = None):
        print("=== LLM Selection Benchmark ===")
        print(f"Model: {self.model}")
        print(f"Prompt Mode: {self.prompt_mode}")
        if hardware:
            print(f"Hardware: {hardware}")

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
            results_dir = f"results/{hardware}" if hardware else "results"
            # prompt_modeが optimize の場合はファイル名に反映
            mode_suffix = f"-{self.prompt_mode}" if self.prompt_mode == "optimize" else ""
            output_file = f"{results_dir}/{safe_model_name}{mode_suffix}.json"

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
                "prompt_mode": self.prompt_mode,
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
    hardware = os.getenv("HARDWARE")  # hardware1 or hardware2
    prompt_mode = os.getenv("PROMPT_MODE", "rerank-all")  # rerank-all or optimize

    # API_KEYが "dummy" の場合は、ローカルLLMを使用
    if api_key == "dummy" and base_url:
        print("Using local LLM (API key is dummy)")

    LLMSelectionBenchmark(api_key, model, base_url, prompt_mode).run_benchmark(hardware=hardware)


if __name__ == "__main__":
    main()
