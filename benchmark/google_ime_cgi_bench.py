#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Google日本語入力 (Google CGI API) のかな漢字変換ベンチマークを実行して、
結果をJSONで出力するプログラムです。

Google CGI API for Japanese Input を使用して、ひらがな入力からかな漢字変換を行います。
APIエンドポイント: http://www.google.com/transliterate

使用方法:
  python3 google_ime_bench.py <evaluation_json_file> <output_json_file>

前提条件:
  - インターネット接続が必要
  - Google CGI API が利用可能であること
  - APIキーは不要
"""

import sys
import os
import json
import time
import urllib.request
import urllib.parse
from katakana_to_hiragana_converter import KatakanaToHiraganaConverter
from importlib.machinery import SourceFileLoader

# Dynamically load AJIMEE-Bench/utils.py
script_dir = os.path.dirname(os.path.abspath(__file__))
utils_path = os.path.join(script_dir, "AJIMEE-Bench", "utils.py")
ajimee_utils = SourceFileLoader("ajimee_utils", utils_path).load_module()


class GoogleIMEClient:
    """
    Google CGI API for Japanese Input を使用してかな漢字変換を行うクライアント。

    APIドキュメント:
    https://www.google.co.jp/ime/cgiapi.html

    エンドポイント:
    http://www.google.com/transliterate?langpair=ja-Hira|ja&text={ひらがなテキスト}

    レスポンス形式:
    JSON配列: [[入力1, [候補1, 候補2, ...]], [入力2, [候補1, ...]], ...]
    """

    API_URL = "http://www.google.com/transliterate"

    def __init__(self, timeout=30):
        """
        Google IME クライアントを初期化します。

        Args:
            timeout: APIリクエストのタイムアウト秒数
        """
        self.timeout = timeout

    def convert(self, hiragana_text):
        """
        ひらがなテキストをGoogle CGI APIでかな漢字変換します。

        Args:
            hiragana_text: 変換対象のひらがなテキスト

        Returns:
            変換後の日本語テキスト（各文節の第1候補を結合）
        """
        try:
            # URLエンコードしてリクエスト
            params = urllib.parse.urlencode({
                'langpair': 'ja-Hira|ja',
                'text': hiragana_text
            })
            url = f"{self.API_URL}?{params}"

            req = urllib.request.Request(url)
            req.add_header('User-Agent', 'AJIMEE-Bench/1.0')

            with urllib.request.urlopen(req, timeout=self.timeout) as response:
                # レスポンスを読み込み
                raw_data = response.read().decode('utf-8')

                # Google CGI API のレスポンスをパース
                # レスポンスは JSON 形式: [[input, [candidate1, candidate2, ...]], ...]
                result_data = json.loads(raw_data)

                # 各文節の第1候補を結合
                result_parts = []
                for segment in result_data:
                    if len(segment) >= 2 and len(segment[1]) > 0:
                        # 第1候補を使用
                        result_parts.append(segment[1][0])
                    else:
                        # 候補がない場合は入力をそのまま使用
                        result_parts.append(segment[0] if len(segment) > 0 else "")

                return "".join(result_parts)

        except urllib.error.URLError as e:
            print(f"  => API接続エラー: {e}")
            return hiragana_text
        except json.JSONDecodeError as e:
            print(f"  => JSONパースエラー: {e}")
            return hiragana_text
        except Exception as e:
            print(f"  => Google IME変換エラー: {e}")
            return hiragana_text


class GoogleIMEBench:
    """
    Google日本語入力 (Google CGI API) のベンチマークを実行するクラス。

    AJIMEE-Benchの評価データを使用して、Google IMEの変換精度を測定します。
    hiragana_input モードではカタカナ→ひらがなに変換してからAPIに渡します。
    katakana_input モードではカタカナをそのまま渡します。
    """

    def __init__(self, mode='hiragana_input'):
        """ベンチマークを初期化します。

        Args:
            mode: 'hiragana_input' または 'katakana_input'
        """
        self.mode = mode
        self.hiragana_converter = KatakanaToHiraganaConverter()
        self.client = GoogleIMEClient()
        self.result_arr = []

    def _convert_input(self, katakana_text):
        """モードに応じて入力テキストを変換する。"""
        if self.mode == 'katakana_input':
            return katakana_text
        else:
            return self.hiragana_converter.convert(katakana_text)

    def henkan(self, expected_output, input_text, katakana_text, context_text,
               splitted_input=None, skip_save=False):
        """
        Google IME (CGI API) による変換を実行し、結果を記録します。

        Args:
            expected_output: 期待される変換結果のリスト
            input_text: 変換対象のテキスト（ひらがなまたはカタカナ）
            katakana_text: カタカナの入力テキスト（表示用）
            context_text: コンテキストテキスト
            splitted_input: 分割入力データ（長い入力用）
            skip_save: Trueの場合、結果を保存しない（ウォームアップ用）
        """
        # 分割入力がある場合は分割して変換
        if splitted_input and len(splitted_input) > 0:
            parts = [self._convert_input(part) for part in splitted_input]
            start = time.perf_counter()
            result_parts = [self.client.convert(part) for part in parts]
            result = "".join(result_parts)
            end = time.perf_counter()
        else:
            start = time.perf_counter()
            result = self.client.convert(input_text)
            end = time.perf_counter()

        elapsed = end - start

        warmup_label = " [WARMUP - not saved]" if skip_save else ""
        print(f"  => elapsed: {elapsed:.2f} sec{warmup_label}")
        print(f"mode:            'google_ime_cgi ({self.mode})'")
        print(f"katakana_text:   '{katakana_text}'")
        print(f"input_text:      '{input_text}'")
        print(f"expect:          '{expected_output}'")
        print(f"result:          '{result}'\n")

        if not skip_save:
            cer = ajimee_utils.calculate_MinCER(expected_output, result)
            at1 = ajimee_utils.calculate_accuracy_at1(expected_output, result)
            self.result_arr.append({
                'surrounding_text': context_text + input_text,
                'henkan_text': input_text,
                'expect': expected_output,
                'result': result,
                'cer': cer,
                'at1': at1,
                'elapsed_sec': elapsed
            })

    def benchmark(self, evaluation_data):
        """
        ベンチマークを実行します。

        Args:
            evaluation_data: AJIMEE-Benchの評価データ（dictのリスト）
        """
        if len(evaluation_data) == 0:
            print("Warning: evaluation_data is empty")
            return

        # ウォームアップ: 最初のエントリを1回実行
        print("=" * 80)
        print(f"WARMUP PHASE: Executing first entry to warm up connection ({self.mode})")
        print("=" * 80)
        first_entry = evaluation_data[0]
        katakana_text = first_entry.get('input', '')
        input_text = self._convert_input(katakana_text)
        context_text = first_entry.get('context_text', '')
        expected_output = first_entry.get('expected_output', [])
        splitted = first_entry.get('splitted_input_for_limited_input_length', [])

        print("\nWarmup run:")
        self.henkan(expected_output, input_text, katakana_text, context_text,
                    splitted_input=splitted, skip_save=True)

        # ベンチマーク実行
        print("=" * 80)
        print("BENCHMARK PHASE: Executing all entries with result saving")
        print("=" * 80)

        for idx, entry in enumerate(evaluation_data, 1):
            print(f"\nBenchmark entry {idx}/{len(evaluation_data)}:")
            expected_output = entry.get('expected_output', [])
            context_text = entry.get('context_text', '')
            katakana_text = entry.get('input', '')
            input_text = self._convert_input(katakana_text)
            splitted = entry.get('splitted_input_for_limited_input_length', [])

            self.henkan(expected_output, input_text, katakana_text, context_text,
                        splitted_input=splitted, skip_save=False)


def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <evaluation_json_file> <output_json_file> [mode]")
        print(f"  mode: 'hiragana_input' (default) or 'katakana_input'")
        print(f"\nGoogle日本語入力 (Google CGI API) のベンチマークを実行します。")
        print(f"\n前提条件:")
        print(f"  - インターネット接続が必要")
        print(f"  - Google CGI API (http://www.google.com/transliterate) が利用可能であること")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]
    mode = sys.argv[3] if len(sys.argv) > 3 else 'hiragana_input'

    with open(input_path, 'r', encoding='utf-8') as f:
        evaluation_data = json.load(f)

    bench = GoogleIMEBench(mode=mode)
    bench.benchmark(evaluation_data)

    with open(output_path, 'w', encoding='utf-8') as fo:
        json.dump(bench.result_arr, fo, ensure_ascii=False, indent=2)

    print(f"\n結果を {output_path} に保存しました。")


if __name__ == "__main__":
    main()
