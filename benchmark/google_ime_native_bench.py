#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
mozcのかな漢字変換のベンチマークを実行して、結果をJSONで出力するプログラムです。

mozcはローカルで動作するIMEエンジンで、ひらがな入力からかな漢字変換を行います。
このスクリプトはmozcのコマンドラインツール (mozc_emacs_helper) を使用して変換を実行します。

使用方法:
  python3 mozc_bench.py <evaluation_json_file> <output_json_file>

前提条件:
  - mozcがインストールされていること
  - macOS: brew install mozc (Homebrew) または手動ビルド
  - Linux: sudo apt install mozc-utils-gui (Ubuntu/Debian)
  - mozc_emacs_helper コマンドが利用可能であること
"""

import sys
import os
import json
import time
import subprocess
from katakana_to_hiragana_converter import KatakanaToHiraganaConverter
from importlib.machinery import SourceFileLoader

# Dynamically load AJIMEE-Bench/utils.py
script_dir = os.path.dirname(os.path.abspath(__file__))
utils_path = os.path.join(script_dir, "AJIMEE-Bench", "utils.py")
ajimee_utils = SourceFileLoader("ajimee_utils", utils_path).load_module()


class MozcClient:
    """
    mozcのコマンドラインインターフェースを使用してかな漢字変換を行うクライアント。

    mozc_emacs_helper を使用して、ひらがな入力をかな漢字変換します。
    mozc_emacs_helper が利用できない場合は、mozc_server + mozc_tool を試みます。
    """

    def __init__(self):
        """mozcクライアントを初期化します。"""
        self.mozc_helper = self._find_mozc_helper()
        if self.mozc_helper is None:
            raise RuntimeError(
                "mozcのコマンドラインツールが見つかりません。\n"
                "以下のいずれかをインストールしてください:\n"
                "  macOS: brew install mozc\n"
                "  Linux (Ubuntu/Debian): sudo apt install emacs-mozc\n"
                "  Linux (Fedora): sudo dnf install emacs-mozc"
            )

    def _find_mozc_helper(self):
        """mozc_emacs_helper の実行パスを検索します。"""
        # 一般的なパスを検索
        candidates = [
            "mozc_emacs_helper",  # PATH上にある場合
            "/usr/local/bin/mozc_emacs_helper",  # macOS (Homebrew bin)
            "/usr/lib/mozc/mozc_emacs_helper",  # Linux (Debian/Ubuntu)
            "/usr/lib64/mozc/mozc_emacs_helper",  # Linux (Fedora)
            "/usr/local/lib/mozc/mozc_emacs_helper",  # 手動インストール
            "/opt/homebrew/lib/mozc/mozc_emacs_helper",  # macOS (Homebrew ARM)
            "/opt/homebrew/bin/mozc_emacs_helper",  # macOS (Homebrew ARM bin)
        ]
        for candidate in candidates:
            # which で検索、または絶対パスの場合は存在チェック
            if os.path.isabs(candidate):
                if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
                    return candidate
            else:
                result = subprocess.run(
                    ["which", candidate],
                    capture_output=True, text=True
                )
                if result.returncode == 0:
                    return result.stdout.strip()
        return None

    def convert(self, hiragana_text):
        """
        ひらがなテキストをmozcでかな漢字変換します。

        mozc_emacs_helper にひらがなをS式プロトコルで送信し、
        スペースで変換、Enterで確定して結果を返します。

        Args:
            hiragana_text: 変換対象のひらがなテキスト

        Returns:
            変換後の日本語テキスト（第1候補）
        """
        import re
        try:
            proc = subprocess.Popen(
                [self.mozc_helper],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )

            # mozc_emacs_helper のS式プロトコルに従ってコマンドを構築
            # フォーマット: (event_id command [session_id] [args...])
            commands = []
            event_id = 0

            # セッションを作成
            commands.append(f"({event_id} CreateSession)")
            event_id += 1

            # ひらがなを1文字ずつ送信（セッションID=1）
            for char in hiragana_text:
                commands.append(f'({event_id} SendKey 1 "{char}")')
                event_id += 1

            # スペースで変換を実行
            commands.append(f"({event_id} SendKey 1 space)")
            event_id += 1

            # Enterで確定
            commands.append(f"({event_id} SendKey 1 return)")
            event_id += 1

            # セッションを削除
            commands.append(f"({event_id} DeleteSession 1)")

            input_text = "\n".join(commands) + "\n"
            stdout, stderr = proc.communicate(input=input_text, timeout=30)

            # 確定結果（return送信後のレスポンス）からresultのvalueを抽出
            # 形式: (result . ((type . string)(value . "変換結果")...))
            lines = stdout.strip().split("\n")
            for line in reversed(lines):
                # result の value を探す
                match = re.search(r'\(result \. \(\(type \. string\)\(value \. "([^"]*)"\)', line)
                if match:
                    return match.group(1)

            # resultが見つからない場合、preeditのvalueから取得を試みる
            # （変換後スペース押下時のレスポンスから）
            for line in reversed(lines):
                match = re.search(r'\(annotation \. highlight\)\(value \. "([^"]*)"\)', line)
                if match:
                    return match.group(1)

            return hiragana_text

        except subprocess.TimeoutExpired:
            proc.kill()
            return hiragana_text
        except Exception as e:
            print(f"  => mozc変換エラー: {e}")
            return hiragana_text


class MozcBench:
    """
    mozcのかな漢字変換ベンチマークを実行するクラス。

    AJIMEE-Benchの評価データを使用して、Google日本語入力 (native) の変換精度を測定します。
    hiragana_input モードではカタカナ→ひらがなに変換してから渡します。
    katakana_input モードではカタカナをそのまま渡します。
    """

    def __init__(self, mode='hiragana_input'):
        """ベンチマークを初期化します。

        Args:
            mode: 'hiragana_input' または 'katakana_input'
        """
        self.mode = mode
        self.hiragana_converter = KatakanaToHiraganaConverter()
        self.client = MozcClient()
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
        Google日本語入力 (native) による変換を実行し、結果を記録します。

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
        print(f"mode:            'google_ime_native ({self.mode})'")
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
        print(f"WARMUP PHASE: Executing first entry to initialize ({self.mode})")
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
        print(f"\nGoogle日本語入力 (native / mozc_emacs_helper経由) のベンチマークを実行します。")
        print(f"\n前提条件:")
        print(f"  - Google日本語入力がインストールされていること")
        print(f"  - mozc_emacs_helper コマンドが利用可能であること")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]
    mode = sys.argv[3] if len(sys.argv) > 3 else 'hiragana_input'

    with open(input_path, 'r', encoding='utf-8') as f:
        evaluation_data = json.load(f)

    bench = MozcBench(mode=mode)
    bench.benchmark(evaluation_data)

    with open(output_path, 'w', encoding='utf-8') as fo:
        json.dump(bench.result_arr, fo, ensure_ascii=False, indent=2)

    print(f"\n結果を {output_path} に保存しました。")


if __name__ == "__main__":
    main()
