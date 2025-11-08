#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
SCOWL (Spell Checker Oriented Word Lists) から英単語辞書を生成する

このスクリプトは、SCOWLの単語リストファイルから英単語を抽出し、
Emacs Lispのハッシュテーブル形式で出力します。

データソース: http://wordlist.aspell.net/
ライセンス: Public Domain (SCOWL)
"""

import sys
import re


def is_valid_word(word):
    """
    単語が有効かどうかをチェックする

    基準:
    - 3-10文字の英単語のみ
    - アルファベットのみで構成される（ハイフン、アポストロフィなどを除外）
    """
    # 3-10文字の制限
    if len(word) < 3 or len(word) > 10:
        return False

    # アルファベットのみかチェック
    if not word.isalpha():
        return False

    return True


def load_words(wordlist_filename):
    """
    単語リストファイルを読み込んで、有効な単語のみを抽出する

    Args:
        wordlist_filename: SCOWLの単語リストファイルパス

    Returns:
        有効な単語のセット（重複なし、ソート済み）
    """
    words = set()

    # SCOWL ファイルは latin-1 エンコーディングを使用している
    with open(wordlist_filename, 'r', encoding='latin-1') as f:
        for line in f:
            # 前後の空白を削除
            word = line.strip()

            # 空行やコメント行をスキップ
            if not word or word.startswith('#'):
                continue

            # 小文字に正規化
            word_lower = word.lower()

            # 有効な単語のみ追加
            if is_valid_word(word_lower):
                words.add(word_lower)

    # ソートして返す
    return sorted(words)


def generate_emacs_lisp(words):
    """
    単語リストからEmacs Lispのハッシュテーブル定義を生成する

    Args:
        words: 単語のリスト（ソート済み）
    """
    # ファイルヘッダー
    print(';;; sumibi-english-words.el --- English words dictionary for romaji-to-hiragana conversion  -*- lexical-binding: t; -*-')
    print()
    print(';; This file is generated from SCOWL (Spell Checker Oriented Word Lists).')
    print(';; Source: http://wordlist.aspell.net/')
    print(';; SCOWL Version: 2020.12.07')
    print(';; SCOWL License: Public Domain')
    print(';;')
    print(';; As SCOWL is in the Public Domain, this file can be freely distributed')
    print(';; under any license, including GPL.')
    print()
    print(';;; Commentary:')
    print(';; This file contains a hash table of common English words (3-10 letters)')
    print(';; used by sumibi-romaji-to-hiragana to detect and preserve English words')
    print(';; during romaji-to-hiragana conversion.')
    print(';;')
    print(';; Total words: {}'.format(len(words)))
    print()
    print(';;; Code:')
    print()

    # ハッシュテーブル定義の開始
    print('(defconst sumibi--english-words-hash')
    print('  (let ((ht (make-hash-table :test \'equal :size {})))'.format(len(words) + 100))

    # 単語を追加
    for word in words:
        print('    (puthash "{}" t ht)'.format(word))

    # ハッシュテーブル定義の終了
    print('    ht)')
    print('  "Hash table of common English words (3-10 letters).")')
    print()

    # provide文
    print('(provide \'sumibi-english-words)')
    print(';;; sumibi-english-words.el ends here')


def main(argv):
    """
    メイン関数

    Args:
        argv: コマンドライン引数
    """
    if len(argv) < 2:
        print("Usage: {} <wordlist_file>".format(argv[0]), file=sys.stderr)
        print("Example: {} scowl-2020.12.07/final/english-words.10".format(argv[0]), file=sys.stderr)
        sys.exit(1)

    wordlist_file = argv[1]

    try:
        # 単語を読み込む
        words = load_words(wordlist_file)

        # Emacs Lispを生成
        generate_emacs_lisp(words)

    except FileNotFoundError:
        print("Error: File not found: {}".format(wordlist_file), file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print("Error: {}".format(str(e)), file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main(sys.argv)
