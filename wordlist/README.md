# English Words Dictionary Generator

このディレクトリには、SCOWL (Spell Checker Oriented Word Lists) から英単語辞書を生成するスクリプトが含まれています。

## 概要

`sumibi-english-words.el` は、ローマ字→ひらがな変換時に英単語を検出・保持するための辞書ファイルです。このファイルは SCOWL プロジェクトの単語リストから自動生成されます。

## データソース

- **プロジェクト名**: SCOWL (Spell Checker Oriented Word Lists)
- **URL**: http://wordlist.aspell.net/
- **バージョン**: 2020.12.07
- **ライセンス**: Public Domain
- **使用ファイル**: `final/english-words.10` (基本的な英単語)

## ファイル構成

- `Makefile` - ビルド自動化スクリプト
- `gen_english_words.py` - 辞書生成 Python スクリプト
- `README.md` - このファイル

## 使用方法

### 1. 全自動生成（推奨）

```bash
cd wordlist
make all
```

このコマンドは以下を実行します：
1. SCOWL tarball をダウンロード
2. tarball を展開
3. `sumibi-english-words.el` を生成

### 2. 段階的な実行

#### ダウンロードのみ

```bash
make download
```

- SCOWL tarball をダウンロードして展開します
- 生成されるファイル: `scowl-2020.12.07.tar.gz`
- 展開先: `scowl-2020.12.07/`

#### 生成のみ

```bash
make generate
```

- ダウンロード済みの SCOWL データから辞書ファイルを生成します
- 生成先: `../lisp/sumibi-english-words.el`

### 3. クリーンアップ

```bash
make clean
```

以下のファイル・ディレクトリを削除します：
- `scowl-2020.12.07/`
- `scowl-2020.12.07.tar.gz`
- `../lisp/sumibi-english-words.el`

## 生成される辞書の仕様

### 単語の選択基準

- **文字数**: 3-10 文字の英単語のみ
- **文字種**: アルファベットのみ（ハイフン、アポストロフィなどを除外）
- **総単語数**: 約 3,679 語

### ファイル形式

Emacs Lisp のハッシュテーブル形式：

```elisp
(defconst sumibi--english-words-hash
  (let ((ht (make-hash-table :test 'equal :size 3779)))
    (puthash "test" t ht)
    (puthash "hello" t ht)
    (puthash "attention" t ht)
    ...
    ht)
  "Hash table of common English words (3-10 letters).")
```

### 検索性能

- **データ構造**: ハッシュテーブル
- **検索速度**: O(1)
- **メモリ使用量**: 約 104KB

## カスタマイズ

### 単語数を増やす

`gen_english_words.py` の `is_valid_word()` 関数を編集：

```python
def is_valid_word(word):
    # 文字数制限を変更（例: 3-12 文字）
    if len(word) < 3 or len(word) > 12:
        return False
    ...
```

### 別の SCOWL レベルを使用

`Makefile` の `WORDLIST_FILE` を変更：

```makefile
# english-words.10 → より多くの単語を含む english-words.20 に変更
WORDLIST_FILE = $(SCOWL_DIR)/final/english-words.20
```

利用可能なレベル:
- `english-words.10` - 基本単語（最小）
- `english-words.20` - 一般的な単語
- `english-words.35` - やや専門的な単語
- `english-words.40` - 専門的な単語
- `english-words.50` - 高度に専門的な単語（最大）

## トラブルシューティング

### ダウンロードに失敗する

SourceForge のミラーサーバーが一時的に利用できない可能性があります。時間をおいて再試行してください。

### UTF-8 デコードエラー

SCOWL ファイルは `latin-1` エンコーディングを使用しています。`gen_english_words.py` は自動的に `latin-1` で読み込むよう設定されています。

### 生成されたファイルが読み込めない

```bash
# Emacs Lisp の構文チェック
emacs --batch -l ../lisp/sumibi-english-words.el
```

## ライセンス

- **SCOWL データ**: Public Domain
- **このスクリプト**: GPL-2.0（Sumibi プロジェクトに準拠）

SCOWL が Public Domain であるため、生成された辞書ファイルは GPL との互換性があり、ソースコードに直接埋め込むことができます。

## 参考リンク

- [SCOWL 公式サイト](http://wordlist.aspell.net/)
- [SCOWL GitHub](https://github.com/en-wl/wordlist)
- [Issue #97 - 英単語検出・保持機能](https://github.com/kiyoka/Sumibi/issues/97)
