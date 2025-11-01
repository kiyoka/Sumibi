
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

Sumibiは、AI (ChatGPT API) を使用したEmacs用の日本語入力メソッドです。モードレス入力が特徴で、入力モードの切り替えなしに日本語を入力できます。

## 開発コマンド

### ビルドとリリース
```bash
# リリースアーカイブ (tar.gz) の作成
make release

# 生成物のクリーンアップ
make clean
```

### テスト
```bash
# ERT (Emacs Regression Testing) テストの実行
make test
```

### リント／構文チェック
Emacs Lispファイルを編集した後は、**必ず**括弧バランスチェックツールを実行してください：

```bash
agent-lisp-paren-aid-linux lisp/sumibi.el
```

もし括弧の不整合が検出されたら：
1. 他の編集作業はせず、指摘された行番号の括弧を修正
2. 再度 `agent-lisp-paren-aid-linux` を実行して確認
3. すべての括弧が整合してから次の作業へ

**重要**: LLMはLisp括弧を数えるのが苦手なため、自分で数えたり考えたりせず、必ずこのツールを使うようにしてください。

### GitHub連携

ghコマンドはインストールされていませんので、issueの内容を確認する時は、以下のようなURLを直接開いてください。
https://github.com/kiyoka/Sumibi/issues/53

## アーキテクチャ

### ディレクトリ構造
- `lisp/` - メインのEmacs Lispソースコード
  - `sumibi.el` - メイン実装 (v3.5.0)
  - `sumibi-localdic.el` - ローカル辞書サポート
- `test/` - ERTテストファイル
- `benchmark/` - パフォーマンスベンチマークツール
- `skkdic/` - SKK辞書関連ファイル

### 主要コンポーネント
1. **AI変換エンジン**: OpenAI/Gemini/DeepSeek APIを使用してローマ字を日本語に変換
2. **Mozcバックエンド**: オフライン環境用のフォールバック
3. **ポップアップUI**: 候補選択インターフェース
4. **履歴管理**: より良いコンテキスト理解のための変換履歴

### 依存関係
- Emacs >= 29.0
- popup >= 0.5.9
- unicode-escape >= 1.1
- deferred >= 0.5.1
- mozc (オプション)

## 編集プロセス

- sumibi.el を編集した後は、必ず agent-lisp-paren-aid-linux を実行して、閉じ括弧が合っているか確認してください。

もし括弧が整合していない場合は、修正すべき行番号を教えてくれます。

```
agent-lisp-paren-aid-linux lisp/sumibi.el
```

もし不整合が検出されたら他の編集作業はせず、一旦指摘された行番号に括弧を補う修正のみを行って、
再度 agent-lisp-paren-aid-linux を実行するようにしてください。
LLMはLisp括弧を数えるのが苦手なため、自分で数えたり考えたりせず、必ずこのツールを使うようにしてください。

## 外部ライブラリ

もし、ちょっとした手元のテストコードで以下のライブラリを読み込みたくなった場合は、 ~/.emacs.d/elpa/ から検索してパスを追加してください

(require 'cl-lib)
(require 'popup)
(require 'url)
(require 'url-http)
(require 'unicode-escape)
(require 'deferred)
(require 'sumibi-localdic)


github issue 97に対する現在のワークの実装状態を以下に記載してください。

## GitHub Issue #97 実装状態

### 概要

GitHub Issue #97「ローマ字→ひらがな事前変換によるLocal LLM精度向上」の実装が完了しました。

### 実装完了項目

#### 1. カスタマイズ変数の追加

**ファイル**: `lisp/sumibi.el` (318-338行目)

```elisp
(defcustom sumibi-typo-correction t
  "Non-nil の場合、タイプミス補正機能を有効化する（ローマ字のまま LLM に送る）。

  この変数は、LLM に送る入力形式を制御します：

    - t (デフォルト): タイプミス補正 ON
      ローマ字のまま LLM に送信します。
      LLM がタイプミスを吸収してくれるため、入力エラーに強くなります。
      例: \"shimasit\" → LLM が \"しました\" と正しく解釈

    - nil: タイプミス補正 OFF（精度重視モード）
      ローマ字を事前にひらがなに変換してから LLM に送信します。
      Issue #96 のベンチマーク結果により、Local LLM の変換精度が
      大幅に向上することが実証されました（エラー率 29-42% 削減）。
      約 10B パラメータの小型モデルでも実用的な変換精度を実現できます。
      ただし、タイプミス吸収機能は失われます。

  英単語は変換されず、ローマ字のまま保持されます。
  変換できない不正なローマ字もそのまま保持されます。"
  :type 'boolean
  :group 'sumibi)
```

**効果**: ユーザーがタイプミス補正と精度重視を切り替え可能

#### 2. ローマ字→ひらがな変換テーブル

**ファイル**: `lisp/sumibi.el` (345-396行目)

70+エントリの変換テーブルを実装：
- 3文字の拗音・外来音（kya, sha, chi, tsu など）
- 2文字の組み合わせ（ka, ki, shi, ji など）
- 1文字の母音（a, i, u, e, o）
- 特殊処理（nna, nni など → んな、んに）

**特徴**:
- 最長一致アルゴリズムで変換
- 促音（っ）の自動検出（tt, kk, pp など）
- n の特別処理（子音前で ん に変換）

#### 3. ローマ字→ひらがな変換関数

**ファイル**: `lisp/sumibi.el` (398-467行目)

```elisp
(defun sumibi-romaji-to-hiragana (romaji-str &optional preserve-english)
  "ローマ字文字列をひらがなに変換する。

  ROMAJI-STR: 変換対象のローマ字文字列
  PRESERVE-ENGLISH: 非nil の場合、英単語を保持（未実装、将来の拡張用）

  戻り値: ひらがなに変換された文字列

  最長一致アルゴリズムで変換し、変換できない部分はそのまま保持。"
  ...)
```

**アルゴリズム**:
1. 促音チェック（子音の重複 → っ）
2. n の特別処理（子音前 → ん）
3. 最長一致で変換テーブル検索
4. マッチしない文字はそのまま保持

**性能**: O(n×m) - n: 文字列長, m: テーブルエントリ数（最適化済み）

#### 4. 既存関数への統合

**ファイル**: `lisp/sumibi.el` (1107-1120行目)

`sumibi-roman-to-kanji-with-surrounding` 関数に統合：

```elisp
(let* ((split (sumibi--split-markdown-prefix roman))
       (prefix (car split))
       (core-roman (cdr split))
       ;; タイプミス補正のチェック (Issue #97)
       (processed-roman
        (if (and (not sumibi-typo-correction)  ; 補正OFF（精度重視モード）の場合
                 (not (sumibi-backend-mozc-p)))
            (sumibi-romaji-to-hiragana core-roman t) ; ひらがなに変換
          core-roman)))  ; 補正ON の場合はローマ字のまま
  ;; デバッグ出力
  (sumibi-debug-print (format "  sumibi-typo-correction: %s\n" sumibi-typo-correction))
  (sumibi-debug-print (format "  core-roman (入力): %s\n" core-roman))
  (sumibi-debug-print (format "  processed-roman (LLMへ送信): %s\n" processed-roman))
  ...)
```

**処理フロー**:
1. Markdownプレフィックスを抽出
2. `sumibi-typo-correction` が nil かつ非Mozcバックエンドの場合、ひらがなに変換
3. 変換結果をデバッグ出力
4. LLMに送信

#### 5. デバッグ機能

**ファイル**: `lisp/sumibi.el` (1118-1120行目)

デバッグモード（`sumibi-debug` が `t`）で以下を `*sumibi-debug*` バッファに出力：
- タイプミス補正の設定値
- 入力されたローマ字文字列
- LLMに送信される文字列（変換後）

**使用例**:
```elisp
(setq sumibi-debug t)
(setq sumibi-typo-correction nil)
;; 変換実行時に *sumibi-debug* に出力される
```

#### 6. ユニットテスト

**ファイル**: `test/sumibi-romaji-to-hiragana-test.el` (214行、20個のテストケース)

**テストカバレッジ**:
- 基本変換（母音、子音、よく使う単語）
- 特殊文字（拗音、促音、ん、長音）
- 濁音・半濁音
- エッジケース（不正シーケンス、大文字小文字、数字記号）
- 複雑な文章（Issue #96ベンチマーク例を含む）
- ローマ字表記バリエーション（shi/si, chi/ti, tsu/tu, fu/hu）

**テスト結果**: ✅ **全43テスト合格 (43/43)**

**Makefile統合**: (79行目、87行目)
```makefile
-l test/sumibi-romaji-to-hiragana-test.el \
```

### 期待される効果（Issue #96ベンチマーク結果より）

#### Local LLMでの精度向上

| モデル | CER削減率 | @1精度向上 |
|--------|----------|-----------|
| gemma-3-12b-it-qat (12B) | -41.3% | +300% |
| llm-jp-3.1-13b-instruct4 (13B) | -41.9% | +∞ |
| openai/gpt-oss-20b (20B) | -37.8% | +350% |
| sarashina2.2-3b-instruct-v0.1 (3B) | -29.3% | +∞ |

### 残課題（Phase 2）

以下の機能は将来の拡張として保留中：

1. **英単語検出・保持機能**
   - 現状: 英単語もひらがなに変換されてしまう可能性
   - 今後: 英単語辞書を使った検出機能の追加を検討

2. **不正ローマ字の完全保持**
   - 現状: 基本的な保持は実装済み
   - 今後: より高度な検証ロジックの追加を検討

### 使用方法

#### タイプミス補正モード（デフォルト）

```elisp
;; デフォルト設定（何もしなくてもOK）
(setq sumibi-typo-correction t)
;; ローマ字のままLLMに送信 → タイプミスを吸収
```

#### 精度重視モード（Local LLM推奨）

```elisp
;; 精度重視モードに切り替え
(setq sumibi-typo-correction nil)
;; ローマ字→ひらがな変換してからLLMに送信 → 精度向上
```

### 実装完了日

2025年11月1日

### 関連ファイル

- `lisp/sumibi.el`: 本体実装
- `test/sumibi-romaji-to-hiragana-test.el`: ユニットテスト
- `Makefile`: テスト統合
- `CLAUDE.md`: 実装ドキュメント

sumibi-romaji-to-hiragana 関数の実装に不備があるため、修正してください。
test/sumibi-romaji-to-hiragana-test.el
の期待結果が間違っています。
文節に1箇所でもローマ字に変換できない箇所があれば、その文節は元のまま変更しないでください。期待結果は以下の様にしてください。
  (should (string= "shimasit" (sumibi-romaji-to-hiragana "shimasit")))
  (should (string= "axyz" (sumibi-romaji-to-hiragana "axyz")))
  (should (string= "tesutoq" (sumibi-romaji-to-hiragana "tesutoq"))))
同様に以下の2箇所も誤りです。
  (should (string= "あxyz" (sumibi-romaji-to-hiragana "axyz"))))
  (should (string= "わたし123" (sumibi-romaji-to-hiragana "watashi123"))))

続きをお願いします。

### 仕様変更: 変換不可能文字の全文節保護

**修正日**: 2025年11月1日

#### 変更内容

`sumibi-romaji-to-hiragana` 関数の仕様を変更しました。

**旧仕様**: 部分的に変換
- 変換できない文字があっても、変換可能な部分だけ変換
- 例: "shimasit" → "しましt"、"axyz" → "あxyz"

**新仕様**: 全文節保護
- **文節に1箇所でも変換できない文字があれば、文節全体を元のまま返す**
- 例: "shimasit" → "shimasit"、"axyz" → "axyz"

#### 変換可能な文字の定義

- **許容される文字**:
  - ローマ字（a-z, A-Z）
  - ハイフン（-）: 長音（ー）として変換

- **変換不可能な文字**（これらが1つでも含まれると文節全体を保持）:
  - 数字（0-9）
  - 記号（!@#$,. など、ハイフン以外）
  - 変換テーブルにない不正なローマ字パターン（xyz、単独のq、単独のtなど）

#### 修正箇所

**1. lisp/sumibi.el (399-482行目)**

関数のロジックを全面的に書き直し：

```elisp
(defun sumibi-romaji-to-hiragana (romaji-str &optional preserve-english)
  "ローマ字文字列をひらがなに変換する。

重要: 文字列に1箇所でも変換できない文字が含まれている場合、
      文字列全体を変換せずに元のまま返す。"
  (let ((result '())
        (pos 0)
        (len (length romaji-str))
        (romaji-lower (downcase romaji-str))
        (all-convertible t))  ; 全て変換可能かを追跡
    (if (= len 0)
        romaji-str
      (progn
        (while (and (< pos len) all-convertible)
          (let ((matched nil)
                (current-char (aref romaji-lower pos)))
            ;; ハイフン、促音、変換テーブルのチェック
            ...
            ;; マッチしなかった場合は変換不可
            (unless matched
              (setq all-convertible nil))))
        ;; 全て変換できた場合のみ変換結果を返す
        (if all-convertible
            (apply 'concat (nreverse result))
          romaji-str)))))
```

**主な変更点**:
- `all-convertible` フラグを追加して変換可能性を追跡
- 変換不可能な文字に遭遇したら即座にフラグを false に設定
- 最後に `all-convertible` をチェックし、true の場合のみ変換結果を返す
- false の場合は元の文字列をそのまま返す

**2. test/sumibi-romaji-to-hiragana-test.el**

テストケースの期待値を修正：

- **test-romaji-to-hiragana-invalid-sequence** (117-119行目):
  ```elisp
  ;; 旧: "しましt", "あxyz", "てすとq"
  ;; 新: "shimasit", "axyz", "tesutoq"
  (should (string= "shimasit" (sumibi-romaji-to-hiragana "shimasit")))
  (should (string= "axyz" (sumibi-romaji-to-hiragana "axyz")))
  (should (string= "tesutoq" (sumibi-romaji-to-hiragana "tesutoq")))
  ```

- **test-romaji-to-hiragana-preserve-unconvertible** (184行目):
  ```elisp
  ;; 旧: "あxyz"
  ;; 新: "axyz"
  (should (string= "axyz" (sumibi-romaji-to-hiragana "axyz")))
  ```

- **test-romaji-to-hiragana-numbers-and-symbols** (211行目):
  ```elisp
  ;; 旧: "わたし123"
  ;; 新: "watashi123"
  (should (string= "watashi123" (sumibi-romaji-to-hiragana "watashi123")))
  ```

- **test-romaji-to-hiragana-benchmark-examples** (194-195行目):
  ```elisp
  ;; 旧: "こんにちは,げんきですか"
  ;; 新: "konnitiha,genkidesuka" (カンマが含まれるため全体を保持)
  (should (string= "konnitiha,genkidesuka"
                   (sumibi-romaji-to-hiragana "konnitiha,genkidesuka")))
  ```

#### 変更の理由

この仕様変更により、以下の利点があります：

1. **予測可能な動作**: ユーザーは変換結果が完全か未変換かを明確に判断できる
2. **データ保護**: 変換できない文字が含まれる場合、部分的な変換で意味が変わることを防ぐ
3. **エラー検出**: 変換されなかった文節を見ることで、タイプミスや不正な入力を発見しやすい

#### テスト結果

✅ **全43テスト合格 (43/43)**

```
Ran 43 tests, 43 results as expected, 0 unexpected
```

TODOとなっていた、以下の機能を実装してください。キーワードは、SCOWLを使います。理由はソースコードにそのまま埋め込んでも網羅的にGPLで問題ないためです。
取得先URLはwget http://downloads.sourceforge.net/wordlist/scowl-2020.12.07.tar.gz です。
1. **英単語検出・保持機能**
   - 現状: 英単語もひらがなに変換されてしまう可能性
   - 今後: 英単語辞書を使った検出機能の追加を検討

続きをお願いします。

sumibi.elの括弧の対応は人間の私が修正します。

### 英単語検出・保持機能の実装完了

**実装日**: 2025年11月1日

#### 概要

SCOWL (Spell Checker Oriented Word Lists) を使用した英単語検出・保持機能を実装しました。これにより、`sumibi-romaji-to-hiragana` 関数が英単語を正しく識別し、ひらがなに変換せずに保持できるようになりました。

#### 実装内容

**1. 英単語辞書ファイルの作成**

**ファイル**: `lisp/sumibi-english-words.el` (約50KB、1880単語)

- **データソース**: SCOWL 2020.12.07 (Public Domain)
  - URL: http://downloads.sourceforge.net/wordlist/scowl-2020.12.07.tar.gz
  - ファイル: `final/english-words.10` (基本的な英単語)
- **抽出条件**: 3-6文字の英単語のみ
  - 理由: ファイルサイズの最小化と、一般的な英単語のカバー
- **実装方式**: ハッシュテーブル (高速検索のため)
- **ライセンス**: Public Domain (GPLで問題なく使用可能)

```elisp
(defconst sumibi--english-words-hash
  (let ((ht (make-hash-table :test 'equal :size 1980)))
    (puthash "test" t ht)
    (puthash "hello" t ht)
    (puthash "world" t ht)
    ...
    ht)
  "Hash table of common English words (3-6 letters).")
```

**含まれる単語の例**:
- 基本単語: test, hello, world, about, think, make, take, etc.
- 動詞活用形: added, adding, adopted, etc.
- 一般的な名詞: access, action, address, etc.

**2. 英単語検出関数の実装**

**ファイル**: `lisp/sumibi.el` (402-409行目)

```elisp
(defun sumibi--is-english-word (word)
  "WORD が英単語辞書に含まれているかチェックする。

辞書が利用可能な場合はハッシュテーブルで高速検索を行う。
辞書が利用できない場合は nil を返す。"
  (and (boundp 'sumibi--english-words-hash)
       sumibi--english-words-hash
       (gethash (downcase word) sumibi--english-words-hash)))
```

**特徴**:
- ハッシュテーブルによるO(1)検索
- 大文字小文字を区別しない (`downcase` で正規化)
- 辞書が利用できない場合は nil を返す (graceful degradation)

**3. sumibi-romaji-to-hiragana 関数の拡張**

**ファイル**: `lisp/sumibi.el` (456-457行目)

```elisp
;; 英単語検出: preserve-english が non-nil で、辞書に含まれる場合は保持
(if (and preserve-english (sumibi--is-english-word romaji-str))
    romaji-str
  (progn
    ...))
```

**動作**:
1. `preserve-english` パラメータが non-nil の場合
2. 入力文字列が英単語辞書に含まれているかチェック
3. 含まれている場合は元の文字列をそのまま返す（変換しない）
4. 含まれていない場合は通常のローマ字→ひらがな変換処理を実行

**4. テストケースの追加**

**ファイル**: `test/sumibi-romaji-to-hiragana-test.el` (214-250行目)

追加したテスト（4個のテストケース）:

1. **test-romaji-to-hiragana-english-word-preservation**
   - 英単語が `preserve-english=t` で保持されることを確認
   - テスト単語: test, hello, world, about, think, Test, HELLO

2. **test-romaji-to-hiragana-english-without-preserve**
   - `preserve-english=nil` の場合の動作を確認
   - 変換不可能な文字があるため全体が保持される

3. **test-romaji-to-hiragana-non-dictionary-words**
   - 辞書にない単語の処理を確認
   - テスト単語: wikipedia, xyz

4. **test-romaji-to-hiragana-japanese-not-affected**
   - 日本語のローマ字変換が影響を受けないことを確認
   - `preserve-english` の値に関わらず正しく変換される

#### 使用方法

```elisp
;; 英単語を保持する（推奨）
(sumibi-romaji-to-hiragana "test" t)    ; → "test"
(sumibi-romaji-to-hiragana "hello" t)   ; → "hello"
(sumibi-romaji-to-hiragana "WORLD" t)   ; → "WORLD"

;; 英単語も変換を試みる（変換不可能なら保持）
(sumibi-romaji-to-hiragana "test" nil)  ; → "test" (変換不可のため保持)

;; 日本語は常に変換される
(sumibi-romaji-to-hiragana "watashi" t)   ; → "わたし"
(sumibi-romaji-to-hiragana "watashi" nil) ; → "わたし"
```

#### 性能

- **辞書サイズ**: 1880単語
- **ファイルサイズ**: 約50KB
- **検索速度**: O(1) (ハッシュテーブル)
- **メモリ使用量**: 約50KB (辞書読み込み時)

#### 制限事項

1. **文字数制限**: 3-6文字の英単語のみ対象
   - 理由: ファイルサイズの最小化
   - 影響: 7文字以上の単語（例: wikipedia）は検出されない

2. **基本単語のみ**: SCOWL english-words.10 レベルの単語のみ
   - 専門用語や固有名詞は含まれない可能性がある

3. **大文字小文字**: 保存時は元の大文字小文字を保持
   - 検索は大文字小文字を区別しない

#### 今後の拡張案

1. **辞書の拡充**:
   - より多くの単語を含むレベル（english-words.20, 35など）の使用
   - 専門用語辞書の追加

2. **動的辞書更新**:
   - ユーザーカスタム辞書のサポート
   - 実行時に辞書を追加・更新する機能

3. **統計ベースの判定**:
   - 辞書にない単語でも、統計的に英単語らしさを判定

#### 関連ファイル

- `lisp/sumibi-english-words.el`: 英単語辞書（新規作成）
- `lisp/sumibi.el`: 英単語検出機能の実装
- `test/sumibi-romaji-to-hiragana-test.el`: テストケース
- `/tmp/generate_wordlist.py`: 辞書生成スクリプト（一時ファイル）

#### テスト結果

括弧の修正後、以下のコマンドでテストを実行してください：

```bash
make test
```

期待される結果: 全47テスト合格 (43 + 4個の新規テスト)

エラーが発生しました。


make test
# デフォルトはモック版でテスト実行
# SUMIBI_TEST_USE_MOCK=0 make test で本物のmozc_serverを使用
Running tests with mock (default)...
../../.emacs.d/elpa/unicode-escape-20230109.1222/unicode-escape-autoloads.el: Warning: Unknown type: char-or-string

Error: error ("Loading file /mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/lisp/sumibi-english-words.el failed to provide feature ‘sumibi-english-words’")
  mapbacktrace(#f(compiled-function (evald func args flags) #<bytecode 0x19c2bab927ae87e7>))
  debug-early-backtrace()
  debug-early(error (error "Loading file /mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/lisp/sumibi-english-words.el failed to provide feature ‘sumibi-english-words’"))
  require(sumibi-english-words nil noerror)
  load-with-code-conversion("/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/lisp/sumibi.el" "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/lisp/sumibi.el" nil t)
  require(sumibi)
  load-with-code-conversion("/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/test/sumibi-mozc-tests.el" "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/test/sumibi-mozc-tests.el" nil t)
  command-line-1(("-L" "lisp" "--eval" "(setq sumibi-test-use-mozc-mock t)" "-l" "test/sumibi-mozc-tests.el" "-l" "test/sumibi-romaji-to-hiragana-test.el" "-f" "ert-run-tests-batch-and-exit"))
  command-line()
  normal-top-level()
Loading file /mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/lisp/sumibi-english-words.el failed to provide feature 'sumibi-english-words'
make: *** [Makefile:71: test] Error 255

### エラー1の修正: sumibi-english-words.el

**問題**: `(provide 'sumibi-english-words)` が欠けていた

**修正内容**:
1. ファイルヘッダーを追加：
   ```elisp
   ;;; sumibi-english-words.el --- English words dictionary for romaji-to-hiragana conversion  -*- lexical-binding: t; -*-
   ```

2. `;;; Code:` セクションを追加

3. ファイル末尾に以下を追加：
   ```elisp
   (provide 'sumibi-english-words)
   ;;; sumibi-english-words.el ends here
   ```

**修正済み**: ✅

### エラー2: sumibi.el の括弧エラー

再度テストを実行すると、新しいエラーが発生：

```
Invalid read syntax: ")", 500, 29
Error: invalid-read-syntax (")" 500 29)
```

**問題**: `lisp/sumibi.el` の500行目29文字目に構文エラー

**現在の500行目**:
```elisp
            romaji-str)))))))
```

**必要な修正**: 括弧のバランス調整（ユーザーが修正予定）

**修正箇所**: `lisp/sumibi.el` 456-500行目付近の `sumibi-romaji-to-hiragana` 関数

括弧の対応を手当てします。

sumibi.elの括弧の対応を修正しました。

