
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

全てのテストが正常に動作し、新仕様が正しく実装されていることを確認しました。
