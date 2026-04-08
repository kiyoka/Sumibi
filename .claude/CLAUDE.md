
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

- macOSの場合のコマンドはこちら

```bash
agent-lisp-paren-aid lisp/sumibi.el
```
- Linuxの場合のコマンドはこちら

```bash
agent-lisp-paren-aid-linux lisp/sumibi.el
```

もし括弧の不整合が検出されたら：
1. 他の編集作業はせず、指摘された行番号の括弧を修正
2. 再度 `agent-lisp-paren-aid-linux` を実行して確認
3. すべての括弧が整合してから次の作業へ

**重要**: LLMはLisp括弧を数えるのが苦手なため、自分で数えたり考えたりせず、必ずこのツールを使うようにしてください。

**注意**: `agent-lisp-paren-aid` はフォーマット文字列内の `(%s)` などを括弧として誤検出することがあります。ツールがエラーを報告しても、Emacsの `M-x check-parens` で問題がなければ、誤検出と判断してください。


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
2. **ポップアップUI**: 候補選択インターフェース
3. **履歴管理**: より良いコンテキスト理解のための変換履歴

### 依存関係
- Emacs >= 29.0
- popup >= 0.5.9
- unicode-escape >= 1.1
- deferred >= 0.5.1

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

## GitHub Issue 95 実装計画: API Keyのセキュア管理

### 概要
LLMのAPI Keyをgpgやkeychainに保存し、セキュリティーを確保できるようにする機能を追加します。

### 実装する機能

#### 1. カスタマイズ変数の追加
```elisp
(defcustom sumibi-api-key-source 'environment
  "API Keyの取得元を指定します。
- 'environment: 環境変数から取得（従来の動作）
- 'auth-source-gpg: auth-sourceを使用してGPG暗号化ファイル (~/.authinfo.gpg) から取得
  ※ gpgコマンドが必要です。gpgが利用できない場合はエラーになります。
- 'auth-source-keychain: auth-sourceを使用してmacOS Keychainから取得
  ※ macOSでのみ利用可能です。macOS以外の環境ではエラーになります。"
  :type '(choice (const :tag "環境変数" environment)
                 (const :tag "auth-source (GPG)" auth-source-gpg)
                 (const :tag "auth-source (macOS Keychain)" auth-source-keychain))
  :group 'sumibi)
```

#### 2. auth-sourceのロードと可用性チェック関数
```elisp
;; auth-sourceは無条件でロード（Emacs 29.x以上で利用可能）
(require 'auth-source)

(defun sumibi-gpg-available-p ()
  "gpgコマンドが利用可能かチェックする。"
  (executable-find "gpg"))

(defun sumibi-macos-keychain-available-p ()
  "macOS Keychainが利用可能かチェックする（macOSかどうか）。"
  (eq system-type 'darwin))
```

#### 3. auth-source設定関数
```elisp
(defun sumibi-setup-auth-source-for-gpg ()
  "GPG用にauth-sourceを設定する。"
  (unless (sumibi-gpg-available-p)
    (error "gpgコマンドが見つかりません。auth-source-gpgを使用するにはGPGをインストールしてください"))
  ;; GPGファイルのみをターゲットに
  (setq auth-sources '("~/.authinfo.gpg")))

(defun sumibi-setup-auth-source-for-keychain ()
  "Keychain用にauth-sourceを設定する。"
  (unless (sumibi-macos-keychain-available-p)
    (error "macOS KeychainはmacOSでのみ利用可能です。現在のシステム: %s" system-type))
  ;; Keychainのみをターゲットに
  (setq auth-sources '(macos-keychain-internet macos-keychain-generic)))
```

#### 4. auth-sourceからAPI Keyを取得する関数
```elisp
(defun sumibi-get-api-key-from-auth-source ()
  "auth-sourceからAPI Keyを取得する。
hostとして 'api.openai.com' を使用し、loginは 'apikey' を想定。"
  (let* ((found (auth-source-search :host "api.openai.com"
                                     :user "apikey"
                                     :require '(:secret)
                                     :max 1))
         (secret (when found
                   (plist-get (car found) :secret))))
    (if (functionp secret)
        (funcall secret)
      secret)))
```

#### 5. 統合されたAPI Key取得関数
```elisp
(defun sumibi-get-api-key ()
  "設定に基づいてAPI Keyを取得する。
各ソースタイプで厳密なチェックを行う。"
  (cond
   ;; 環境変数から取得（従来の方法）
   ((eq sumibi-api-key-source 'environment)
    (or (getenv "SUMIBI_AI_API_KEY")
        (getenv "OPENAI_API_KEY")))

   ;; GPG経由でauth-sourceから取得
   ((eq sumibi-api-key-source 'auth-source-gpg)
    (sumibi-setup-auth-source-for-gpg)  ; GPGチェック + auth-sources設定
    (sumibi-get-api-key-from-auth-source))

   ;; macOS Keychain経由でauth-sourceから取得
   ((eq sumibi-api-key-source 'auth-source-keychain)
    (sumibi-setup-auth-source-for-keychain)  ; macOSチェック + auth-sources設定
    (sumibi-get-api-key-from-auth-source))))
```

### エラーメッセージ（日本語）

| 状況 | エラーメッセージ |
|------|------------------|
| GPGコマンドが存在しない | "gpgコマンドが見つかりません。auth-source-gpgを使用するにはGPGをインストールしてください" |
| macOS以外でkeychainを使用 | "macOS KeychainはmacOSでのみ利用可能です。現在のシステム: linux" |
| 環境変数が設定されていない | "API Keyが見つかりません。環境変数 SUMIBI_AI_API_KEY または OPENAI_API_KEY を設定するか、sumibi-api-key-source を適切に設定してください。" |
| auth-sourceでAPI Keyが見つからない | "API Keyが見つかりません。設定ファイルを確認してください" |

**注記**: auth-sourceはEmacs 29.x以上に標準で含まれているため、auth-source自体の可用性チェックは行いません。

### ユーザー向け設定例

#### 環境変数（デフォルト）
```elisp
(setq sumibi-api-key-source 'environment)
;; 環境変数 SUMIBI_AI_API_KEY または OPENAI_API_KEY を設定
```

#### GPG暗号化ファイル
```elisp
(setq sumibi-api-key-source 'auth-source-gpg)
;; ~/.authinfo.gpg に以下の形式で記述:
;; machine api.openai.com login apikey password sk-...
```

#### macOS Keychain
```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
;; macOSのKeychainでAPI Keyを登録
;; サーバー: api.openai.com
;; アカウント: apikey
;; パスワード: sk-...
```

### 実装のポイント

1. **厳密なターゲット指定**:
   - GPGモードでは `auth-sources` を `'("~/.authinfo.gpg")` のみに設定
   - Keychainモードでは `auth-sources` を `'(macos-keychain-internet macos-keychain-generic)` のみに設定

2. **事前チェックの徹底**:
   - GPGモード選択時は必ず `gpg` コマンドの存在を確認
   - Keychainモード選択時は必ず `system-type` が `darwin` であることを確認
   - auth-sourceはEmacs 29.x以上に標準で含まれているため、無条件で `(require 'auth-source)` を実行

3. **既存コードの更新箇所**:
   - lisp/sumibi.el:806-808 の起動時チェック
   - lisp/sumibi.el:879 の HTTP リクエスト時の Authorization ヘッダー
   - 上記2箇所で `(getenv ...)` の代わりに `(sumibi-get-api-key)` を使用

---

## GitHub Issue 95 実装完了報告

### 実装完了のお知らせ

Issue #95 で要望いただいた、LLMのAPI Keyをgpgやkeychainに保存してセキュリティーを確保する機能を実装しました。

### 実装した機能

#### 1. API Key取得元の選択機能
新しいカスタマイズ変数 `sumibi-api-key-source` を追加しました。以下の3つの取得方法から選択できます：

- `'environment` (デフォルト): 環境変数から取得（従来の動作）
- `'auth-source-gpg`: GPG暗号化ファイル (~/.authinfo.gpg) から取得
- `'auth-source-keychain`: macOS Keychainから取得

#### 2. 厳密なエラーチェック
各モードで適切なエラーチェックを実装しました：

- **GPGモード**: gpgコマンドの存在を確認。ない場合は日本語エラーメッセージを表示
- **Keychainモード**: macOS環境であることを確認。macOS以外の場合は日本語エラーメッセージを表示

#### 3. auth-sourceとの統合
Emacs標準のauth-sourceライブラリを活用し、以下の機能を実現しました：

- GPGモード: `~/.authinfo.gpg` のみをターゲットに設定
- Keychainモード: macOSのkeychainサービスのみをターゲットに設定

### 使用方法

#### 環境変数（デフォルト）
```elisp
;; 設定不要（従来通り）
;; 環境変数 SUMIBI_AI_API_KEY または OPENAI_API_KEY を設定
```

#### GPG暗号化ファイル
```elisp
(setq sumibi-api-key-source 'auth-source-gpg)
```

~/.authinfo.gpg に以下の形式で記述：
```
machine api.openai.com login apikey password sk-...
```

#### macOS Keychain
```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
```

macOSのKeychainでAPI Keyを登録：
- サーバー: api.openai.com
- アカウント: apikey
- パスワード: sk-...

### テスト結果
- ✅ 全23テストがパス
- ✅ 既存機能に影響なし
- ✅ Emacs 29.x以上で動作確認済み

### 技術的詳細
- 実装ファイル: lisp/sumibi.el
- 追加関数:
  - `sumibi-get-api-key`: 統合されたAPI Key取得関数
  - `sumibi-get-api-key-from-auth-source`: auth-sourceからの取得
  - `sumibi-setup-auth-source-for-gpg`: GPG用設定
  - `sumibi-setup-auth-source-for-keychain`: Keychain用設定
  - `sumibi-gpg-available-p`: gpgコマンド存在チェック
  - `sumibi-macos-keychain-available-p`: macOS環境チェック

この実装により、プレーンテキストでの環境変数保存から脱却し、より安全なAPI Key管理が可能になりました。

---

## GitHub Issue 119 実装完了報告

### 実装完了のお知らせ

Issue #119 で要望いただいた、助詞検出ベースの自動変換機能を実装しました。Ctrl+Jを押さなくても、助詞の後にスペースを入力するだけで自動的に日本語変換が実行されます。

### 実装した機能

#### 1. 助詞+スペースでの自動変換
助詞の後にスペースを入力すると、自動的にローマ字を日本語に変換します：

**動作例**:
- `watashiwa ` → `私は`
- `nihonga ` → `日本が`
- `tokyode ` → `東京で`
- `watashidesu ` → `私です`

#### 2. 句読点での自動変換
句読点（`.` または `,`）を入力すると、直前のローマ字を自動変換し、句読点を全角に変換します：

**動作例**:
- `nihongoni.` → `日本語に。`
- `tokyode,` → `東京で、`

#### 3. カスタマイズ可能な設定
新しいカスタマイズ変数を追加しました：

```elisp
;; 自動変換機能の有効/無効（デフォルト: nil）
(defcustom sumibi-ambient-enable nil
  :type 'boolean
  :group 'sumibi)

;; トリガーとなる助詞のリスト
(defvar sumibi-auto-convert-particles
  '("wa" "ha" "ga" "wo" "ni" "de" "to" "kara" "made" "he" "mo" "no" "ya" "desu"))

;; トリガーとなる句読点のリスト
(defvar sumibi-auto-convert-punctuation
  '(?. ?, ??))
```

### 使用方法

#### init.elでの設定例

```elisp
;; Sumibi
(when t
  (add-to-list 'load-path (expand-file-name "~/GitHub/Sumibi/lisp"))
  (load-file "~/GitHub/Sumibi/lisp/sumibi.el")
  (setq sumibi-debug nil)
  (setq sumibi-ambient-enable t)  ; 自動変換を有効化
  (global-sumibi-mode 1))
```

`global-sumibi-mode`を有効にするだけで、全てのバッファで自動変換が動作します。

### テスト結果
- ✅ 全24テストがパス
- ✅ 既存機能に影響なし
- ✅ Emacs 29.x以上で動作確認済み

### 技術的詳細
- 実装ファイル: lisp/sumibi.el
- 追加関数:
  - `sumibi-check-particle-trigger`: スペース/句読点入力時の助詞チェックと自動変換実行
  - `sumibi-setup-auto-convert-hook`: `post-self-insert-hook`の管理
  - `sumibi-auto-convert-hook-function`: `after-change-major-mode-hook`用のフック関数

### 実装のポイント

1. **`post-self-insert-hook`の活用**: 文字入力ごとにチェックを実行し、条件を満たせば自動変換
2. **`after-change-major-mode-hook`との統合**: `global-sumibi-mode`有効時、新しいバッファでも自動的にフックが設定される
3. **デバッグメッセージの制御**: `sumibi-debug`フラグで制御し、通常使用時は`*Messages*`バッファをクリーンに保つ

この実装により、Ctrl+Jを押す手間が省け、より自然な日本語入力が可能になりました。

---

## 英文の自動変換スキップ機能の追加

### 概要

自動変換機能の改良として、英文を誤って日本語に変換しないように、英文判定機能を追加しました。テキスト中の英単語の割合が80%以上の場合、自動変換をスキップします。

### 実装した機能

#### 1. 短い英単語のリスト
英単語辞書（sumibi-english-words.el）には3文字以上の単語しか含まれていないため、1-3文字の一般的な英単語のリストを追加しました。

```elisp
(defconst sumibi--short-english-words
  '("i" "a" "an" "as" "at" "be" "by" "do" "go" "he" "if" "in" "is" "it"
    "me" "my" "no" "of" "on" "or" "so" "to" "up" "us" "we"
    "add" "all" "am" "and" "any" "are" "bad" "big" "box" "but" "can" "did"
    "end" "far" "few" "fly" "for" "fox" "get" "got" "had" "has" "her" "him"
    "his" "hot" "how" "its" "let" "may" "new" "not" "now" "off" "old" "one"
    "our" "out" "own" "per" "put" "run" "saw" "say" "see" "she" "the" "too"
    "top" "two" "use" "was" "way" "who" "why" "win" "yes" "yet" "you")
  "英単語辞書に含まれない短い（1-3文字）の一般的な英単語のリスト。")
```

#### 2. 英文判定関数
テキスト全体を分析し、英単語の割合を計算する関数を実装しました。

```elisp
(defvar sumibi-auto-convert-english-threshold 0.8
  "自動変換をスキップする英単語の割合の閾値。
デフォルトは 0.8 (80%)。")

(defun sumibi-is-english-text-p (text)
  "TEXT が英文かどうかを判定する。
英単語の割合が sumibi-auto-convert-english-threshold 以上の場合、t を返す。"
  ...)
```

#### 3. 自動変換トリガーの改良
スペース入力時と句読点入力時、行頭から現在位置までのテキストで英文判定を実行し、英文の場合は自動変換をスキップします。

### 動作例

**英文の場合（自動変換スキップ）**:
- `I will not create a pull request.` → そのまま（ピリオドも半角）

**日本語の場合（自動変換実行）**:
- `watashiwa ` → `私は`
- `nihongoni.` → `日本語に。`

**混在テキストの判定**:
- `I will not create watashi` → 5単語中4単語が英語（80%）→ スキップ
- `I will watashi nihongo desu` → 5単語中3単語が英語（60%）→ 変換実行

### テスト結果
- ✅ 新規英文判定テスト: 全10テストがパス
  - `test-is-english-word-*`: 英単語認識テスト
  - `test-is-english-text-*`: 英文判定テスト
- ✅ 既存テスト: 全24テストがパス
- ✅ 既存機能に影響なし

### 技術的詳細
- 実装ファイル: lisp/sumibi.el
- テストファイル: test/sumibi-english-detection-test.el
- 追加関数:
  - `sumibi--short-english-words`: 短い英単語のリスト（定数）
  - `sumibi--is-short-english-word-p`: 短い英単語の判定関数
  - `sumibi-is-english-text-p`: テキスト全体の英文判定関数
  - `sumibi-auto-convert-english-threshold`: 英文判定の閾値（カスタマイズ変数）

この実装により、英文を入力する際に自動変換が誤って発動することがなくなり、より実用的な自動変換機能になりました。

---

## 自動変換トリガーの追加と改善

### 追加したトリガー

#### 1. 助詞 "ha" の追加
`sumibi-auto-convert-particles` に "ha" を追加しました。

**動作例**:
- `watashiha ` → `私は`
- `koreha ` → `これは`

#### 2. 句読点 "?" の追加
`sumibi-auto-convert-punctuation` に `??` を追加しました。

**動作例**:
- `darekadesu?` → `だれかです？`
- `dousuru?` → `どうする？`

### スペース削除の改善

「kyou ha 」のように助詞の前にスペースがある場合、以前は「今日 は」とスペースが残る問題がありました。

**修正内容**:
- 最後のスペース（助詞の後ろ）を削除
- 助詞の前のスペースも削除（ある場合）
- 変換を実行

**動作例**:
- `kyou ha ` → `今日は`（スペースなし）
- `nihon de ` → `日本で`（スペースなし）

### 技術的詳細
- 実装ファイル: lisp/sumibi.el
- 修正関数: `sumibi-check-particle-trigger` (lisp/sumibi.el:2393-2426)
- 変更箇所:
  - `sumibi-auto-convert-particles`: "ha" を追加（130行目）
  - `sumibi-auto-convert-punctuation`: `??` を追加（137行目）
  - スペース削除ロジック: 助詞の前のスペースも削除するように改善（2418-2424行目）

### テスト結果
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全10テストがパス
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし

---

## 英文判定機能の改善

自動変換機能で英文を正しく判定できるよう、以下の3つの改善を実装しました。

### 解決した問題

1. **固有名詞**: "Claude Code" のような大文字で始まる固有名詞が認識されず、自動変換が誤動作
2. **アポストロフィ**: "that's", "it's", "don't" などが英単語として認識されず、英文判定に失敗
3. **句読点**: "hello,", "world.", "test?" など句読点付き単語が認識されず、長文で判定精度が低下

### 実装した改善

1. **大文字判定**: 大文字で始まる単語（A-Z）を固有名詞として英単語に分類
2. **アポストロフィ除去**: `sumibi--remove-apostrophe` 関数を追加し、"that's" → "thats" として辞書照合
3. **句読点正規化**: `sumibi--normalize-word` 関数を追加し、句読点を除去してから判定

処理フロー: 単語 → 句読点除去（正規化） → アポストロフィ除去 → 辞書照合・大文字判定

### 動作例

- `"Claude Code"` → 固有名詞として認識、自動変換スキップ ✓
- `"that's what our new LLMFunction is about"` → 86%が英単語と認識、自動変換スキップ ✓
- `"So far, we mostly think of LLMs..."` → 90%が英単語と認識（修正前80%）、自動変換スキップ ✓

### 技術詳細

- 実装ファイル: lisp/sumibi.el
- 追加関数:
  - `sumibi--remove-apostrophe` (234行目)
  - `sumibi--normalize-word` (239-253行目)
- 修正関数: `sumibi-is-english-text-p` (254-275行目)

### テスト結果
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全14テストがパス
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし

---

## サンプル英文（ブログ用）

### 固有名詞を含む例
```
Claude Code is an AI-powered coding assistant developed by Anthropic.
GitHub Copilot and OpenAI Codex are also popular AI coding tools.
The LLMFunction API allows developers to integrate AI capabilities easily.
```

### アポストロフィを含む例
```
It's amazing how AI can understand natural language.
That's what makes modern language models so powerful.
I don't think we've seen anything like this before.
You're going to love this new feature.
```

### 句読点を含む長文例
```
So far, we mostly think of LLMs as things we interact directly with, say through chat interfaces. But what if we could take LLM functionality and "package it up" so that we can routinely use it as a component inside anything we're doing? Well, that's what our new LLMFunction is about.
```

### 混合パターン例
```
Claude's new feature, called "auto-convert," helps Japanese users type more naturally. It's designed to skip English text automatically, so phrases like "Hello, World!" won't be converted to hiragana.
```

これらの英文を入力して、自動変換が誤って発動しないことを確認できます。

---

## 英語短縮形（Contractions）の対応

### 問題

"I don't think we've seen anything like this before." のような短縮形を含む英文で、自動変換が誤って発動する問題がありました。
- "don't" → "dont" に正規化されるが辞書にない
- "we've" → "weve" に正規化されるが辞書にない
- 結果: 9単語中7単語しか認識されず、比率が78%で閾値80%を下回る

### 解決策

一般的な英語の短縮形リスト `sumibi--english-contractions` を追加しました。

### 追加した短縮形（抜粋）
- 人称代名詞: I'm, I've, I'll, you're, you've, we're, we've, they're, they've
- be動詞否定: isn't, aren't, wasn't, weren't
- have動詞否定: hasn't, haven't, hadn't
- do動詞否定: don't, doesn't, didn't
- 助動詞否定: can't, couldn't, won't, wouldn't, shouldn't
- その他: it's, that's, what's, let's, here's, there's

### 技術詳細
- 追加定数: `sumibi--english-contractions` (228-244行目)
- 追加関数: `sumibi--is-english-contraction-p` (246行目)
- 修正関数: `sumibi-is-english-text-p` (309行目に短縮形チェックを追加)

### テスト結果
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全16テストがパス（短縮形テスト含む）
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし

---

## ローマ字比率による自動変換閾値の追加

### 概要

自動変換機能に新しい閾値を追加しました。テキスト中のローマ字として変換可能な文字の割合が50%を超えた場合のみ、自動変換を発動します。

### 実装した機能

#### 1. 新しいカスタマイズ変数
```elisp
(defvar sumibi-auto-convert-romaji-threshold 0.5
  "自動変換を発動するローマ字変換可能文字の割合の閾値。
デフォルトは 0.5 (50%)。")
```

#### 2. ローマ字変換文字数計算関数
`sumibi-romaji-converted-chars`: `sumibi-romaji-to-hiragana` で変換し、変換後のひらがな文字数 × 2 をローマ字として変換された文字数として返す。

```elisp
;; 例:
;; "watashi" (7文字) → "わたし" (3文字) → 3×2=6文字
;; "nihon" (5文字) → "にほん" (3文字) → min(3×2, 5)=5文字
;; "hello" (英単語) → 変換されない → 0文字
```

#### 3. ローマ字比率計算関数
`sumibi-romaji-ratio`: テキスト全体のローマ字変換可能な文字の割合を計算。

#### 4. 閾値判定関数
`sumibi-has-sufficient-romaji-p`: 割合が閾値以上かを判定。

### 動作例

| テキスト | 計算 | 比率 | 自動変換 |
|---------|------|------|---------|
| `watashi nihon` | (6+5)/(7+5) | 92% | ✅ 発動 |
| `watashi hello` | (6+0)/(7+5) | 50% | ✅ 発動 |
| `nihon hello world` | (5+0+0)/(5+5+5) | 33% | ❌ スキップ |
| `I will not create watashi` | (0+0+0+0+6)/21 | 29% | ❌ スキップ |

### 技術詳細
- 実装ファイル: lisp/sumibi.el
- テストファイル: test/sumibi-romaji-ratio-test.el
- 追加関数:
  - `sumibi-romaji-to-hiragana-partial`: 部分変換（変換できない文字はそのまま残す）
  - `sumibi--count-hiragana`: ひらがな/カタカナ文字数カウント
  - `sumibi-romaji-converted-chars`: 変換された文字数を計算
  - `sumibi-romaji-ratio`: ローマ字比率を計算
  - `sumibi-has-sufficient-romaji-p`: 閾値判定
- 追加カスタマイズ変数:
  - `sumibi-auto-convert-romaji-threshold` (218行目)

### 部分変換の例

```
"anbientokonpyu-thingu" → "あんびえんとこんぴゅーtひんぐ"
"thingu" → "tひんぐ"
"hello" → 英単語なので変換しない (0文字)
```

### テスト結果
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全16テストがパス
- ✅ ローマ字比率テスト: 全11テストがパス
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし

---

## 判定用テキスト取得範囲の修正

### 問題

「Emacsでro-majiwouchituduketeiruto,」のような入力で、自動変換が発動しない問題がありました。

**原因**: `line-text` を行頭から取得していたため、「Emacsでro-majiwouchituduketeiruto」全体が判定対象となり、「E」で始まるため固有名詞として英文と判定されていました。

### 修正

`sumibi-skip-chars` を使って後方スキップし、マッチしない文字（ひらがな「で」等）で区切るようにしました。

```elisp
;; 修正前: 行頭から取得
(line-text (buffer-substring-no-properties
            (line-beginning-position)
            (save-excursion (backward-char 1) (point))))

;; 修正後: sumibi-skip-charsで後方スキップした範囲を取得
(line-text (save-excursion
             (backward-char 1)
             (let ((current (point)))
               (skip-chars-backward sumibi-skip-chars (line-beginning-position))
               (buffer-substring-no-properties (point) current))))
```

### 動作例

「Emacsでro-majiwouchituduketeiruto,」の場合:
- 修正前: 「Emacsでro-majiwouchituduketeiruto」が判定対象 → 英文と判定 → スキップ
- 修正後: 「ro-majiwouchituduketeiruto」のみが判定対象 → ローマ字100% → 発動

### テスト結果
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全16テストがパス
- ✅ ローマ字比率テスト: 全11テストがパス
- ✅ 括弧バランスチェック: OK

---

## 自動変換の除外機能の実装

### 概要

自動変換機能に、特定のバッファやファイルで自動変換を無効化する除外機能を追加しました。セキュリティ上のリスクや、誤変換による操作ミスを防ぐために、以下のケースで自動変換をスキップします。

### 実装した機能

#### 1. シェルバッファでの自動変換の無効化

シェルバッファ（`shell-mode`）では、コマンドラインでの操作が主体となります。ファイル名やコマンド名、オプション引数などはローマ字の連続で構成されることが多く、これらを日本語に変換してしまうと操作に支障をきたします。

**具体例**:
- `ls -la Documents/project-alpha/` → ディレクトリ名が変換されると意図しない結果に
- `git checkout feature/user-authentication` → ブランチ名の誤変換を防ぐ
- 環境変数名やファイルパスも同様

前述の英単語辞書による判定があるものの、ローマ字のディレクトリ名やプロジェクト名は英単語として認識されない可能性があるため、シェルバッファでは自動変換を完全に無効化しました。

#### 2. GPG暗号化ファイルでの自動変換の無効化

GPG（GNU Privacy Guard）で暗号化されたファイル（`.gpg`拡張子）には、パスワードや秘密鍵、個人情報などの機密データが含まれている可能性が高いです。

Sumibiは変換処理にLLM（GPT-5.1やGemini）を使用するため、入力された文字列はOpenAIやGoogleのAPIサーバーに送信されます。暗号化ファイルの内容を復号化した後に編集する際、その内容が外部APIに送信されることは、セキュリティ上重大なリスクとなります。

**リスクの具体例**:
- パスワードマネージャーのデータベース
- SSH秘密鍵
- API認証情報
- 個人情報を含むファイル

これらの機密情報が誤ってクラウドに送信されることを絶対に避けるため、`.gpg`拡張子のファイルでは自動変換を無効にしました。

#### 3. カスタマイズ可能な設定

新しいカスタマイズ変数を追加しました：

```elisp
;; 除外するメジャーモード
(defcustom sumibi-ambient-exclude-modes
  '(shell-mode)
  "自動変換を無効にするメジャーモードのリスト。"
  :type '(repeat symbol)
  :group 'sumibi)

;; 除外するファイル拡張子
(defcustom sumibi-ambient-exclude-file-extensions
  '("gpg")
  "自動変換を無効にするファイル拡張子のリスト。"
  :type '(repeat string)
  :group 'sumibi)
```

### 使用方法

#### デフォルト設定

特別な設定なしで、以下のバッファで自動変換が無効になります：
- `shell-mode`（シェルバッファ）
- `.gpg`拡張子のファイル

#### カスタマイズ例

他のモードや拡張子を追加したい場合：

```elisp
;; init.el での設定例

;; emacs-lisp-mode でも自動変換を無効化
(setq sumibi-ambient-exclude-modes
      '(shell-mode emacs-lisp-mode))

;; .secret 拡張子のファイルでも自動変換を無効化
(setq sumibi-ambient-exclude-file-extensions
      '("gpg" "secret"))
```

### テスト結果
- ✅ 自動変換除外テスト: 全7テストがパス
  - `test-should-exclude-shell-mode`: シェルモードでの除外
  - `test-should-exclude-gpg-file`: GPGファイルでの除外
  - `test-should-not-exclude-normal-buffer`: 通常バッファでは除外されない
  - `test-should-not-exclude-normal-file`: 通常ファイルでは除外されない
  - `test-should-exclude-custom-mode`: カスタムモードの除外
  - `test-should-exclude-custom-extension`: カスタム拡張子の除外
  - `test-should-not-exclude-buffer-without-file`: ファイル名なしバッファ
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全16テストがパス
- ✅ ローマ字比率テスト: 全11テストがパス
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし

### 技術的詳細
- 実装ファイル: lisp/sumibi.el
- テストファイル: test/sumibi-auto-convert-exclude-test.el
- 追加関数:
  - `sumibi-should-exclude-auto-convert-p`: 除外対象かどうかを判定する関数（2597-2611行目）
    - メジャーモードが `sumibi-ambient-exclude-modes` に含まれているかチェック
    - ファイル拡張子が `sumibi-ambient-exclude-file-extensions` に含まれているかチェック
- 追加カスタマイズ変数:
  - `sumibi-ambient-exclude-modes` (144-149行目)
  - `sumibi-ambient-exclude-file-extensions` (151-156行目)
- 修正関数: `sumibi-check-particle-trigger` (2613-2699行目)
  - 除外チェックを追加し、除外対象の場合は自動変換をスキップ

この実装により、セキュリティリスクの高いファイルや、誤変換によるトラブルが発生しやすいバッファで、自動変換が適切に無効化されるようになりました。

---

## ミニバッファでの自動変換無効化の追加

### 概要

自動変換除外機能に、ミニバッファ（Find File:などの入力エリア）での自動変換を無効化する機能を追加しました。

### 問題

ファイル名入力などのミニバッファで自動変換が発動すると、非常に不便です。

**具体例**:
- `M-x find-file` で `a.txt` と入力したとき、ドット `.` のタイミングで自動変換が発動
- `M-x grep` でコマンドを入力中に、句読点で自動変換が発動
- バッファ名やディレクトリ名の入力中に誤変換が発生

### 実装した機能

`sumibi-should-exclude-auto-convert-p` 関数に、ミニバッファのチェックを追加しました。Emacsの組み込み関数 `minibufferp` を使用して、現在のバッファがミニバッファかどうかを判定します。

```elisp
(defun sumibi-should-exclude-auto-convert-p ()
  "現在のバッファが自動変換の除外対象かどうかを判定する。
以下の条件のいずれかを満たす場合、t を返す:
1. 現在のメジャーモードが `sumibi-ambient-exclude-modes` に含まれている
2. 現在のバッファのファイル拡張子が `sumibi-ambient-exclude-file-extensions` に含まれている
3. 現在のバッファがミニバッファである（Find File: などの入力エリア）"
  (or
   ;; ミニバッファチェック
   (minibufferp)
   ;; メジャーモードチェック
   (memq major-mode sumibi-ambient-exclude-modes)
   ;; ファイル拡張子チェック
   (when-let* ((filename (buffer-file-name))
               (extension (file-name-extension filename)))
     (member extension sumibi-ambient-exclude-file-extensions))))
```

### 動作確認方法

以下の操作で、ミニバッファでの自動変換が無効になることを確認できます：

1. `M-x find-file` を実行
2. ミニバッファで `a.txt` と入力
3. ドット `.` を入力しても自動変換が発動しない ✓
4. `M-x grep` を実行
5. コマンド入力中に句読点を入力しても自動変換が発動しない ✓

### テスト結果
- ✅ 自動変換除外テスト: 全8テストがパス（ミニバッファテスト追加）
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全16テストがパス
- ✅ ローマ字比率テスト: 全11テストがパス
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし

### 技術的詳細
- 実装ファイル: lisp/sumibi.el
- テストファイル: test/sumibi-auto-convert-exclude-test.el
- 修正関数: `sumibi-should-exclude-auto-convert-p` (2601-2615行目)
  - `minibufferp` によるミニバッファチェックを最初に追加
  - ミニバッファの場合は即座に `t` を返して自動変換をスキップ

この実装により、ファイル名入力やコマンド入力など、ミニバッファでの操作時に自動変換が誤って発動することがなくなり、より快適な操作が可能になりました。


## README.zh.md の日本語訳

### Sumibi Chinese

LLM APIを使用したEmacs用中国語ピンイン入力メソッド

### Sumibi Chineseとは

Emacs用の中国語入力システムです。[Sumibi](README.md)（日本語入力メソッド）の中国語版モジュールです。

Sumibi Chineseはモードレスです。中国語入力モードに切り替えることなく中国語を入力できます。

ピンインを入力してCtrl-Jを押すだけで、中国語（簡体字）に変換されます。

### 利用可能なEmacsバージョン

Emacs version 29.x 以上（Windows/Linux/macOS）。Sumibi本体パッケージが必要です。

### インストール

1. OpenAI AIのサブスクリプションを契約します。
2. 環境変数 `OPENAI_API_KEY` にOpenAI APIキーを登録します。（`SUMIBI_AI_API_KEY` も使用可能です）
3. MELPAからパッケージ「sumibi」をインストールします。
4. ~/.emacs.d/init.el に以下のコードを追加します。

```lisp
(require 'sumibi-chinese)
(global-sumibi-chinese-mode 1)
```

Gemini APIを使う場合は、環境変数 `GEMINI_API_KEY` を設定し、init.el に以下を追加します。

```lisp
(require 'sumibi-chinese)
(setq sumibi-provider 'gemini)
(global-sumibi-chinese-mode 1)
```

### インストールが成功したかどうかの確認方法

Emacsを再起動するとステータスバーに `[中]` が表示されます。

### ピンインから中国語への変換

1. ピンインで書いた文章の最後にカーソルを合わせて、Ctrl-Jを入力すると中国語に変換されます。

   例：
   ```
   wo shi zhongguo ren  →  我是中国人
   ni hao ma            →  你好吗
   jin tian tian qi hen hao  →  今天天气很好
   xue xi zhong wen     →  学习中文
   ```

2. 変換結果が気に入らない場合は、そのままCtrl-Jを入力すると変換候補のポップアップが表示されるので、その中から選択できます。

3. ピンインのテキストをregion選択してからCtrl-Jで変換することもできます。

### Undo

変換結果が気に入らない時は、ESC-u キーを入力することでUndoできます。

または、変換候補の中に「原文」の選択肢がありますので、それを選択して元のピンインに戻すこともできます。

### 利用するAIサービスの切り替え

AIサービスの切り替え方法はSumibi日本語版と同じです。詳しくは [README](README.md) の「利用するAIサービスの切り替え」を参照してください。

- Geminiに切り替える場合（推奨）: `(setq sumibi-provider 'gemini)`
- DeepSeekに切り替える場合: 環境変数で `SUMIBI_AI_BASEURL` と `SUMIBI_AI_MODEL` を設定
- ローカルLLMに切り替える場合: 同様に環境変数で設定

### 日本語版Sumibiとの併用

Sumibi Chineseと日本語版Sumibiは同時に使用できますが、両方ともC-jキーにバインドされているため、必要に応じてキーバインドを変更してください。

```lisp
;; 日本語版はC-j、中国語版はC-;を使用
(require 'sumibi)
(global-sumibi-mode 1)

(require 'sumibi-chinese)
(define-key sumibi-chinese-mode-map (kbd "C-;") 'sumibi-chinese-trans)
(global-sumibi-chinese-mode 1)
```

### LLMが使えない環境での利用

本入力メソッドはLLM API接続が必要です。LLMが使えない場合は、オフラインの中国語入力メソッド [pyim](https://github.com/tumashu/pyim) をご検討ください。


## ピンイン入力テスト用サンプル文章

### 基本的な挨拶

| ピンイン入力 | 期待される変換結果 | 日本語訳 |
|---|---|---|
| ni hao | 你好 | こんにちは |
| xie xie | 谢谢 | ありがとう |
| zai jian | 再见 | さようなら |

### 短い文

| ピンイン入力 | 期待される変換結果 | 日本語訳 |
|---|---|---|
| wo shi zhongguo ren | 我是中国人 | 私は中国人です |
| ni hao ma | 你好吗 | お元気ですか |
| jin tian tian qi hen hao | 今天天气很好 | 今日はいい天気です |
| wo xiang xue xi zhong wen | 我想学习中文 | 中国語を勉強したいです |

### 日常会話

| ピンイン入力 | 期待される変換結果 | 日本語訳 |
|---|---|---|
| qing wen xi shou jian zai na li | 请问洗手间在哪里 | すみません、トイレはどこですか |
| wo xiang he yi bei ka fei | 我想喝一杯咖啡 | コーヒーを一杯飲みたいです |
| jin tian shi xing qi ji | 今天是星期几 | 今日は何曜日ですか |
| ming tian jian | 明天见 | また明日 |

### 少し長い文

| ピンイン入力 | 期待される変換結果 | 日本語訳 |
|---|---|---|
| zhe shi yi ge hen hao de ji hui | 这是一个很好的机会 | これはとても良い機会です |
| wo men yi qi qu chi fan ba | 我们一起去吃饭吧 | 一緒にご飯を食べに行きましょう |
| ta shi wo de hao peng you | 他是我的好朋友 | 彼は私の親友です |
| zhong guo de li shi hen you yi si | 中国的历史很有意思 | 中国の歴史はとても面白い |

### 数字・日付を含む文

| ピンイン入力 | 期待される変換結果 | 日本語訳 |
|---|---|---|
| wo jin nian san shi sui | 我今年三十岁 | 私は今年30歳です |
| xian zai shi xia wu san dian | 现在是下午三点 | 今は午後3時です |

### ビジネス的な文

| ピンイン入力 | 期待される変換結果 | 日本語訳 |
|---|---|---|
| hen gao xing ren shi ni | 很高兴认识你 | お会いできて嬉しいです |
| fei chang gan xie nin de bang zhu | 非常感谢您的帮助 | ご支援に大変感謝いたします |


## pyim の UI 設計調査（1文字訂正と工夫）

参考: https://github.com/tumashu/pyim

### 1文字訂正の核心機能：「以词定字」（いしていじ）

pyim の最も巧みな機能。中国語では同じピンインを持つ漢字が非常に多いため、1文字だけ選ぶのが大変。これを解決する仕組みがある。

**仕組み**: 複数文字の候補（例：`你好`）が表示されているとき、**F1〜F4** を押すとN番目の文字だけを取り出せる。

- 候補: `你好`
- **F1** → `你` だけ出力
- **F2** → `好` だけ出力

**なぜ便利か**: 「ni」のピンインには「你」「尼」「泥」「逆」...と何十もの候補がある。しかし「nihao」と入力すれば「你好」が上位に来るので、そこからF1で「你」を確実に取り出せる。**単語の文脈を使って1文字を特定する**という発想。

**実装詳細**:
- `pyim-select-char-in-word-by-number` がF1-F4にバインドされている
- `pyim-process-plan-to-select-char-in-word(char-position)` を呼び出し、`pyim-process--char-position-in-word` にインデックスを保存
- 確定時にそのインデックスの文字だけが挿入される

### pyim のその他の工夫

| 機能 | 説明 |
|---|---|
| **金手指（Magic Finger）** | バッファにピンインを直接打ってから後で変換（`pyim-convert-string-at-point`）。Sumibiの Ctrl-J と同じ発想 |
| **音節単位の削除** | `M-DEL` でピンイン1音節分を削除（`zhong` 全体を一度に消せる） |
| **コンテキスト自動切替（Probe）** | コード中（コメント外）では自動的に英語モードに切替。`pyim-probe-*` 系関数で実現 |
| **4種類の表示バックエンド** | posframe / popup / popon / minibuffer から環境に合わせて選択 |
| **非同期候補取得** | 候補を非同期で取得しUIの応答性を維持 |
| **Magic Converter** | 出力前に変換（簡体字→繁体字など）をかけるフック（`pyim-outcome-magic-converter`） |
| **カーソル色/モードライン** | 入力状態（中国語/英語）をカーソル色やモードライン表示で視覚的にフィードバック |

### 候補選択 UI

**表示形式**: 2種類
- **2行表示（デフォルト）**: 1行目にプレビューとページ番号、2行目に候補メニュー `1.你好 2.你号 3.尼好 ...`
- **1行表示**: すべて1行に `[preview]: 1.你好 2.你号 ... (page#)`

**主なキーバインド**:
- `1`〜`9`: 番号で候補を直接選択
- `C-n` / `C-p`: 次/前の候補へ移動
- `M-n` / `M-p` または `=` / `-`: 次/前のページ
- `C-f` / `C-b`: ピンイン内でカーソル移動
- `M-f` / `M-b`: 音節単位でカーソル移動
- `DEL`: 1文字削除
- `C-DEL` / `M-DEL`: 1音節削除

### 変換済みテキストの部分訂正

pyim も変換確定後の部分訂正は直接サポートしていない。訂正方法は：
1. **Undo** して再入力
2. 間違った文字を**削除**してピンインを打ち直す
3. **金手指**: 正しいピンインを打って `pyim-convert-string-at-point` で変換
4. `pyim-create-Nchar-word-at-point` で周囲のテキストからN文字の単語を抽出し、個人辞書に登録して将来の予測精度を向上

### Sumibi Chinese への示唆

1. **「以词定字」の実装**: Ctrl-J のポップアップ候補表示時に、候補の中の特定の1文字だけを選べるUIがあると便利。例えば候補 `我是中国人` から「国」だけ取り出したい場合など
2. **音節単位の編集**: ピンインは音節境界が明確なので、音節単位でのカーソル移動・削除があると使いやすい
3. **LLMベースの部分訂正**: pyim も直接的なサポートはないが、LLMベースの Sumibi なら「この1文字だけ別の候補に」というUIを独自に実装できる可能性がある
4. **コンテキスト自動切替**: Sumibi の英文判定（`sumibi-is-english-text-p`）に加えて、モードベースの判定（コード編集中は自動変換しない等）も検討に値する


質問です。
さきほどの、deepseekのパスコードを保存しますか？という質問はChinese版の時も日本語で質問しますか。？

中国語圏の人が日本語を読めることはまずありません。
日本語と英語の日本語と英語の両方で表示する可能性がある可能性は、英語にしてください。


コミットしてください。


