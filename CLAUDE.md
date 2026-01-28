
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
(defcustom sumibi-auto-convert-enable nil
  :type 'boolean
  :group 'sumibi)

;; トリガーとなる助詞のリスト
(defcustom sumibi-auto-convert-particles
  '("wa" "ga" "wo" "ni" "de" "to" "kara" "made" "he" "mo" "no" "ya" "desu")
  :type '(repeat string)
  :group 'sumibi)

;; トリガーとなる句読点のリスト
(defcustom sumibi-auto-convert-punctuation
  '(?. ?,)
  :type '(repeat character)
  :group 'sumibi)
```

### 使用方法

#### init.elでの設定例

```elisp
;; Sumibi
(when t
  (add-to-list 'load-path (expand-file-name "~/GitHub/Sumibi/lisp"))
  (load-file "~/GitHub/Sumibi/lisp/sumibi.el")
  (setq sumibi-debug nil)
  (setq sumibi-auto-convert-enable t)  ; 自動変換を有効化
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
(defcustom sumibi-auto-convert-english-threshold 0.8
  "自動変換をスキップする英単語の割合の閾値。
デフォルトは 0.8 (80%)。"
  :type 'float
  :group 'sumibi)

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
(defcustom sumibi-auto-convert-romaji-threshold 0.5
  "自動変換を発動するローマ字変換可能文字の割合の閾値。
デフォルトは 0.5 (50%)。"
  :type 'float
  :group 'sumibi)
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
  - `sumibi-romaji-converted-chars` (449行目)
  - `sumibi-romaji-ratio` (463行目)
  - `sumibi-has-sufficient-romaji-p` (488行目)
- 追加カスタマイズ変数:
  - `sumibi-auto-convert-romaji-threshold` (218行目)

### テスト結果
- ✅ ローマ字変換テスト: 全24テストがパス
- ✅ 英文検出テスト: 全16テストがパス
- ✅ ローマ字比率テスト: 全11テストがパス
- ✅ 括弧バランスチェック: OK
- ✅ 既存機能に影響なし
