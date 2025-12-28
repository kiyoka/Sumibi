
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
