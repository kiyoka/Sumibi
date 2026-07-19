# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

Sumibiは、AI (LLM API) を使用したEmacs用の日本語入力メソッドです。モードレス入力が特徴で、入力モードの切り替えなしに日本語を入力できます。ローマ字を書いてCtrl-Jで変換します。中国語（ピンイン）入力版の sumibi-chinese も同梱しています。

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
# macOSの場合
agent-lisp-paren-aid lisp/sumibi.el

# Linuxの場合
agent-lisp-paren-aid-linux lisp/sumibi.el
```

もし括弧の不整合が検出されたら：
1. 他の編集作業はせず、指摘された行番号の括弧を修正
2. 再度ツールを実行して確認
3. すべての括弧が整合してから次の作業へ

**重要**: LLMはLisp括弧を数えるのが苦手なため、自分で数えたり考えたりせず、必ずこのツールを使うようにしてください。

**注意**: `agent-lisp-paren-aid` はフォーマット文字列内の `(%s)` などを括弧として誤検出することがあります。ツールがエラーを報告しても、Emacsの `M-x check-parens` で問題がなければ、誤検出と判断してください。

## アーキテクチャ

### ディレクトリ構造
- `lisp/` - メインのEmacs Lispソースコード
  - `sumibi.el` - メイン実装
  - `sumibi-localdic.el` - ローカル辞書サポート
  - `sumibi-mozc.el` - mozc連携（ローカル変換エンジン）
  - `sumibi-chinese.el` - 中国語（ピンイン）入力版
  - `sumibi-english-words.el` - 英単語辞書（英文判定用）
- `test/` - ERTテストファイル
- `benchmark/` - パフォーマンスベンチマークツール
- `skkdic/` - SKK辞書関連ファイル

### 主要コンポーネント
1. **AI変換エンジン**: OpenAI/Gemini/DeepSeek APIを使用してローマ字を日本語に変換
2. **ポップアップUI**: 候補選択インターフェース
3. **履歴管理**: より良いコンテキスト理解のための変換履歴
4. **自動変換（ambient）**: 助詞や句読点をトリガーにCtrl-Jなしで変換

### 依存関係
- Emacs >= 29.0
- popup >= 0.5.9
- unicode-escape >= 1.1
- deferred >= 0.5.1
- markdown-mode >= 2.0

## 外部ライブラリ

もし、ちょっとした手元のテストコードで以下のライブラリを読み込みたくなった場合は、 ~/.emacs.d/elpa/ から検索してパスを追加してください

(require 'cl-lib)
(require 'popup)
(require 'url)
(require 'url-http)
(require 'unicode-escape)
(require 'deferred)
(require 'sumibi-localdic)

## 実装済み機能のメモ

過去のIssueで実装済み。詳細な経緯は各Issueとgit履歴を参照。

### API Keyのセキュア管理 (Issue #95)
- `sumibi-api-key-source` で取得元を選択: `'environment`（デフォルト、環境変数 SUMIBI_AI_API_KEY / OPENAI_API_KEY）、`'auth-source-gpg`（~/.authinfo.gpg）、`'auth-source-keychain`（macOS Keychain）
- 取得は `sumibi-get-api-key` に統合。auth-source利用時は host=api.openai.com, login=apikey で検索
- GPGモードはgpgコマンドの存在、KeychainモードはmacOSであることを事前チェックし、日本語エラーメッセージを表示

### 自動変換機能 (Issue #119, ambient)
- `sumibi-ambient-enable`（デフォルト nil）で有効化。助詞（wa/ha/ga/ni/de/desu など `sumibi-auto-convert-particles`）+スペース、または句読点（`.` `,` `?` = `sumibi-auto-convert-punctuation`）で自動変換
- 変換対象テキストは行頭からではなく `sumibi-skip-chars` の後方スキップで取得（日本語文字で区切る）
- 誤発動を防ぐ2段階の判定:
  - **英文判定** `sumibi-is-english-text-p`: 英単語比率が `sumibi-auto-convert-english-threshold`（0.8）以上ならスキップ。短い英単語リスト・短縮形（don't等）・固有名詞（大文字始まり）・句読点付き単語に対応
  - **ローマ字比率判定** `sumibi-has-sufficient-romaji-p`: ひらがなに変換可能な文字の比率が `sumibi-auto-convert-romaji-threshold`（0.5）未満ならスキップ
- 除外判定 `sumibi-should-exclude-auto-convert-p`: ミニバッファ、`sumibi-ambient-exclude-modes`（デフォルト shell-mode）、`sumibi-ambient-exclude-file-extensions`（デフォルト "gpg"、機密情報を外部APIに送らないため）で自動変換を無効化

## 参考: pyim の UI 設計調査

sumibi-chinese のUI改善の参考として pyim (https://github.com/tumashu/pyim) を調査済み。要点:
- **以词定字**: 候補語の中の1文字だけをF1〜F4で取り出せる（単語の文脈で1文字を特定する発想）
- 音節単位のカーソル移動・削除、コンテキストによる自動モード切替（コード中は英語）、posframe/popup等の複数表示バックエンド
- Sumibiへの示唆: 候補ポップアップでの1文字選択UI、LLMベースの部分訂正の可能性
