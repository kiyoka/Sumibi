ge
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

github issue 87を読んで理解してください。


sumibi.elに対して、以下の修正を加えようと思います。

sumibi-backendに3つ目の選択肢を用意します。
それは、"Mozc + Local LLM" です。
これを洗濯した場合は、まずは、mozcで変換候補を洗い出し、その変換候補から相応しい変換候補に並べ替えるのを
local LLM を使って実行します。
それは、"Mozc + Local LLM"を選択した時にだけ使うsumibi用カスタマイズ変数を設けてください。
local LLMのエンドポイントURLとモデル名を指定するためのカスタマイズ変数です。
それでは、この対応について検討を開始してください。


  実装完了

  GitHub issue 87の要求に対応して、以下の機能を実装しました：

  実装した機能

  1. 新しいバックエンド選択肢: sumibi-backend に mozc-llm を追加
  2. カスタマイズ変数:
    - sumibi-local-llm-base-url: Local LLM のエンドポイント URL
    - sumibi-local-llm-model: Local LLM のモデル名（デフォルト: "gpt-oss-20b"）
  3. 基盤となる関数: sumibi-local-llm-rank-candidates 関数（現在はスケルトン実装）
  4. 統合: mozc-llm バックエンド選択時に Local LLM でランク付けを行うロジック

  変更されたファイル

  - lisp/sumibi.el: メイン実装ファイル
    - 行267-273: バックエンドの説明を更新
    - 行277-279: :type の選択肢を追加
    - 行282-285: sumibi-backend-mozc-p 関数を更新
    - 行311-322: Local LLM 用カスタマイズ変数を追加
    - 行949-957: sumibi-local-llm-rank-candidates 関数を追加
    - 行1060-1061: mozc-llm 使用時のランク付け呼び出しを追加

  今後の拡張予定

  現在の sumibi-local-llm-rank-candidates 関数は基本的なスケルトンのみですが、将来的に以下の機能を実装できます：

  1. Local LLM との HTTP 通信
  2. 文脈を考慮した候補のランク付け
  3. エラーハンドリング
  4. パフォーマンス最適化

  この基盤により、ユーザーは mozc-llm バックエンドを選択し、適切なLocal LLMの設定を行うことで、文脈に応じた高精度な日本語変換を利用できるようになります。


sumibi-openai-http-postという関数とsumibi-local-llm-rank-candidatesという関数の共通項を括り出して、
HTTP POSTする関数をまとめてください
