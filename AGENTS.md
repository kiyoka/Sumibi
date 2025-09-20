
# Repository Guidelines

## プロジェクト構成

- `lisp/` Emacs Lisp 本体（主: `lisp/sumibi.el`、辞書: `lisp/sumibi-localdic.el`）
- `test/` ERT テスト一式（`test/sumibi-mozc-tests.el`、`test/mozc-mock.el`）
- `benchmark/`, `benchmark_mozc_conversion_helper/` ベンチマーク関連、`skkdic/` 辞書、`images/` 画像資産
- ルート: `Makefile`（パッケージ/テスト入口）、`README.md`, `TESTING.md`, `MOZC.md` などの文書

## ビルド・テスト・開発

- `make release` `lisp/*.el` と文書・LICENSE を同梱した `Sumibi-elisp-<version>.tar.gz` を作成
- `make clean` 生成物を削除
- `make test` モック環境で ERT を実行（高速・再現性重視）
- `SUMIBI_TEST_USE_MOCK=0 make test` 実 Mozc と接続してテスト（Mozc 必須）

## コーディング規約（Emacs Lisp）

- インデント: 2 スペース。読みやすさを最優先
- 命名: 公開シンボルは `sumibi-` 接頭辞（例: `sumibi-roman-to-kanji-with-surrounding`）
- ファイル先頭: 必要に応じ `-*- lexical-binding: t; -*-` を付与
- 依存追加は最小限に。小さなユーティリティで置き換えを検討

## テスト指針

- フレームワーク: ERT。`test/` 配下に追加し、既存テストに倣う
- 命名: `sumibi-mozc-...` を基本。確定的な検証はモックを使用
- 実行: `make test`（モック）/ `SUMIBI_TEST_USE_MOCK=0 make test`（実 Mozc）

## コミット / PR ガイド

- メッセージは簡潔に。必要なら日本語/英語いずれでも。関連 Issue は `#<番号>` を付記
- PR には概要・目的・変更点、関連 Issue、テスト/スクリーンショット（あれば）を含める
- 影響や設定変更（例: `SUMIBI_AI_API_KEY` 等の環境変数）は明記。PR は小さく焦点化

## エージェント向け手順

- `lisp/sumibi.el` を編集後は必ず括弧バランスを確認:
  `agent-lisp-paren-aid-linux lisp/sumibi.el`
- 不整合が出た場合は、指摘行のみ括弧を補正し、再度上記コマンドを実行。他の編集は行わない
- Lisp 括弧は手で数えず、必ずツールで検証

## セキュリティ / 設定

- 機密情報はコミットしない。API キー等は環境変数 `SUMIBI_AI_API_KEY`、任意で `SUMIBI_AI_BASEURL`, `SUMIBI_AI_MODEL` を使用
- テストは既定でモックを使用し、外部ネットワーク不要で通る構成を維持
