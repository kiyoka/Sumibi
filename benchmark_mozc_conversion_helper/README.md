# Mozc Conversion Helper Benchmark

## 概要

このディレクトリは、issue #87 で提案された「LLMを使った Mozc 変換候補の知的選択」のベンチマーク環境です。
現代日本語への適合性を高めるため、コーパスを青空文庫から Wikipedia 日本語ダンプへ切り替えました。

## 目的

バックエンドが mozc モードの際に、LLM が文脈に最適な変換候補を自動選択できるかを検証します。

## ベンチマーク作成手順（Livedoor ニュース）

1. 事前準備: Livedoor ニュースコーパス（LDCC）を入手・展開
   - 例: `./external/LDCC/text/...` にカテゴリ別の `.txt` が並ぶ状態
2. プレーンテキスト化: 記事を1行テキストに整形
   - `python3 scripts/prepare_livedoor.py ./external/LDCC/text`
   - 出力: `data/corpus/livedoor.txt`
3. パターン抽出: `python3 scripts/extract_patterns_from_corpus.py`
   - `data/corpus/*.txt` を自動検出し、`extracted_pattern_code.txt` を生成
4. 正解データ作成: `python3 scripts/create_ground_truth_data.py`
   - `ground_truth_data.json` を生成
5. テストケース検証: `python3 scripts/test_embedded_cases.py`
6. ベンチマーク実行: `export OPENAI_API_KEY=... && make run-benchmark`

## ディレクトリ構成

- `data/` - Livedoor 抽出済みプレーンテキスト（`data/corpus/livedoor.txt` など）
- `scripts/` - データ処理・ベンチマーク実行スクリプト
- `results/` - ベンチマーク結果
- `mozc_helper/` - Mozc 変換候補シミュレーションモジュール

## 環境要件

- Python 3.8+
- OpenAI API（LLM 用）
- Wikipedia 日本語ダンプ
- 追加ライブラリ: `mwparserfromhell`

## 注意点

- 旧仮名/旧字体など歴史的表記に引きずられないよう、現代表記を優先する評価へシフトしています。
- 既存の `extracted_pattern_code.txt`（Aozora 由来）は互換のため残していますが、Wikipedia 版での再生成を推奨します。
