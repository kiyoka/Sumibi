# Mozc Conversion Helper Benchmark

## 概要

このディレクトリは、issue #87 で提案された「LLMを使ったMozc変換候補の知的選択」のベンチマーク環境です。
青空文庫コーパスを使用して、LLMがMozcの変換候補からどれだけ適切な候補を選択できるかを測定します。

## 目的

バックエンドがmozcモードの際に、ローカルLLMを使って文脈に最適な変換候補を自動選択する機能の有効性を検証します。

## ベンチマーク作成手順

1. **パターン抽出**: `extract_patterns_from_aozora.py`で青空文庫から126パターンのmozc_simulatorに対応する文章を抽出
2. **正解データ作成**: `create_ground_truth_data.py`で抽出した文章から正解変換データを生成
3. **テストケース埋め込み**: 抽出した113件のパターンを`llm_selection_benchmark.py`に直接埋め込み
4. **検証ツール**: `test_embedded_cases.py`で埋め込まれたテストケースの整合性を確認

## ディレクトリ構成

- `data/` - 青空文庫テキストデータ
- `scripts/` - データ処理・ベンチマーク実行スクリプト
- `results/` - ベンチマーク結果
- `mozc_helper/` - Mozc変換候補シミュレーションモジュール（127パターン対応）

## 環境要件

- Python 3.8+
- OpenAI/Anthropic/Google API (LLM用)
- 青空文庫コーパスデータ
