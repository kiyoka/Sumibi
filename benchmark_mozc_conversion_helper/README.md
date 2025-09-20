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


pip install -r benchmark_mozc_conversion_helper/requirements.txtを実行しました


抽出を実行してください。ただし、実行前に venv/bin/activate を実行してください。

次の手順に進んでください。

はい、実装してください。

ベンチマークはこちらで実行します。

wikipediaのダウンロードデータはどこになりますか？まだダウンロードしていませんか？

wikipedia以外に小さいサイズのコーパスはありますか？現代の日本語が書かれたものが良いです。

それでは、wikipedia要約ダンプに切り替えます。このベンチマークには巨大なコーパスは不要です。

jawiki-latest-abstract.xml.gzのURLを教えてください

abstractのファイルが404で見つかりません。どうしましょうか。

https://dumps.wikimedia.org/other/cirrussearch/current/ が404なります。

ダウンロード開始しました。22時間かかります。

Wikipedia以外の選択肢を探してください。

Livedoorニュースコーパスに切り替えてください。

以下の作業が完了しました。
  - 展開と配置
      - mkdir -p external/LDCC
      - tar -xzf ldcc-20140209.tar.gz -C external/LDCC
      - 中身が external/LDCC/text/... に並べばOK

このファイル名を変更してください。もうWikipediaではありません。
benchmark_mozc_conversion_helper/scripts/extract_patterns_from_corpus.py

このパス名もおかしいです。wikiではありません。修正お願いします。
data/corpus/livedoor.txt

既に `data/wiki/livedoor.txt` の後方互換は廃止しました。新パス `data/corpus/livedoor.txt` を使用してください。

mada livedoorのコンテンツで llm_selection_benchmark.py を更新していませんでした。対応お願いします。



llm_selection_benchmark.pyの extracted_pattern_code.txtが無い場合のみ埋め込み（旧Aozora 113件）にフォールバックは削除してください。
extracted_pattern_code.txtがないときはファイルがない旨のエラーを表示してください。
また、test_embedded_cases.py もファイル件数に合わせて動的検証に変更してください。


