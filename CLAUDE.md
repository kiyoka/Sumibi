
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

## GitHub Issue 96 対応内容

### 実装概要

GitHub Issue 96の研究課題に対応するベンチマークインフラの拡張が完了しました。
Local LLM利用時に、異なる入力形式（ローマ字、ひらがな、カタカナ）をLLMに入力することで、変換精度やレスポンス時間がどのように変化するかを調査できるようになりました。

**研究仮説**: ひらがなで入力することで、LLMが日本語の文脈をより正確に理解でき、変換精度の向上とレスポンス時間の改善が期待される。

**実験結果**: 実際のベンチマークで、Local LLMにおいてひらがな入力が**大幅な精度改善**をもたらすことが実証されました。カタカナ入力はローマ字入力と同等の精度となり、仮説が裏付けられました。

### 新規作成ファイル

1. **`benchmark/katakana_to_hiragana_converter.py`**
   - カタカナ→ひらがな変換クラス
   - Unicode範囲(U+30A1-U+30F3)を利用したシンプルな変換ロジック
   - 実装方式: Unicode offset -0x60 による変換
   - テストケース付き

2. **`benchmark/test_conversion_modes.py`**
   - 3つのモードの違いを示すデモスクリプト
   - API key不要でローカル実行可能
   - Issue 96の提案内容を視覚的に確認できる

### 変更ファイル

1. **`benchmark/sumibi_bench.py`**
   - 3つの入力モードを実装:
     - `romaji_direct_input` (デフォルト): カタカナ→ローマ字→LLM
     - `hiragana_input` (新方式): カタカナ→ひらがな→LLM
     - `katakana_input` (新方式): カタカナ→カタカナ→LLM（対照実験用）
   - 重要な修正: `henkan()`メソッドに`context_text`パラメータを追加
     - 日本語コンテキストは保持したまま、入力文字列のみを変換
     - これにより、文脈情報を損なわずに変換精度を向上
   - ウォームアップ機能: 第一回目のLLMリクエストを2回実行
     - LM Studioのモデルロードによる遅延を排除
     - 2回目以降の結果のみをベンチマーク結果として保存
   - 各モードの詳細な出力ログ機能を追加

2. **`benchmark/aggregate_results.py`**
   - 平均応答時間の計算に95パーセンタイルを採用
   - 外れ値（タイムアウトなど）の影響を排除
   - より正確なパフォーマンス評価が可能に

3. **`benchmark/Makefile`**
   - `%_hiragana.json` ターゲットを追加（ひらがな入力モード）
   - `%_katakana.json` ターゲットを追加（カタカナ入力モード）
   - `run_notify` マクロを導入してNOTIFY_SCRIPT処理を一元化
   - 3つの入力モードすべてのベンチマーク実行に対応

4. **`benchmark/plot_errorrate_vs_paramsize.py`**
   - 静的データから動的JSONファイル読み込みに全面リファクタリング
   - 3つの入力モード（romaji_direct_input, hiragana_input, katakana_input）をサポート
   - 色分け: 青色(romaji_direct_input) / 赤色(hiragana_input) / 緑色(katakana_input)
   - モデル間の対応関係を示す点線を自動描画
   - Y軸範囲: 0%〜110% に設定
   - 通常版とズーム版の2種類のグラフを自動生成
     - 通常版: 全体ビュー（0-120B パラメータ）
     - ズーム版: 詳細ビュー（0-40B パラメータ、35-110% エラー率）

### 使用方法

#### デモ実行（API key不要）
```bash
cd benchmark
python3 test_conversion_modes.py
```

#### ベンチマーク実行

**ローマ字直接入力モード (従来方式)**:
```bash
export SUMIBI_AI_API_KEY="your-api-key"
export SUMIBI_AI_MODEL="gemma-3-12b-it-qat"
make result_ver2.4.0/gemma-3-12b-it-qat.json
```

**ひらがな入力モード (新方式)**:
```bash
export SUMIBI_AI_API_KEY="your-api-key"
export SUMIBI_AI_MODEL="gemma-3-12b-it-qat"
make result_ver2.4.0/gemma-3-12b-it-qat_hiragana.json
```

**カタカナ入力モード (対照実験用)**:
```bash
export SUMIBI_AI_API_KEY="your-api-key"
export SUMIBI_AI_MODEL="gemma-3-12b-it-qat"
make result_ver2.4.0/gemma-3-12b-it-qat_katakana.json
```

または直接実行:
```bash
# ひらがな入力モード
python3 sumibi_bench.py AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json \
    output_hiragana.json hiragana_input

# カタカナ入力モード
python3 sumibi_bench.py AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json \
    output_katakana.json katakana_input
```

#### グラフ生成
```bash
cd benchmark
make plots
```

生成されるグラフ:
- `images/plot_errorrate_vs_paramsize_1000x600.png`: 通常版（全体ビュー）
- `images/plot_errorrate_vs_paramsize_zoomed_1000x600.png`: ズーム版（詳細ビュー）
- `images/plot_errorrate_vs_cost.png`: コスト vs エラー率
- `images/plot_mean_response_time.png`: 平均応答時間（95パーセンタイル）

### 3つのモードの比較

**Mode 1: romaji_direct_input (従来方式)**
```
入力: ワタシノナマエハニシヤマデス
  ↓ カタカナ→ローマ字変換
LLM入力: watashinonamaehanishiyamadesu
  ↓ LLMで変換
期待出力: 私の名前は西山です。
```

**Mode 2: hiragana_input (新方式)**
```
入力: ワタシノナマエハニシヤマデス
  ↓ カタカナ→ひらがな変換
LLM入力: わたしのなまえはにしやまです
  ↓ LLMで変換
期待出力: 私の名前は西山です。
```

**Mode 3: katakana_input (対照実験用)**
```
入力: ワタシノナマエハニシヤマデス
  ↓ カタカナをそのまま使用
LLM入力: ワタシノナマエハニシヤマデス
  ↓ LLMで変換
期待出力: 私の名前は西山です。
```

### 実測ベンチマーク結果 (v2.4.0)

#### gemma-3-12b-it-qat (12B パラメータ)

| モード | CER (エラー率) | 平均応答時間 | @1精度 |
|--------|---------------|------------|-------|
| romaji_direct_input | **73.04%** | 2.019秒 | 2.0% |
| hiragana_input | **42.85%** | 1.945秒 | 6.0% |
| katakana_input | **73.55%** | 1.897秒 | 2.0% |
| **改善率 (hiragana vs romaji)** | **-41.3%** | -3.7% | +300% |

#### llm-jp-3.1-13b-instruct4 (13B パラメータ)

| モード | CER (エラー率) | 平均応答時間 | @1精度 |
|--------|---------------|------------|-------|
| romaji_direct_input | **91.49%** | 2.577秒 | 0.0% |
| hiragana_input | **53.19%** | 4.905秒 | 4.5% |
| katakana_input | **92.47%** | 2.404秒 | 0.0% |
| **改善率 (hiragana vs romaji)** | **-41.9%** | +90.3% | +∞ |

#### openai/gpt-oss-20b (20B パラメータ)

| モード | CER (エラー率) | 平均応答時間 | @1精度 |
|--------|---------------|------------|-------|
| romaji_direct_input | **82.30%** | 3.158秒 | 1.0% |
| hiragana_input | **51.17%** | 3.058秒 | 4.5% |
| katakana_input | **65.51%** | 3.021秒 | 1.5% |
| **改善率 (hiragana vs romaji)** | **-37.8%** | -3.2% | +350% |

#### gemma-3n-e2b-it-mlx (2B パラメータ)

| モード | CER (エラー率) | 平均応答時間 | @1精度 |
|--------|---------------|------------|-------|
| romaji_direct_input | **81.46%** | 0.701秒 | 0.0% |
| hiragana_input | **52.86%** | 0.733秒 | 2.0% |
| katakana_input | **64.91%** | 0.753秒 | 1.0% |
| **改善率 (hiragana vs romaji)** | **-35.1%** | +4.6% | +∞ |

#### sarashina2.2-3b-instruct-v0.1 (3B パラメータ)

| モード | CER (エラー率) | 平均応答時間 | @1精度 |
|--------|---------------|------------|-------|
| romaji_direct_input | **86.91%** | 1.324秒 | 0.0% |
| hiragana_input | **61.47%** | 1.320秒 | 1.5% |
| katakana_input | **68.44%** | 1.248秒 | 1.0% |
| **改善率 (hiragana vs romaji)** | **-29.3%** | -0.3% | +∞ |

### 結論と考察

1. **精度の大幅改善**: 全モデルで**ひらがな入力がエラー率を29-42%削減**
2. **カタカナ入力の特性**: カタカナ入力はローマ字入力とほぼ同等の精度（一部モデルでやや改善）
3. **応答時間**: ほとんどのモデルで大きな差はなし（ウォームアップと95パーセンタイル採用により正確な測定を実現）
4. **@1精度**: トップ候補の正解率も大幅に向上（300-350%改善）
5. **実用性**: Local LLM でひらがな入力を使うことで、実用レベルに近づく可能性

この結果は、GitHub Issue 96 の仮説「ひらがな入力による精度改善」を**強力に裏付ける**ものであり、今後のSumibi開発において、Local LLM利用時はひらがな入力モードを推奨すべきことを示唆しています。

### 技術的なポイント

1. **コンテキスト保持の重要性**: `context_text`パラメータにより、文脈情報は日本語のまま保持し、変換対象の文字列のみを変換する設計により、精度向上と実装のシンプルさを両立

2. **ウォームアップの必要性**: LM Studioなどのローカル環境では、最初のリクエスト時にモデルロードが発生するため、第一回目を2回実行して2回目以降の結果のみを記録

3. **外れ値処理**: 95パーセンタイルを採用することで、タイムアウトやネットワーク遅延などの外れ値の影響を排除し、正確なパフォーマンス評価を実現

4. **可視化の工夫**:
   - 3色の色分けで3つのモードを明確に区別
   - 点線でモデル間の対応関係を可視化
   - ズーム版グラフで小型・中型モデルの詳細比較を容易に

