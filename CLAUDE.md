
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


LocalLLMだけでなく、GPT-5などのフロンティアモデルもhiragana_inputで、データ取得したいです。

計画では、今の plot_errorrate_vs_cost_zoomed_1000x600.png はそのままにして、
 romaji_direct_input hiragana_input  katakana_input の3つのデータを比較できるグラフを作りたいです。

APIキーの設定はこちらで行います。

GPT-5に絞ります。

katakana_input も欲しいです。
それも含めて、shell scriptにしてください。

一旦様子を見たいので、次の2つだけを実行します。
  1. gpt-5(medium) - hiragana_input
  2. gpt-5(medium) - katakana_input

make result_ver2.4.0/gpt-5\(medium\)_hiragana.json
python3 sumibi_bench.py ./AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json "result_ver2.4.0/gpt-5(medium)_hiragana.json" hiragana_input
================================================================================
WARMUP PHASE: Executing first entry twice to load LM Studio model
================================================================================

Warmup run 1/2:
Traceback (most recent call last):
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/sumibi_bench.py", line 192, in <module>
    main()
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/sumibi_bench.py", line 186, in main
    bench.benchmark(evaluation_data)
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/sumibi_bench.py", line 149, in benchmark
    self.henkan(expected_output, context_text + romaji_text, romaji_text, katakana_text, context_text, skip_save=True)
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/sumibi_bench.py", line 79, in henkan
    result = self.client.convert(surrounding_text_llm, henkan_text_llm)
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/sumibi_typical_convert_client.py", line 130, in convert
    response = self.client.chat.completions.create(**api_params)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/venv/lib/python3.12/site-packages/openai/_utils/_utils.py", line 287, in wrapper
    return func(*args, **kwargs)
           ^^^^^^^^^^^^^^^^^^^^^
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/venv/lib/python3.12/site-packages/openai/resources/chat/completions/completions.py", line 1150, in create
    return self._post(
           ^^^^^^^^^^^
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/venv/lib/python3.12/site-packages/openai/_base_client.py", line 1259, in post
    return cast(ResponseT, self.request(cast_to, opts, stream=stream, stream_cls=stream_cls))
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/benchmark/venv/lib/python3.12/site-packages/openai/_base_client.py", line 1047, in request
    raise self._make_status_error_from_response(err.response) from None
openai.BadRequestError: Error code: 400 - [{'error': {'code': 400, 'message': 'Invalid JSON payload received. Unknown name "verbosity": Cannot find field.', 'status': 'INVALID_ARGUMENT', 'details': [{'@type': 'type.googleapis.com/google.rpc.BadRequest', 'fieldViolations': [{'description': 'Invalid JSON payload received. Unknown name "verbosity": Cannot find field.'}]}]}}]
make: *** [Makefile:66: result_ver2.4.0/gpt-5(medium)_hiragana.json] Error 1
(venv) kiyoka@bedroom:~/GitHub/Sumibi/benchmark$ 


以下を設定していなかった為です。
export SUMIBI_AI_VERBOSITY=low

result_ver2.4.0の下に以下の2つのファイルがありますが、どのような違いがあるのですか？
gpt-5-mini(minimal).json
gpt-5-mini(minimal+low).json

SUMIBI_AI_VERBOSITY 環境変数が未設定の場合に対応していますか？

修正してください。

SUMIBI_AI_MODELがgpt-5の場合もtemperature = 1.0になるように修正してください。

gpt-5系のベンチマークをシンプルにしたいです。
gpt-5系の場合は、以下の様にしてください。
temperature = 1.0 固定
reasoning_effortをminimal固定
verbosityはlow固定

まだ、verbosityはlow固定になっていません。

以下のコードがまだ直っていません。
        # Set verbosity for gpt-5 models
        verbosity = None
        if model.startswith("gpt-5"):
            # First, check if model name contains +low to auto-set verbosity
            if "+low" in model:
                verbosity = "low"
            # Then, check environment variable (overrides model name)
            verbosity_env = os.getenv("SUMIBI_AI_VERBOSITY", "").strip()
            if verbosity_env in ["low", "medium", "high"]:
                verbosity = verbosity_env


今の sumibi_bench.py は、ファイル名からモデル名を自動的に取り出しているのですか？

そうであれば、以下のようなモデル名は、OpenAIからの提供はありません。gpt-5というモデル名が正解です。
  export SUMIBI_AI_MODEL="gpt-5(medium)"


その実装は必要ありません。

gpt-5(medium)のようなモデル名は廃止します。gpt-5の場合は、reasoning_effortをminimal固定にしてください。

  - ✅ モデル名に+lowが含まれる場合、verbosity="low"を自動設定

gpt-5の場合環境変数に渡されるモデル名は、以下の3パターンしかありません。
export SUMIBI_AI_MODEL="gpt-5"
export SUMIBI_AI_MODEL="gpt-5-mini"
export SUMIBI_AI_MODEL="gpt-5-nano"


以下verbosityも環境変数からは読み込まずにlow固定にしてください。
  | モデル        | temperature | reasoning_effort | verbosity |
  |------------|-------------|------------------|-----------|
  | gpt-5      | 1.0         | minimal (固定)     | 環境変数から設定  |
  | gpt-5-mini | 1.0         | minimal (固定)     | 環境変数から設定  |
  | gpt-5-nano | 1.0         | minimal (固定)     | 環境変数から設定  |


make aggregateした結果のデータで、以下の2つのグラフを更新してください。
plot_errorrate_vs_cost_1000x600.png
plot_errorrate_vs_cost_zoomed_1000x600.png
