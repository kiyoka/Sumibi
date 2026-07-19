<!--
benchmark ディレクトリの README
各モデルのベンチマーク結果（平均応答時間・文字誤り率）をまとめ、傾向分析を行った内容を記載しています。
-->

# ベンチマーク結果まとめ (計測時期 2025年8月〜2026年4月)

このディレクトリには、Sumibi クライアントを用いて取得した各モデルの応答時間および文字誤り率（CER: Character Error Rate）を集計・可視化するスクリプトと測定結果が含まれています。

Sumibiはローマ字かな漢字変換という性質上、**応答時間が約2秒を超えるとユーザビリティが低下**します。そのため、本ベンチマークでは以下の優先度で評価します：

1. **レイテンシー（応答時間）**: 2秒以内が実用の必須条件
2. **文字誤り率（CER）**: 実用範囲内での精度
3. **API 利用コスト（$/リクエスト）**: 継続利用可能なコスト

## ディレクトリ構成

- 計測スクリプト
  - `sumibi_bench.py` / `sumibi_typical_convert_client.py`: Sumibi (LLM) ベンチマーク実行用スクリプト
  - `google_ime_native_bench.py`: Google日本語入力 (native / mozc_emacs_helper経由) ベンチマーク（ひらがな入力）
  - `google_ime_cgi_bench.py`: Google日本語入力 (CGI API経由) ベンチマーク（ひらがな入力）
- 集計・グラフ生成
  - `aggregate_results.py`: JSON 形式の結果を統合し、CSV に出力
  - `plot_mean_response_time.py`: 各モデルの平均応答時間＋エラー率
  - `plot_errorrate_vs_cost.py`: 文字誤り率と API コストの関係
  - `plot_errorrate_vs_paramsize.py`: 文字誤り率とモデルパラメータ数の関係
  - `plot_errorrate_vs_inputtype.py` / `plot_errorrate_vs_inputtype_hiragana.py`: 入力形式別の文字誤り率比較
  - `plot_context_length_vs_response_time.py`: Context Length 別の応答時間・CER
- ユーティリティ
  - `katakana_to_hiragana_converter.py` / `katakana_to_romaji_converter.py`: 評価データの入力形式変換
- 結果データ
  - `result_ver2.3.0/` / `result_ver2.4.0/`: ベンチマーク結果 JSON（プロンプトバージョン別。`_hiragana` / `_katakana` サフィックスで入力形式を区別、無印はローマ字入力）
- `AJIMEE-Bench/`: 評価データセット（別途ダウンロードして配置）
- `Makefile`: 計測・集計・プロットの実行ターゲット

## 計測方法

### Chat Completion API 呼び出し内容

ベンチマークでは、`sumibi_typical_convert_client.py` 内で以下の Chat Completion API 呼び出しを行っています:

```python
response = self.client.chat.completions.create(
    model=モデル名, 
    temperature=0.8,
    n=1,
    messages=messages,
)
```

### プロンプト (`messages`) の内容

| role      | content |
|-----------|---------|
| system    | あなたはローマ字とひらがなを日本語に変換するアシスタントです。ローマ字の 「nn」 は 「ん」と読んでください。\[\]\(URL\)のようなmarkdown構文は維持してください。\# や \#\# や \#\#\# や \#\#\#\# のようなmarkdown構文は維持してください。ローマ字の字面をそのままひらがなや漢字にするだけで、元のローマ字にない文章を作り出さないでください。出力は変換後の一文のみ。注釈や説明は一切付けないください。もし、入力された文章が英語の文章と判断できた場合は、日本語に翻訳してください。 |
| user      | ローマ字とひらがなの文を漢字仮名混じり文にしてください。 周辺の文章は、「こんにちは、中野です。watashi no namae ha nakano desu . どうぞよろしくお願いします。」のような文章になっています。周辺の文脈を見てそれに合った語彙を選んでください。: watashi no namae ha nakano desu . |
| assistant | 私の名前は中野です。 |
| user      | ローマ字とひらがなの文を漢字仮名混じり文にしてください。周辺の文章は、「説明はここまでです。それ以外は ikano toori desu .」のような文章になっています。周辺の文脈を見てそれに合った語彙を選んでください。: ikano toori desu . |
| assistant | 以下の通りです。 |
| user      | ローマ字とひらがなの文を漢字仮名混じり文にしてください。 周辺の文章は、「{**surrounding_text**}」のような文章になっています。 周辺の文脈を見てそれに合った語彙を選んでください。: {**text**} |

※ **surrounding_text** 部分には「近年知名度gaagattekiteori」のような変換対象の周辺文章を含む文字列が入ります。変換結果は「近年知名度が上がってきており」となります。

※ **text**部分には「gaagattekiteori」のような変換対象の文字列が入ります。

### 入力形式（3方式）

`sumibi_bench.py` の第3引数で、LLMに渡す入力の形式を指定できます：

```bash
# ローマ字入力モード（デフォルト）
python3 sumibi_bench.py evaluation_items.json output_romaji.json romaji_direct_input

# ひらがな入力モード（推奨）
python3 sumibi_bench.py evaluation_items.json output_hiragana.json hiragana_input

# カタカナ入力モード
python3 sumibi_bench.py evaluation_items.json output_katakana.json katakana_input
```

## ベンチマーク環境の構築と実行手順

### セットアップ

1. Sumibiのリポジトリをgit cloneする

   ```bash
   git clone git@github.com:kiyoka/Sumibi.git
   ```

2. https://github.com/azooKey/AJIMEE-Bench のソースコード一式をダウンロードし、`./Sumibi/benchmark/AJIMEE-Bench` に展開する

3. venv環境を作成する

   ```bash
   python3 -m venv venv
   source venv/bin/activate
   pip install --upgrade openai
   ```

### 実行手順

1. LLMベンチマーク実行（Googleのgemini-2.0-flashの例）

   ```bash
   export SUMIBI_AI_API_KEY="AIxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   export SUMIBI_AI_BASEURL=https://generativelanguage.googleapis.com/v1beta/openai/
   export SUMIBI_AI_MODEL=gemini-2.0-flash
   make result_ver2.4.0/gemini-2.0-flash.json

   # ひらがな入力モード / カタカナ入力モードの場合
   make result_ver2.4.0/gemini-2.0-flash_hiragana.json
   make result_ver2.4.0/gemini-2.0-flash_katakana.json
   ```

2. Google日本語入力 (native) ベンチマーク実行

   ```bash
   make google_ime_native
   ```
   前提条件: Google日本語入力がインストールされていること（`mozc_emacs_helper` コマンドが利用可能であること）

3. Google日本語入力 (CGI API) ベンチマーク実行

   ```bash
   make google_ime_cgi
   ```
   前提条件: インターネット接続が必要（Google CGI API を使用）

4. 結果集計

   ```bash
   make aggregate
   ```

5. プロット生成

   ```bash
   make plots
   ```

# 測定結果: クラウドLLM（ローマ字入力）

注記:
  - gpt-5, gpt-5-mini, gpt-5-nanoはreasoning_effortがminimal固定です。(sumibi.elでもminimalを利用)
  - gpt-5.1, gpt-5.2, gpt-5.4, gpt-5.5はreasoning_effortがnone固定です。(sumibi.elでもnoneを利用)
  - `(low)`というサフィックスが付いているモデルは、reasoning_effortをlow指定したケースを指します。
  - gpt-5.6-terra, gpt-5.6-lunaはreasoning_effortがnone固定です（`minimal` は非対応。thinking無効化のため `none` を明示指定）。

### エラー率 vs コスト（全体表示）
![plot1](../images/plot_errorrate_vs_cost_1000x600.png)

### エラー率 vs コスト（ズーム表示）
低コスト・低エラー率の実用的なモデルに焦点を当てた詳細表示（エラー率0-40%、コスト0-0.010$の範囲）
![plot1_zoomed](../images/plot_errorrate_vs_cost_zoomed_1000x600.png)

### 平均応答時間とエラー率
![plot2](../images/plot_mean_response_time_1000x1800.png)

## 傾向分析

### 実用的なモデル（応答時間2秒以内）

| モデル | 応答時間 | CER | コスト/リクエスト | 寸評 |
|--------|---------|-----|------------------|------|
| `gemini-2.0-flash-lite` | 0.66 s | 33.1 % | $0.0000975 | 最速・最安だが精度は控えめ |
| `gemini-2.0-flash` | 0.74 s | 21.2 % | $0.00013 | 速度・精度・コストのバランス最優秀 |
| `gpt-3.5-turbo` | 0.83 s | 64.5 % | $0.00055 | 高速だが誤り率が高い |
| `gpt-4o` | 0.98 s | 13.0 % | $0.0065 | 高精度だが高価 |
| `gpt-4.1-mini` | 0.98 s | 30.8 % | $0.00052 | 中コスト・中精度 |
| `gpt-5.6-luna` | 0.99 s | 15.1 % | $0.00170 | **GPT-5.6系で最速**。低コストで応答性最高クラス |
| `gpt-5.6-terra` | 1.05 s | 7.4 % | $0.004250 | **gpt-5.4相当の精度を高速化**（1.21s→1.05s） |
| `gpt-4o-mini` | 1.06 s | 51.5 % | $0.000195 | 低価格だが精度が課題 |
| `gpt-5-nano` | 1.14 s | 89.4 % | $0.000105 | 超低コストだが精度が不十分 |
| `gpt-5.4` | 1.21 s | 7.7 % | $0.004250 | **実用的なGPT-5系では最高精度**。応答も高速 |
| `gpt-4.1` | 1.28 s | 11.7 % | $0.0065 | 高精度だが高コスト |
| `gpt-5-mini` | 1.28 s | 35.9 % | $0.000525 | 低コストで中程度精度 |
| `gemini-3.1-flash-lite-preview` | 1.60 s | 13.3 % | $0.000425 | 低コストで高精度。**コスパ最優秀** |
| `gpt-5` | 1.86 s | 12.8 % | $0.002625 | 高精度で中程度コスト。2秒以内ギリギリ |

### 実用性に課題があるモデル（応答時間2秒超）

| モデル | 応答時間 | CER | コスト/リクエスト | 寸評 |
|--------|---------|-----|------------------|------|
| `claude-sonnet-4-20250514` | 2.79 s | 12.5 % | $0.0045 | 高精度だが遅延でストレス |
| `claude-sonnet-4-5-20250929` | 2.81 s | 11.6 % | $0.0045 | Sonnet 4より若干精度向上、遅延は同等 |
| `gemini-3-flash-preview` | 2.81 s | 4.1 % | $0.00085 | 非常に高精度（thinking_level: lowで応答時間が大幅改善） |
| `gemini-2.5-flash` | 4.20 s | 11.0 % | $0.00065 | 高精度だが実用には不向き |
| `gpt-5.5` | 4.23 s | 2.7 % | $0.0085 | **GPT-5系最高精度（CER 2.7%）**だが応答時間が実用限界超え |
| `deepseek-v3` | 5.14 s | 29.6 % | $0.000355 | 低コストだが遅すぎる |
| `o3` | 12.77 s | 7.8 % | $0.0065 | 高精度だが遅延が致命的 |
| `o4-mini` | 14.31 s | 19.6 % | $0.00143 | レイテンシー面で完全に不向き |
| `gemini-2.5-pro` | 18.83 s | 4.8 % | $0.001625 | 最高クラスの精度だが日常使用不可 |

## 総合評価

実用性（2秒以内）を必須条件とした場合の推奨モデル：

1. **最優秀バランス**: `gemini-2.0-flash`（0.74s / CER 21.2% / $0.00013）─ 速度・精度・コストのバランスが最も優秀
2. **高精度重視**: `gpt-5.4`（1.21s / CER 7.7% / $0.004250）─ 実用速度で最高精度。精度と速度・コストのバランスでは現時点で実用上の最適解
3. **コスパ最優秀**: `gemini-3.1-flash-lite-preview`（1.60s / CER 13.3% / $0.000425）─ gemini-2.0-flash-liteの後継。CER 33.1%→13.3%と大幅改善しつつ低価格
4. **バランス重視**: `gpt-5`（1.86s / CER 12.8% / $0.002625）─ 高精度を適度なコストで実現
5. **超高速**: `gemini-2.0-flash-lite`（0.66s / CER 33.1% / $0.0000975）─ 最速かつ最安だが精度は劣る

なお `gpt-5.5` はGPT-5系最高精度（CER 2.7%）を達成しているが、4.23sの応答時間はIMEとして実用域を超えており、API高速化が待たれる。

GPT-5.6系の新モデルは実用速度で有望：
- `gpt-5.6-terra`（1.05s / CER 7.4% / $0.004250）─ gpt-5.4と同精度・同価格を維持しつつ応答時間を約20%短縮。GPT-5系の実用モデルの新たな候補
- `gpt-5.6-luna`（0.99s / CER 15.1% / $0.00170）─ GPT-5.6系で最速かつ低コスト。ひらがな入力では CER 3.8% と大幅改善するため、次節も参照

# 入力形式による精度の違い（GitHub Issue #96 の成果）

異なる入力形式（ローマ字、ひらがな、カタカナ）がLLMの変換精度に与える影響を調査しました。その結果、**ローカルLLMだけでなく、各種フロンティアモデルにおいても、ひらがな入力に変更することで劇的に変換精度が改善する**ことが実証されました。

## フロンティアモデルにおける改善効果

| モデル | ローマ字入力 | カタカナ入力 | ひらがな入力 | 改善率 (H/R) |
|--------|-------------|-------------|-------------|--------------|
| gemini-3-pro-preview | **1.6%** | 2.1% | 1.9% | +19%増加 |
| gemini-2.5-pro | 5.9% | 4.0% | **2.2%** | **63%削減** |
| gpt-5.5 | **2.7%** | **2.6%** | 2.8% | +4%増加 |
| gpt-5.6-terra | 7.4% | 6.5% | **3.0%** | **60%削減** |
| gpt-5.6-luna | 15.1% | 10.0% | **3.8%** | **75%削減** |
| gemini-3-flash-preview | 4.1% | **3.8%** | 4.1% | 0% |
| gpt-5.1 | 11.5% | 7.8% | **4.9%** | **57%削減** |
| gpt-4.1 | 11.7% | 9.0% | **5.0%** | **57%削減** |
| gpt-5 | 13.1% | 9.8% | **5.2%** | **60%削減** |
| gemini-2.5-flash | 11.0% | 10.4% | **5.5%** | **50%削減** |
| gpt-4o | 13.0% | 9.3% | **6.6%** | **49%削減** |
| gpt-5.4 | 7.7% | 8.3% | **7.0%** | **9%削減** |
| gemini-2.0-flash | 21.2% | 17.8% | **9.1%** | **57%削減** |
| gpt-5.2 | **10.3%** | 11.0% | 11.2% | +9%増加 |
| gemini-3.1-flash-lite-preview | 13.3% | 13.5% | **12.5%** | **6%削減** |
| gpt-4.1-mini | 30.8% | 20.1% | **14.6%** | **53%削減** |
| gemini-2.0-flash-lite | 33.1% | 23.0% | **15.0%** | **55%削減** |
| gpt-5-mini | 34.4% | 23.5% | **15.4%** | **55%削減** |

## ローカルLLMにおける劇的な改善

| モデル | ローマ字入力 | カタカナ入力 | ひらがな入力 | 改善率 (H/R) |
|--------|-------------|-------------|-------------|--------------|
| gemma-3n-e4b-it-mlx | 68.3% | 47.9% | **37.0%** | **46%削減** |
| gemma-3-12b-it-qat | 73.0% | 58.4% | **42.9%** | **41%削減** |
| gpt-3.5-turbo | 64.5% | 61.8% | **47.8%** | **26%削減** |
| llm-jp-3.1-13b-instruct4 | 91.5% | 67.5% | **53.2%** | **42%削減** |
| gpt-5-nano | 88.0% | 69.9% | **57.3%** | **35%削減** |
| qwen3-vl-8b-instruct-mlx | 90.4% | 75.5% | **62.8%** | **31%削減** |

このほか `openai/gpt-oss-20b`（82.3% → 51.2%）、`sarashina2.2-3b-instruct-v0.1`（86.9% → 61.5%）でも同様のひらがな入力による改善を確認しています。

## 主要な知見

1. **ほぼすべてのモデルでひらがな入力が最高精度**: 調査した多くのモデルで、ひらがな入力が最も低いエラー率を記録
2. **フロンティアモデルでも49-75%の改善**: 最新のGPTやGeminiモデルでも、ひらがな入力により大幅な精度向上。特にgpt-5.6-lunaは75%削減という顕著な改善を示す
3. **gemini-2.5-proが最高精度**: ひらがな入力で2.2%の最低エラー率を達成
4. **カタカナ入力はローマ字とひらがなの中間**: モデルによって改善幅は異なるが、ひらがなほどの効果はない
5. **日本語文脈の理解が鍵**: LLMは日本語文字列（ひらがな）を入力することで、文脈をより正確に理解できる
6. **gpt-5.6-terra ひらがな入力の実用性**: CER 3.0%（gpt-5.5の2.8%に匹敵）を応答時間約1秒で実現し、IME用途で有望

![入力形式別エラー率比較](../images/plot_errorrate_vs_inputtype_1000x600.png)

# ローカルLLM

## ⭐ 2026年4月: ローカルLLMが実用段階に到達 (Issue #132, #141, #158)

長らくローカルLLMはSumibiのIME用途には精度・速度ともに不十分でしたが、**2026年4月リリースの `google/gemma-4-e4b` (4.5Bパラメータ) により、ローカルLLMが初めて実用レベルに到達しました**。さらに同月リリースのMoEモデル `google/gemma-4-26b-a4b` では hiragana CER 15.6% まで改善し、クラウドAPIの上位モデルに迫る精度を達成しました。

### 実測ベンチマーク結果 (MacBook Air M4 / 24GB RAM / LM Studio)

**google/gemma-4-e4b** (Dense 4.5B)

| 入力形式 | CER | 平均応答時間 (95%ile) | 中央値応答時間 |
|---|---|---|---|
| **hiragana_input** | **26.0%** | **2.08秒** | **1.75秒** |
| katakana_input | 38.9% | 2.28秒 | 2.07秒 |
| romaji_direct_input | 64.9% | 3.52秒 | 2.03秒 |

**google/gemma-4-26b-a4b** (MoE 26B / アクティブ 4B)

| 入力形式 | CER | 平均応答時間 (95%ile) |
|---|---|---|
| **hiragana_input** | **15.6%** | 5.60秒 |
| katakana_input | 26.7% | 4.06秒 |
| romaji_direct_input | 42.5% | 4.76秒 |

**mlx-community/gemma-4-26b-a4b-it** (MoE 26B / アクティブ 4B / MLX最適化版)

| 入力形式 | CER | 平均応答時間 (95%ile) | 中央値応答時間 |
|---|---|---|---|
| **hiragana_input** | **18.3%** | **2.21秒** | **2.12秒** |
| katakana_input | 27.5% | 2.16秒 | 2.06秒 |
| romaji_direct_input | 44.5% | 2.17秒 | 2.12秒 |

**google/gemma-4-12b** (Dense 12B)

| 入力形式 | CER | 平均応答時間 (95%ile) | 中央値応答時間 |
|---|---|---|---|
| **hiragana_input** | **22.0%** | **10.16秒** | **9.80秒** |
| katakana_input | 33.9% | 10.43秒 | 10.05秒 |
| romaji_direct_input | 54.4% | 11.63秒 | 11.00秒 |

**gemma-4-12b-it-mlx** (Dense 12B / MLX最適化版)

| 入力形式 | CER | 平均応答時間 (95%ile) | 中央値応答時間 |
|---|---|---|---|
| **hiragana_input** | **17.1%** | **5.76秒** | **5.19秒** |
| katakana_input | 28.2% | 5.35秒 | 4.95秒 |
| romaji_direct_input | 44.2% | 5.60秒 | 4.90秒 |

※ すべて LM Studio (Enable thinking OFF) で計測。

### モデル別の評価

- **`gemma-4-e4b` (Dense 4.5B)**: hiragana CER 26.0%、中央値1.75秒で**実用基準「2秒以内」を初めて達成**。従来のローカルLLMベスト (gemma-3n-e4b-it-mlx: CER 37.0%) を大幅に上回る
- **`gemma-4-26b-a4b` (MoE)**: hiragana CER 15.6%で**シリーズ最高精度**。MoEアーキテクチャによりアクティブパラメータはわずか4BのためMacBook Air 24GBで動作可能。ただし応答は5.60秒と遅い
- **`gemma-4-26b-a4b-it-mlx` (MoE / MLX版)**: Apple Silicon最適化により**応答時間が約2.5倍高速化**（5.60秒 → 中央値2.12秒）。CERは15.6% → 18.3%とわずかに悪化するが、実用水準をほぼ達成
- **`gemma-4-12b` (Dense 12B)**: CER 22.0%と精度は順当だが、中央値約9.8秒とシリーズ内で最も遅い。より軽量・高速なe4b / MoE版26bが上位互換に近く、積極的に選ぶ理由は乏しい
- **`gemma-4-12b-it-mlx` (Dense 12B / MLX版)**: 非MLX版から**速度（中央値9.80秒→5.19秒、約1.9倍）・精度（CER 22.0%→17.1%）の両面で改善**（26b版MLXでCERが微悪化したのとは対照的）。ただし中央値約5秒は実用基準に届かない。新アーキテクチャ `gemma4_unified` のため LM Studio の MLXランタイム **mlx-llm 1.9.0 (beta) 以降**が必要（1.8.5ではロードに失敗）
- **`qwen/qwen3.5-9b` (参考)**: e4bの2倍の9BパラメータでもCER 62.5%・応答10.14秒と実用レベルに達せず。**パラメータ数よりも日本語特化トレーニングの質とアーキテクチャ最適化が支配的**であることを示す

### 実使用での体感

実際にEmacsでSumibiを使って日本語入力を試したところ、**ほとんどストレスなく変換できる**ことが確認されました。ベンチマークの数値以上に体感が良好な理由：

1. **プロンプトキャッシュの効果**: 連続入力ではsystemプロンプト + few-shot部分がLM Studio側でキャッシュされ、2回目以降の変換が大幅に高速化される
2. **実文章は短い入力が多い**: 実際のIME利用では短い単位で変換することが多く、出力トークンも少ない
3. **ネットワーク遅延ゼロ**: クラウドAPIと違いネットワーク往復時間が不要

### ローカルLLM実用化の価値

1. **プライバシー**: 機密文書、企業コード、GPG暗号化ファイルの編集で、クラウドAPI送信の懸念なしに日本語入力可能
2. **オフライン環境での利用**: 飛行機内、電波の弱い場所でも変換可能
3. **API料金ゼロ**: 日常使用で何千回変換しても追加料金なし
4. **遅延の一貫性**: ネットワーク遅延に左右されない安定した応答時間
5. **Apache 2.0ライセンス**: 商用利用可能

### 実用化までの経緯

| 時期 | 状況 |
|---|---|
| 2025年まで | ローマ字入力ではCER 40%以上でIMEには使い物にならず。120Bパラメータ（`gpt-oss-120b(low)` / Amazon Bedrock: CER 59.2%、5.25秒）まで拡張しても改善せず |
| Issue #96 | ひらがな入力への切り替えで全ローカルLLMのエラー率が劇的に改善（約26-46%削減）することを発見 |
| 2026年4月 | `gemma-4-e4b` (4.5B) がひらがな入力でCER 26.0%・中央値1.75秒を達成し、**初めて実用レベルに到達**。クラウドAPIとローカルLLMの両方が実用的な選択肢に |

### エラー率 vs パラメーターサイズ（全体表示）

![plot3](../images/plot_errorrate_vs_paramsize_1000x600.png)

### エラー率 vs パラメーターサイズ（ズーム表示）

0-25Bパラメータの小型・中型モデルの詳細比較。**ひらがな入力（赤色）にすることで、ローマ字入力（青色）と比較して劇的にエラー率が減少する**ことが読み取れます。灰色の点線は同じモデルの異なる入力形式を結んでいます。

![plot3_zoomed](../images/plot_errorrate_vs_paramsize_zoomed_1000x600.png)

## Context Length が精度・速度に与える影響 (Issue #143)

`mlx-community/gemma-4-26b-a4b-it` を対象に、LM Studio の Context Length 設定 (512 / 1024 / 2048 / 4096 / 8192) を変えてベンチマークを計測しました。入力モードは hiragana_input のみ、AJIMEE-Bench (JWTD_v2) 200件。

### 実測ベンチマーク結果

| Context Length | CER (%) | 中央値 (秒) | p95 (秒) |
|---------------|---------|-----------|---------|
| 512           | 19.7%   | 2.13      | 4.12    |
| 1024          | 18.8%   | 2.19      | 4.51    |
| 2048          | 18.4%   | 2.13      | 4.28    |
| 4096          | 18.5%   | 2.14      | 4.35    |
| 8192          | 18.6%   | 2.16      | 4.29    |

![Context Length vs Response Time & CER](../images/plot_context_length_vs_response_time.png)

### 考察と結論

- **CER はすべての Context Length で約19〜20% と変化なし**。Context Length を増やしても日本語変換精度は改善しない
- **レスポンス時間も全体的に大きな差はなく、2.1〜2.2秒前後**で安定している
- **結論**: IME用途においては **Context Length 512〜2048 が最適なバランス**。512 がレスポンス時間の観点でわずかに有利。8192 まで増やしても速度劣化はないため、他の用途と共存させる場合は 2048 程度を推奨

### 計測手順

```bash
# LM Studio で mlx-community/gemma-4-26b-a4b-it をロードし、
# Context Length を 512 に設定してサーバーを起動した後:
cd benchmark
make ctx512

# 同様に 1024, 2048, 4096, 8192 と繰り返す
make ctx1024
make ctx2048
make ctx4096
make ctx8192

# グラフ生成
make ctx_plot
```

## ローカルLLMの実行環境

- GPU 16GByte RAMに収まる場合、基本的には MacBook Air M4 (24GByte memory) を使用
- GPU 16GByte RAMに収まる場合の別の選択肢として、AWS g6.xlarge を使用 (USD 1.3512:Windows/Tokyo region)
- GPU 48GByte RAMに拡張したい場合、AWS g6e.2xlarge を使用 (USD 3.61968:Windows/Tokyo region)

### 利用するEC2インスタンスタイプ

| インスタンス名     | vCPU | メモリ (GiB) | NVIDIA L4 Tensor Core GPU | GPU メモリ (GiB) | ネットワーク帯域幅 (Gbps) | EBS 帯域幅 (Gbps) |
|-------------------|------|--------------|--------------------------|------------------|--------------------------|-------------------|
| **g6.xlarge**     | 4    | 16           | 1                        | 24               | 最大 10                  | 最大 5            |
| g6.2xlarge        | 8    | 32           | 1                        | 24               | 最大 10                  | 最大 5            |
| g6.4xlarge        | 16   | 64           | 1                        | 24               | 最大 25                  | 8                 |

| インスタンス名      | vCPU | メモリ (GiB) | NVIDIA L40S GPU | GPU メモリ (GB) | ネットワーク帯域幅 (Gbps) | EBS 帯域幅 (Gbps) |
|------------------|------|--------------|------------------|------------------|---------------------------|-------------------|
| g6e.xlarge       | 4    | 32           | 1                | 48               | 最大 20                   | 最大 5            |
| **g6e.2xlarge**  | 8    | 64           | 1                | 48               | 最大 20                   | 最大 5            |
| g6e.4xlarge      | 16   | 128          | 1                | 48               | 20                        | 8                 |
