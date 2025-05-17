<!--
benchmark ディレクトリの README
各モデルのベンチマーク結果（平均応答時間・文字誤り率）をまとめ、傾向分析を行った内容を記載しています。
-->
# ベンチマーク結果まとめ

このディレクトリには、Sumibi クライアントを用いて取得した各モデルの応答時間および文字誤り率（CER: Character Error Rate）を集計・可視化するスクリプトと測定結果が含まれています。なお、Sumibiはローマ字かな漢字変換という性質上、応答時間が約1秒を超えるとユーザビリティが低下します。そのため、応答時間の長いモデルは実用面で不利であり、評価においても高く評価されにくい点に留意してください。

## ディレクトリ構成
- `sumibi_bench.py` / `sumibi_typical_convert_client.py`: ベンチマーク実行用スクリプト
- `aggregate_results.py`: JSON 形式の結果を統合し、CSV に出力するスクリプト
- `plot_mean_response_time.py`: 各モデルの平均応答時間を横長バー＋エラー率折れ線でプロット
- `plot_errorrate_vs_cost.py`: 文字誤り率と API コストの関係をプロット
- `plot_errorrate_vs_paramsize.py`: 文字誤り率とモデルパラメータ数の関係をプロット
- `result/`: 各モデルのベンチマーク結果 JSON ファイル

## 測定結果サマリ

### 平均応答時間（秒）
| モデル名                                | 平均応答時間 |
|----------------------------------------|-----------:|
| gemini-2.0-flash                       |      0.65  |
| gemini-2.0-flash-lite                  |      0.71  |
| gpt-4o                                 |      0.82  |
| gpt-3.5-turbe                          |      0.84  |
| gpt-4.1-mini                           |      0.91  |
| gpt-4o-mini                            |      0.91  |
| gpt-4.1                                |      0.99  |
| deepseek-v3                            |      6.44  |
| gemini-2.5-flash-preview-04-17         |      6.64  |
| o4-mini                                |     21.78  |
| gemini-2.5-pro-preview-05-06           |     28.21  |

### ローマ字かな漢字変換誤り率（CER）
| モデル名                                | CER (%)   |
|----------------------------------------|---------:|
| gemini-2.5-pro-preview-05-06           |     9.38  |
| gemini-2.5-flash-preview-04-17         |    13.78  |
| gpt-4o                                 |    19.74  |
| gpt-4.1                                |    21.89  |
| gemini-2.0-flash                       |    26.65  |
| gemini-2.0-flash-lite                  |    35.65  |
| o4-mini                                |    33.26  |
| deepseek-v3                            |    59.26  |
| gpt-4.1-mini                           |    43.02  |
| gpt-3.5-turbe                          |    72.18  |
| gpt-4o-mini                            |    73.50  |

## 測定結果グラフ

![plot1](../images/plot_errorrate_vs_cost_1000x600.png)

![plot2](../images/plot_mean_response_time_1000x600.png)

## 傾向分析
Sumibi のユーザビリティを左右する主要因として、以下の3 つの観点が重要です。
1. レイテンシー（応答時間）
2. 文字誤り率（CER: Character Error Rate）
3. API 利用コスト（$/リクエスト）

以下ではこれらを組み合わせた傾向を分析します。

### レイテンシーと精度のトレードオフ
- **1秒未満でバランス良好**  
  - `gemini-2.0-flash`：約0.65s / CER約26.7% / $0.00013。最速かつ低コストで実用的。  
  - `gpt-4o`：約0.82s / CER約19.7% / $0.00550。高精度だがコストは高め。  
- **1秒未満だがトレードオフあり**  
  - `gemini-2.0-flash-lite`：約0.71s / CER約35.7% / $0.0000975。コスト最小だが精度はやや犠牲。  
  - `gpt-3.5-turbo`：約0.84s / CER約72.2% / $0.00055。高速かつ中コストだが精度は低い。  
  - `gpt-4.1-mini` / `gpt-4o-mini`：約0.9s前後 / CER高め（43.0%, 73.5%） / $0.00052, $0.000195。用途を選ぶ。  
- **1秒以上で実用性低下**  
  - `gpt-4.1`：約0.99s / CER約21.9% / $0.00260。速度・コストとも中程度。  
  - `deepseek-v3` / `gemini-2.5-flash-preview-04-17`：約6.4〜6.6s / CER約59.3%, 13.8% / $0.000355, $0.000195。低コストだが遅延が大きい。  
  - `o4-mini` / `gemini-2.5-pro-preview-05-06`：約21.8〜28.2s / CER約33.3%, 9.4% / $0.00143, $0.002625。高い遅延 または高コスト。  

### コスト面の評価
- **低コスト重視**  
  - `gemini-2.0-flash-lite`：$0.0000975。最も安価。リアルタイム用途でコストを抑えたい場合に適する。  
  - `gemini-2.0-flash`：$0.00013。高速かつ安価な実用モデル。  
- **中コスト帯のバランス**  
  - `gpt-4o-mini` / `gpt-4.1-mini`：$0.000195〜$0.00052。中程度のコストで、多様な用途に対応。  
- **高コスト・高精度**  
  - `gpt-4o`：$0.00550。高精度だがコストが最大級。  
  - `gemini-2.5-pro-preview-05-06`：$0.002625。高精度かつコストも高め。  

### 総合評価
リアルタイムかつコストを最優先するなら `gemini-2.0-flash-lite` または `gemini-2.0-flash`。  
精度重視でコストを許容できるなら `gpt-4o` や `gemini-2.5-pro-preview-05-06`。  
速度・精度・コストのバランスを取りたい場合は、実際の要件に応じて中コスト帯のモデル（`gpt-4o-mini` / `gpt-4.1-mini`）を検討してください。


## ベンチマーク環境の構築手順

1. Sumibiのリポジトリをgit cloneする

```bash
   git clone git@github.com:kiyoka/Sumibi.git
```
2. https://github.com/azooKey/AJIMEE-Bench のソースコード一式をダウンロードする

3. ./Sumibi/benchmarkAJIMEE-Benchにダウンロードしたソースコード一式を展開する

## 実行手順
1. ベンチマーク実行 ( google のgemini-2.0-flashのベンチマークデータ取得の例)
   ```bash
   export SUMIBI_AI_API_KEY="AIxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   export SUMIBI_AI_BASEURL=https://generativelanguage.googleapis.com/v1beta/openai/
   export SUMIBI_AI_MODEL=gemini-2.5-flash-preview-04-17   
   make result/gemini-2.0-flash.json
   ```  

2. 結果集計
   ```bash
   make aggregate 
   ```  
   
3. プロット生成
   ```bash
   make plots
   ```
