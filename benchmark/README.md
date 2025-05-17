<!--
benchmark ディレクトリの README
各モデルのベンチマーク結果（平均応答時間・文字誤り率）をまとめ、傾向分析を行った内容を記載しています。
-->
# ベンチマーク結果まとめ

このディレクトリには、Sumibi クライアントを用いて取得した各モデルの応答時間および文字誤り率（CER: Character Error Rate）を集計・可視化するスクリプトと測定結果が含まれています。

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

### 文字誤り率（CER）
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

## 傾向分析
- **高速＋低エラーを両立**: `gemini-2.0-flash` 系は最速（約0.65s）かつ比較的低い誤り率（約26.7%）を実現し、コストパフォーマンスが高い。
- **高精度モデルの遅延**: `gemini-2.5-pro-preview-05-06` は最も低い CER（約9.4%）だが、最長の応答時間（約28.2s）を要し、用途によってはリアルタイム性を犠牲にする。
- **バランス型モデル**: `gpt-4o` は応答時間（約0.82s）・誤り率（約19.7%）ともに優秀で、実用的なトレードオフを提供。
- **小型モデルの限界**: `gpt-4o-mini` は応答時間（約0.91s）こそ悪くないものの、CER が高め（約73.5%）で、変換精度を求める場面には不向き。
- **大規模モデルの遅延と精度**: `o4-mini` や `gemini-2.5` 系はモデルサイズが大きく品質は高いが、応答時間が数秒〜数十秒に及ぶため、インタラクティブ用途には要注意。

## 実行手順
1. ベンチマーク実行
   ```bash
   python3 sumibi_bench.py
   ```  
2. 結果集計
   ```bash
   python3 aggregate_results.py
   ```  
3. プロット生成
   ```bash
   python3 plot_mean_response_time.py
   python3 plot_errorrate_vs_cost.py
   python3 plot_errorrate_vs_paramsize.py
   ```

----
*最終更新: 2025-05-16*  