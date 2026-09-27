# GPT-6 初回計測結果のバックアップ (Issue #182)

2026-09-27 に取得した gpt-6-sol / gpt-6-luna の初回計測結果。以下の2点で本計測と条件が異なるため、参考値として保存している。

- `sumibi_bench.py` が gpt-6 を認識しておらず、`reasoning_effort` / `verbosity` を指定せず（APIデフォルト）に計測した
- macOS 標準の GNU Make 3.81 がパターンルールを定義順に選ぶため、`_hiragana` / `_katakana` も **ローマ字入力 (romaji_direct_input)** で計測されている。3ファイルとも実質ローマ字入力の結果

| ファイル | 実際の入力形式 | CER | 平均応答時間 (p95除外) |
|---|---|---|---|
| gpt-6-sol.json | ローマ字 | 3.3% | 2.60s |
| gpt-6-sol_hiragana.json | ローマ字 | 3.1% | 2.56s |
| gpt-6-sol_katakana.json | ローマ字 | 3.2% | 2.60s |
| gpt-6-luna.json | ローマ字 | 6.0% | 3.13s |
| gpt-6-luna_hiragana.json | ローマ字 | 5.0% | 3.12s |
| gpt-6-luna_katakana.json | ローマ字 | 5.8% | 2.99s |
