# iOS PCCのAJIMEE-Bench v1結果（Issue #215）

iPhone 17e / iOS 27.0.1で、AppleのPrivate Cloud ComputeモデルをFoundation Models APIから呼び出して測定した200問×3入力形式の結果です。ローマ字・ひらがな・カタカナの各JSONは `import_pcc_ios.py` によって、iOS側のベンチマーク専用ブランチ `codex/ajimee-bench-v1` の書き出し結果から生成しました。一般配布アプリのログやユーザー個人の入力内容ではありません。

出典: [AJIMEE-Bench v1](https://github.com/nyanko3141592/AJIMEE-Bench)（日本語Wikipedia入力誤りデータセット v2由来）。入力・左文脈・許容解などの派生データは [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/) に従います。変換結果も同じ設問を含む形で公開しています。元データの200問は `benchmark/JWTD_v2/upstream/JWTD_v2/v2/evaluation_items.json` の `source=v1` 行から照合できます。新版628問全体のスコアとは混合しません。

正規化時に、各ファイルが重複のない200設問IDを網羅し、左文脈・許容解・ローマ字・当該形式の入力が元データと一致することを検証しています。`cer` は許容解との最小Levenshtein距離を許容解の長さで割り、各問で1.0を上限とした値です。安全制限・回答拒否は空回答の不正解として残し、除外していません。`elapsed_sec` はiPhone実測値です。PCCではウォームアップを省略し、既存の外部API計測とプロンプト構造や端末・ガードレール設定も異なるため、順位は参考値です。

再生成（iOS書き出し結果3本を指定）:

```sh
cd benchmark
python3 import_pcc_ios.py /path/to/pcc-romaji.json /path/to/pcc-hiragana.json /path/to/pcc-katakana.json
python3 generate_pcc_comparison.py -o PCC_IOS_COMPARISON.md
python3 plot_pcc_all_models.py -o ../images/plot_pcc_all_models_v1.png
```

JSONのフィールドは既存の `aggregate_results.py` に互換で、追加の `id` と `error` によって設問と安全制限を追跡できます。APIキー・Apple ID・デバイス識別子は含みません。
