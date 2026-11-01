# AJIMEE-Bench JWTD_v2/v2 — Gemini再計測（Issue #205）

## 目的・現在の状態

既存の[旧200件の結果資料](../README.md)とは分離し、拡張版628件でGeminiモデル同士の精度・待ち時間・費用を比較する。**Gemini 3.5 Flash-Lite・3.6 Flash・3.8 Flashの全件計測が完了。** [結果・専用グラフ](REPORT.md)を参照。残りのモデルは未計測のため、全対象の推奨順位は未確定。GPT・Claude・Gemmaは今回計測しない。

上流の`JWTD_v2`の「v2」は元のWikipediaデータセットの名称にも含まれる。ここで旧版／新版と呼ぶのは、その内側の`JWTD_v2/v1`（200件）／`JWTD_v2/v2`（628件）の違い。Sumibiの`result_ver2.4.0`はプロンプト版でありデータセット版ではない。

## 固定するデータ

- 出典: [nyanko3141592/AJIMEE-Bench](https://github.com/nyanko3141592/AJIMEE-Bench)
- コミット: `c0783bba1ec6b9a9e06d07fa52ce3e23d0a7dfe1`
- パス: `JWTD_v2/v2/evaluation_items.json`
- SHA-256: `69a0dde4c1f67b3d06268c761ee3a7b7d38680a4f1c81360cc2d4f0f120e731f`
- 628件（`source=v1` 200件、`source=v2-add` 428件）。新規428件のみの評価ではない。
- 左文脈は使用、予約フィールドの右文脈・正解・original_textはモデルへ渡さない。
- ローマ字・ひらがな・カタカナの3方式。旧版と同じSumibiプロンプト・文字種変換を使う。
- 開発用にtest結果を見ながらプロンプトを調整しない。今回は固定条件で全体を最終比較する。上流READMEの分割件数には不整合があるため、実ファイルで確認した全体628件を採用する。

[データと評価コード](upstream/)は上記コミットから無改変で保存する。[マニフェスト](dataset.json)にハッシュと件数を記録。Wikipedia/JWTD由来部分はCC-BY-SA 3.0、創作例文・評価コードはCC0 1.0。詳しくは[固定した上流README](upstream/README.md)を参照。出典・ライセンスの表示は配布時も保持する。

## 結果を混ぜない仕組み

- v2生結果は`issue205/results/`内の専用JSONのみ。旧`result_ver2.4.0/`は読み書きしない。
- 結果ごとにデータセット出典・パス・版・ハッシュ・件数・評価条件を埋め込む。
- 集計は専用スキーマ、固定データの完全一致、全628件のIDと順序、3方式の完走、プロンプト・文字種変換コードのハッシュを検査。旧JSON、部分計測、異なる版、重複モデルは拒否する。再開機能などランナーだけの変更は別途ハッシュを記録する。
- 旧v1の200件はv2資料の「内訳」としてのみ掲載。過去のv1測定と混ぜたランキングは作らない。
- 今後生成するグラフ・表のタイトルも「JWTD_v2/v2・628件・Geminiのみ」とし、旧画像へ上書きしない。

## モデルと参考費用

2026-10-31に[公式提供状況](https://ai.google.dev/gemini-api/docs/deprecations)・[Standard料金](https://ai.google.dev/gemini-api/docs/pricing)を確認。1モデルは評価1,884回＋ウォームアップ6回＝**1,890回**。固定500入力・200出力トークン/回の参考値は以下。実使用量・思考分・請求額は異なり、費用上限ではない。

| 対象 | 推論指定 | temperature | 1,890回の参考USD | 注意 |
|---|---|---|---:|---|
| gemini-2.5-pro | 省略 | 省略 | 4.961250 | 既存利用プロジェクトに利用制限、思考コストに注意 |
| gemini-2.5-flash | 省略 | 省略 | 1.228500 | 既存利用プロジェクトに利用制限 |
| gemini-3-flash-preview | low | 1.0 | 1.606500 | preview |
| gemini-3.5-flash-lite | minimal | 省略 | 1.228500 | 疎通確認の先行対象 |
| gemini-3.6-flash | minimal | 1.0 | 2.126250 | モデル固有の最低推論設定 |
| gemini-3.7-flash | low | 1.0 | 2.126250 | 3.8へ自動転送。互換確認扱い、独立した順位には含めない |
| gemini-3.8-flash | low | 1.0 | 2.126250 | 導入価格2026-12-31まで |
| gemini-4-argon | low | 省略 | 5.670000 | 導入価格2026-11-30まで |

8モデル合計15,120回・**$21.073500**（固定トークン仮定）。3.7互換確認を全件行うかは開始前に確認する。独立7モデルなら13,230回・$18.947250。疎通各9回は別（8モデルで72回・約$0.100350）。いずれも上限ではなく、特にPro等の思考で増える可能性がある。

以下は提供終了のため再送せず、未実施と記録する。別名へ置換しない。

| 対象 | 提供終了日 |
|---|---|
| gemini-2.0-flash / gemini-2.0-flash-lite | 2026-06-01 |
| gemini-3-pro-preview | 2026-03-09 |
| gemini-3.1-flash-lite-preview | 2026-05-25 |

取得可能か・実際の返却モデルは疎通で確認する。権限エラー・レート制限時は停止し、追加費用やモデル置換を伴う再試行は自動実行しない。2.5は思考設定を省略した旧条件を維持し、全モデルが同じ思考量とは主張しない。3.7は転送先に合わせlowを使うため旧none設定とは異なる。

## 実行手順

作業ブランチ: `codex/issue-205-gemini-jwtd-v2`。フォルダーは既存を再利用する。

```sh
cd /Users/kiyoka/.codex/worktrees/issue-198-gpt-6-1-terra/Sumibi
python3 benchmark/jwtd_v2.py plan
python3 -m unittest discover -s benchmark -p 'test_*.py'
```

**APIを呼ぶ直前に、普段のスクリプトでこのシェルに`SUMIBI_AI_API_KEY`を設定する。設定済みなら再入力不要。キーをチャットに貼らない。** OpenAI用キー・キーチェーン・環境の別接続先へフォールバックしない。Google公式OpenAI互換APIのみを使う。

まず3.5 Flash-Liteを9回だけ実行する（各方式1件＋各2回ウォームアップ、固定トークン参考$0.005850）。

```sh
python3 benchmark/jwtd_v2.py run --model gemini-3.5-flash-lite --limit 1 --output benchmark/issue205/results/gemini-3.5-flash-lite/smoke.json
```

完了後に結果を確認してから全件へ進む。未承認の全モデル一括実行はしない。

```sh
python3 benchmark/jwtd_v2.py run --model gemini-3.5-flash-lite --output benchmark/issue205/results/gemini-3.5-flash-lite/full.json
```

既存ファイルは上書き・再開せず停止。SDK再試行0、タイムアウト既定120秒。失敗した場合も1回ごとの記録を保存。HTTPエラーはクラス・ステータスだけを保存し、生の例外本文・ヘッダー・キーを保存しない。

### エラー後の明示的な途中再開

通常の`run`は既存出力を上書きせず、自動再試行もしない。503等で停止したときは原因を確認し、**`--resume-from`と新しい出力名を明示した場合だけ**途中から再開できる。

Gemini 3.6 Flashはローマ字473件＋ウォームアップ2回の成功後、474件目で503となった。この記録から再開する場合:

```sh
python3 benchmark/jwtd_v2.py run --model gemini-3.6-flash --resume-from benchmark/issue205/results/gemini-3.6-flash/full.json --output benchmark/issue205/results/gemini-3.6-flash/full-resumed.json
```

- 成功済み475回は再送しない。残り1,415回を行う（失敗した474件目の再送は含む）。後続方式の未実施ウォームアップは通常どおり行う。
- 元の`full.json`は変更しない。新しい出力に成功済み行を引き継ぎ、失敗行は`failed_attempts`、再開元のハッシュ・時刻・新ランナーのハッシュは`resume_history`に保存する。
- 元データ、件数、入力の順序・文字列、モデル、推論設定、接続先、料金、タイムアウト、プロンプト・文字種変換のハッシュが不一致なら送信前に停止する。
- もう一度停止した場合は、最新の未完了ファイルを`--resume-from`に指定し、さらに新しい出力名を使う。成功済みのウォームアップや評価行は再送しない。
- 完了後の集計には最終的な完走ファイルだけを渡す。途中ファイルを一緒に集計しない。
- 失敗試行も費用・使用量の監査に含め、不明は不明のまま数える。精度・応答時間は各評価位置の成功応答（ブロックを含む）を1回ずつ使い、失敗待ち時間・停止中の時間・再開までの待ち時間は含めない。再開により計測日時が分かれることも報告する。

再開を含む旧・新ランナーの監査ハッシュは保持するが、プロンプトと変換処理を変えて再計測を混ぜることは許容しない。途中再開のテストはAPIを呼ばずに行っている。

## 評価・報告

主指標は正規化しないMinCER（各例で許容正解中の最小CERを取り、項目平均）。完全一致率を併記。幅正規化完全一致率は別指標として表示し、CERは勝手に正規化しない。上流評価コードを固定し、タグ・source・domain別も集計。ブートストラップ95%信頼区間はseed=42・1,000リサンプルと明示する。

応答時間は全件平均・中央値・nearest-rank p95・p95以下平均・最大。ブロックや空出力は除外せず評価に含め、HTTPエラーを含む未完走はランキングに入れない。費用はウォームアップ込みの返却使用量から計算し、内訳不明差分・費用不明件数を併記する。

```sh
python3 benchmark/jwtd_v2.py summarize benchmark/issue205/results/gemini-3.5-flash-lite/full.json --output benchmark/issue205/summary.json
python3 benchmark/plot_jwtd_v2.py benchmark/issue205/summary.json --output-dir benchmark/issue205/figures
```

2モデル版の再集計（既存出力は保持し、再生成時は新しい出力名を指定）:

```sh
python3 benchmark/jwtd_v2.py summarize benchmark/issue205/results/gemini-3.5-flash-lite/full.json benchmark/issue205/results/gemini-3.6-flash/full-resumed.json --output benchmark/issue205/summary-2models.json
python3 benchmark/plot_jwtd_v2.py benchmark/issue205/summary-2models.json --output-dir benchmark/issue205/figures-2models
```

返却モデル、日時、設定、ソースハッシュも保存する。3.7の転送確認は`alias_checks`として独立モデルの`models`から分離する。結果資料・グラフの作成と結論は実測後に行う。

最新の3モデル比較:

```sh
python3 benchmark/jwtd_v2.py summarize benchmark/issue205/results/gemini-3.5-flash-lite/full.json benchmark/issue205/results/gemini-3.6-flash/full-resumed.json benchmark/issue205/results/gemini-3.8-flash/full.json --output benchmark/issue205/summary-3models.json
python3 benchmark/plot_jwtd_v2.py benchmark/issue205/summary-3models.json --output-dir benchmark/issue205/figures-3models
```
