# AJIMEE-Bench (Advanced Japanese IME Evaluation Benchmark)

このレポジトリはAJIMEE-Bench(味見ベンチ)の評価データを配置しています。

## 参考値（2026-07-21実施、v2は628件時点）

同一環境（`scripts/run_eval.py`、幅ゆれ補正あり）で計測した accuracy@1。値は目安であり、
デコード設定（プロンプト形式・context利用有無等）によって変動します。詳細・タグ別ブレークダウンは
[`JWTD_v2/v2/reference_scores/`](JWTD_v2/v2/reference_scores/) を参照してください。

| モデル | v1 (200件) | v2 (628件) |
|---|---|---|
| mozc（従来型・ベースライン、no-context） | 49.5% | 40.6% |
| zenz-v2.5-xsmall（no-context） | 67.5% | 59.1% |
| zenz-v3.1-xsmall（no-context） | 65.5% | 58.1% |
| zenz-v3.1-small（no-context） | 84.5% | 73.2% |

v1では高精度なモデル同士の差が付きにくく（zenz-v2.5-xsmall と zenz-v3.1-xsmall の95%信頼区間が
v2でも重なる）、v2はより厳しい難所を含むため全体的にスコアが下がり、モデル間の実力差
（特にサイズ差）が見えやすくなっています。mozcとの比較により、ニューラルモデルのスコアが
絶対的にどの水準かも分かります。`domain: production-feedback`（実運用フィードバック由来）では
zenz-v2.5-xsmall(65.0%)とzenz-v3.1-xsmall(50.0%)で15pt差が出ており、実際の弱点を突く
データとして機能しています。

## ディレクトリ

### `JWTD_v2/v1`

[日本語Wikipedia入力誤りデータセット (v2)](https://nlp.ist.i.kyoto-u.ac.jp/?日本語Wikipedia入力誤りデータセット)をベースに構築したデータです。

**ライセンスは元データに準拠し、CC-BY-SA 3.0に従います**。

以下はデータサンプルです。

* `index`は元データにおける行番号を示します
* `context_text`は（ある場合）左文脈を示します
* `input`はカタカナ表記された入力を示します
* `expected_output`は許容可能な変換候補を列挙します
* `original_text`は元データにおける文章全体にあたります
* `splitted_input_for_limited_input_length`は入力長の制約がある手法で評価するために入力を区切った場合の分割位置を示すデータです。入力が十分に長い場合準備されています

```json
{
  "index": "1890",
  "context_text": "",
  "input": "カケイヲタスケルタメ、リョウシュノイエニホウコウシ",
  "expected_output": [
    "家計を助けるため、領主の家に奉公し",
    "家計を助ける為、領主の家に奉公し"
  ],
  "original_text": "家計を助けるため、領主の家に奉公している。",
  "splitted_input_for_limited_input_length": []
}
```

このデータは以下の手順で構築しています。

* 日本語Wikipedia入力誤りデータセット (v2)のテストデータから`kanji-conversion_a`タイプの誤りを含む項目を200件サンプリングしました。これは全体の約20%にあたります。
* ついで、200件のうち100件を左文脈付きかな漢字変換、100件を条件のないかな漢字変換用のデータとしました。
* それぞれのデータにおいて、データの一部分を切り出し、これを変換対象のテキストとします。変換対象のテキストの読み推定を行い、これを入力とします。
* 入力（読み）及び区切りの妥当性を人手でチェック・修正し、変換候補が一意に定まらない場合は許容解を列挙しました。

### `JWTD_v2/v2`

v1 と同じ出典（日本語Wikipedia入力誤りデータセット v2、CC-BY-SA 3.0）から構築した拡張版です。v1 からの変更点:

* v1 の全項目を含み、新規項目を追加しています（`source` フィールドで `v1` / `v2-add` を区別）
* 各項目に難所の種類を示す `tags` を付与しています。層別評価に利用できます
* `right_context_text`（右文脈）フィールドを予約しています。zenz-v3.2 のような右文脈対応モデルの将来的な評価のためのもので、**現時点の評価・スコアリングでは使用しません**。取得できていない項目は空文字です
* Wikipedia 以外の文体（口語チャット・話し言葉・ビジネス・技術/Web・`production-feedback`）のオリジナル項目を追加しています。`domain` フィールドで区別できます（JWTD/Wikipedia 由来は空文字）。これらは実在の文章の引用ではなく本ベンチのために新規作成したもので、この部分のライセンスは CC0 1.0 Universal です
* `domain: production-feedback`（40件）は、azooKey/Zenzai の実運用フィードバック（ユーザーがIMEの初期候補を選ばず別の候補を選び直した記録）から、**個人情報（ユーザー名・入力文全体・周辺文脈）を完全に除去**し、実際に生じた同音異義語の混同パターン（読み・正しい変換・誤変換の組）だけを抜き出して、それを難所として新規に創作した例文です。元の入力文・文脈は一切保持・参照していません

#### データ内訳（全628件）

`source` 別:

| source | n | 説明 |
|---|---|---|
| `v1` | 200 | v1 の全項目（改変なし、タグのみ付与） |
| `v2-add` | 428 | v2 で新規追加した項目 |

`domain` 別（`v2-add` の内訳。Wikipedia/JWTD 由来は空文字）:

| domain | n | 出典 |
|---|---|---|
| （空文字、Wikipedia/JWTD由来） | 482 | v1 の200件 + JWTD v2 の未使用項目から新規抽出した282件 |
| `production-feedback` | 40 | azooKey/Zenzai の実運用フィードバックの誤りパターンを元に新規創作 |
| `tech-web` | 30 | 技術ブログ・レビュー・SNS投稿風の新規創作 |
| `colloquial-chat` | 26 | LINE・チャット風の口語の新規創作 |
| `colloquial-speech` | 25 | 話し言葉の書き起こし風の新規創作 |
| `business` | 25 | ビジネスメール・社内チャット風の新規創作 |

難所タグ別（1項目に複数タグが付く場合があるため合計は628を超える）:

| tags | n |
|---|---|
| `homophone` | 510 |
| `number-notation` | 89 |
| `context-dependent` | 78 |
| `proper-noun` | 60 |
| `functional-word` | 60 |
| `orthographic-variant` | 69 |
| `colloquial-form` | 47 |
| `general` | 37 |
| `unpredictable-name` | 23 |

左文脈（`context_text`）ありの項目: 253件（うち v1 の100件を含む）

dev/test/legacy 分割後の件数は [dev / test 分割](#dev--test-分割) を参照（dev 298 / test 130 / legacy_v1 200）。

| タグ | 意味 |
|---|---|
| `homophone` | 同音異義語の選択が難所 |
| `proper-noun` | 固有名詞に一般的な変換ルールで対応できる表記選択がある |
| `unpredictable-name` | 人名等の固有名詞の表記が、文脈からは一意に決まらず固有知識がないと当てられない（かな漢字変換の限界というより知識の限界） |
| `number-notation` | 数字表記の選択（漢数字「三百」／算用数字「300」）や数字の読み下しが論点。半角/全角の幅ゆれ*だけ*が争点の項目には付けない |
| `context-dependent` | 左文脈がないと正解が一意に決まらない |
| `orthographic-variant` | 送り仮名・かな漢字の表記ゆれが論点 |
| `functional-word` | 機能語・補助用言の漢字/かな選択が難所 |
| `colloquial-form` | 口語特有の表現（縮約形・助詞落ち等）の変換が難所 |
| `general` | 上記いずれにも当たらない |

JWTD 由来の新規項目は以下の手順で構築しています。

* 元データのテストデータから `kanji-conversion_a` タイプの誤りを含む未使用の項目をサンプリングし、誤り箇所を含むセグメントを句読点境界で切り出し（`scripts/build_candidates.py`。人名を含むセグメントは読み推定の信頼性が低いため除外）
* 形態素解析（fugashi + UniDic）で読みを推定し、読みが取れない箇所（数字・固有名詞等）はLLMで補完
* LLMで許容解の列挙とタグ付けを行い、読みは fugashi・LLM・Gemini API の3者でクロスチェック（`scripts/crosscheck_readings.py`）した上で、全件を人手でチェック・修正（`scripts/make_review_sheet.py` によるレビューシートを使用）

Wikipedia 以外のドメインのオリジナル項目は、各文体で変換の難所を意図的に含む文を新規作成し、同様に読みをクロスチェック・人手確認して構築しています。

### dev / test 分割

`evaluation_items.json` に加えて、`dev.json` / `test.json` / `legacy_v1.json` を用意しています
（`scripts/split_dev_test.py` で生成、再現可能）。

* **`legacy_v1.json`**: v1 の200件そのもの。既に公開・利用実績があるためこれ単体では新規性のある
  test setにならないが、v1 との後方互換のスコア比較に使う
* **`dev.json` / `test.json`**: v1 以外の新規項目（388件）を `domain` ごとに層化して 7:3 に分割
  （dev 270件・test 118件）。AIによるループ改善では、**開発中は `dev.json` だけを見て**
  プロンプトやモデルを調整し、`test.json` は最終比較のときだけ開く運用を推奨します。
  同じ固定セットに繰り返しモデルを当て続けると、ベンチマークへの暗黙の過学習
  （リーク）が起きるためです。予算管理（アクセス回数の制限など）は呼び出し側の
  運用に委ねており、本リポジトリはファイルの分割のみを提供します

## 評価方法

Python向けの評価の実装を`utils.py`にまとめています。適宜ご利用ください。

* `calculate_accuracy_at1` / `calculate_MinCER`: 項目単位の指標
* `evaluate_items`: データセット全体の評価。タグ別の層別スコアと、accuracy@1・MinCER 双方のブートストラップ95%信頼区間を返します
* **主指標の推奨**: `evaluate_items` の戻り値は `primary_metric: "MinCER"` を含みます。accuracy@1 は二値（完全一致か否か）のため、小さな改善を検出する力が弱く、近いバージョン間の比較では信頼区間が重なりやすいことを確認しています。**僅差のモデル比較では MinCER（連続値）を優先して見ることを推奨します**。accuracy@1 は結果の直感的な説明に向いた補助指標として引き続き提供します
* `normalize_width` / `normalize=True` オプション: 半角/全角の表記ゆれ（`３００` と `300`、`ＮＨＫ` と `NHK`）はかな漢字変換の本質的な難所ではなく IME の表記正規化の問題です。`calculate_accuracy_at1` と `evaluate_items` に `normalize=True` を渡すと幅ゆれを吸収して判定できます。**デフォルトは `False`（従来どおりの完全一致）で、既存の評価結果に影響しません**。漢数字と算用数字の違い（変換の選択）は正規化されず区別されます

また、評価コードのテストが`test_utils.py`です。

両ファイルともにライセンスはCC0 1.0 Universalです。

## データ構築スクリプト

`scripts/` に v2 データの構築・レビュー用スクリプトを置いています（ライセンスは CC0 1.0 Universal）。

* `build_candidates.py`: JWTD v2 から候補項目を抽出し読みを推定
* `make_review_sheet.py`: 人手レビュー用のHTMLシートを生成（承認/修正/棄却、JSON書き出し）
* `merge_approved.py`: レビュー済み項目を評価データに合成
* `crosscheck_readings.py`: Gemini API による読みのクロスチェック（`GEMINI_API_KEY` が必要）
* `split_dev_test.py`: 新規項目を domain 層化で dev/test に分割
* `run_eval.py`: モデルの出力を評価するCLI。`[{"index": ..., "hypothesis": ...}, ...]` 形式の
  予測ファイルを渡すと、全体スコアとタグ別ブレークダウン（苦手な順）を表示します

  ```bash
  uv run python scripts/run_eval.py \
    --items JWTD_v2/v2/dev.json \
    --predictions my_model_outputs.json \
    --normalize-width
  ```

* `run_mozc_baseline.py`: 従来型かな漢字変換エンジン [mozc](https://github.com/google/mozc)
  （[mozcpy](https://pypi.org/project/mozcpy/) 経由、`uv sync` でインストール済み）によるベースライン予測を生成します。
  ニューラルモデルのスコアが絶対的にどの水準かを判断する基準として利用してください
  （参考値は `JWTD_v2/v2/reference_scores/` を参照。mozcはcontext_textを使わない単発変換で、
  legacy_v1で49.5%・dev/testで32〜34%、特に `number-notation`・`proper-noun` タグが弱い）

  ```bash
  uv run python scripts/run_mozc_baseline.py --items JWTD_v2/v2/dev.json --out-predictions mozc_preds.json
  uv run python scripts/run_eval.py --items JWTD_v2/v2/dev.json --predictions mozc_preds.json --normalize-width
  ```
