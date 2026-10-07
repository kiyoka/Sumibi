# Decisions API / Jev 比較結果

各打鍵は独立に評価。途中で実際の変換は行いません。評価値の校正はAPI間で同一とは仮定しません。

過去Jevとの比較は計測日時・環境差を含みます。遅延のp99は古い結果には保存されていないため補間しません。

![品質](quality.png)

![速度と費用](latency_cost.png)

![閾値比較](thresholds.png)

| API | 好ましい検出/総数 | 許容早期 | 待つべき発動 | 英語発動 | dev選択閾値 |
|---|---:|---:|---:|---:|---:|
| decisions | 12/28 | 9 | 16 | 0 | 0.9 |
| jev | 25/28 | 8 | 10 | 0 | 0.9 |

閾値選択はdevのみで、待つべき位置の発動を最小化し、同数なら好ましい位置の検出数を最大化します。追加・英語・句読点セットを評価側に使用します。既存データはJevプロンプト調整にも使われたため、完全な未見評価ではありません。

| API | 評価側の好ましい検出/総数 | 評価側の待つべき発動 |
|---|---:|---:|
| decisions | 1/21 | 4 |
| jev | 14/21 | 0 |

| API | p50 (ms) | p95 (ms) | p99 (ms) | 1000打鍵概算 (USD) |
|---|---:|---:|---:|---:|
| decisions | 301.0 | 435.5 | 694.5 | 0.035957 |
| jev | 229.4 | 359.0 | 未保存 | 0.020933 |

## 費用と計測情報

- decisions: モデル ['gpt-6-luna']、開始 2026-10-06T22:52:16.124546+00:00、1208回、445510入力トークン、概算$0.044551。
  1日10000打鍵・月30日という仮定：日$0.3596 / 月$10.7871（漢字変換API料金は別）。

  途中の失敗試行：{'network_or_timeout': 1}。再試行後の評価値で品質を集計し、遅延は失敗試行も含めます。

  使用量不明の試行が1件あります。料金は判明分だけで、請求総額ではありません。

- jev: モデル ['jev-1.13.0']、開始 historical: see Jev VISUAL_REPORT、1207回、617520入力トークン、概算$0.025936。
  1日10000打鍵・月30日という仮定：日$0.2093 / 月$6.2799（漢字変換API料金は別）。

## 判断

数値でいうと、今回の共通閾値0.8ではJevが優位です。一方、SumibiですでにOpenAIのアクセスキーを利用する運用なら、別サービスの契約・キー管理を増やさずに使えるDecisions APIも総合評価では有力な選択肢です。グラフの共通した挙動を踏まえ、閾値・発動条件の調整と実操作での検証によって代替可能性を評価します。

この集計だけでは採用を確定しません。新しい未見例文、繰り返し測定、コメント・文字列の追加データ、連続入力での間引き・古い応答破棄を含む実Emacs相当の評価が必要です。

## 全例文：1例文1グラフ

### 折れ線グラフの読み方

折れ線は各APIの変換評価値です。黄色・緑の帯は、テストデータにあらかじめ付けた期待する変換タイミングを示し、APIの評価値や実際に変換が起きた位置ではありません。

- 緑の帯：変換するのが好ましい打鍵位置（`trigger_after`）。
- 黄色の帯：変換してもよいが、もう少し待つ方が好ましい打鍵位置（`acceptable_after`）。
- 帯なし：今回のラベルでは、まだ変換せず待ってほしい打鍵位置。

例えば `arigatou␣gozaimasu␣` では、最初の空白が黄色、最後の空白が緑です。`␣` は空白を表します。帯は評価の基準であり、普遍的な唯一の正解を意味しません。

### greeting_thanks

![greeting_thanks](greeting_thanks.png)

[SVG](greeting_thanks.svg)

### greeting_morning

![greeting_morning](greeting_morning.png)

[SVG](greeting_morning.svg)

### question_are_you_ok

![question_are_you_ok](question_are_you_ok.png)

[SVG](question_are_you_ok.svg)

### weather_sunny

![weather_sunny](weather_sunny.png)

[SVG](weather_sunny.svg)

### read_book

![read_book](read_book.png)

[SVG](read_book.svg)

### acknowledge

![acknowledge](acknowledge.png)

[SVG](acknowledge.svg)

### return_at_three

![return_at_three](return_at_three.png)

[SVG](return_at_three.svg)

### english_iowa

![english_iowa](english_iowa.png)

[SVG](english_iowa.svg)

### decimal_pi

![decimal_pi](decimal_pi.png)

[SVG](decimal_pi.svg)

### shell_status

ローカル除外・API未送信。モデル評価値のグラフは作りません。

### private_note

ローカル除外・API未送信。モデル評価値のグラフは作りません。

### english_test

![english_test](english_test.png)

[SVG](english_test.svg)

### code_number

![code_number](code_number.png)

[SVG](code_number.svg)

### weather_rain

![weather_rain](weather_rain.png)

[SVG](weather_rain.svg)

### use_pen

![use_pen](use_pen.png)

[SVG](use_pen.svg)

### what_time

![what_time](what_time.png)

[SVG](what_time.svg)

### ask_repeat

![ask_repeat](ask_repeat.png)

[SVG](ask_repeat.svg)

### greeting_regards

![greeting_regards](greeting_regards.png)

[SVG](greeting_regards.svg)

### thanks_past

![thanks_past](thanks_past.png)

[SVG](thanks_past.svg)

### meeting_today

![meeting_today](meeting_today.png)

[SVG](meeting_today.svg)

### english_data

![english_data](english_data.png)

[SVG](english_data.svg)

### code_filename

![code_filename](code_filename.png)

[SVG](code_filename.svg)

### decimal_number

![decimal_number](decimal_number.png)

[SVG](decimal_number.svg)

### english_iowa_sentence

![english_iowa_sentence](english_iowa_sentence.png)

[SVG](english_iowa_sentence.svg)

### english_no_to

![english_no_to](english_no_to.png)

[SVG](english_no_to.svg)

### english_data_ready

![english_data_ready](english_data_ready.png)

[SVG](english_data_ready.svg)

### english_question

![english_question](english_question.png)

[SVG](english_question.svg)

### english_two_sentences

![english_two_sentences](english_two_sentences.png)

[SVG](english_two_sentences.svg)

### english_comma_pause

![english_comma_pause](english_comma_pause.png)

[SVG](english_comma_pause.svg)

### english_decimal

![english_decimal](english_decimal.png)

[SVG](english_decimal.svg)

### english_contraction

![english_contraction](english_contraction.png)

[SVG](english_contraction.svg)

### english_technical

![english_technical](english_technical.png)

[SVG](english_technical.svg)

### english_greeting

![english_greeting](english_greeting.png)

[SVG](english_greeting.svg)

### english_long_report

![english_long_report](english_long_report.png)

[SVG](english_long_report.svg)

### english_long_review

![english_long_review](english_long_review.png)

[SVG](english_long_review.svg)

### english_long_work

![english_long_work](english_long_work.png)

[SVG](english_long_work.svg)

### english_long_software

![english_long_software](english_long_software.png)

[SVG](english_long_software.svg)

### english_long_travel

![english_long_travel](english_long_travel.png)

[SVG](english_long_travel.svg)

### jp_period

![jp_period](jp_period.png)

[SVG](jp_period.svg)

### jp_comma

![jp_comma](jp_comma.png)

[SVG](jp_comma.svg)

### jp_exclamation

![jp_exclamation](jp_exclamation.png)

[SVG](jp_exclamation.svg)

### jp_question

![jp_question](jp_question.png)

[SVG](jp_question.svg)

### jp_fullwidth_period

![jp_fullwidth_period](jp_fullwidth_period.png)

[SVG](jp_fullwidth_period.svg)

### jp_fullwidth_comma

![jp_fullwidth_comma](jp_fullwidth_comma.png)

[SVG](jp_fullwidth_comma.svg)

### jp_fullwidth_exclamation

![jp_fullwidth_exclamation](jp_fullwidth_exclamation.png)

[SVG](jp_fullwidth_exclamation.svg)

### jp_fullwidth_question

![jp_fullwidth_question](jp_fullwidth_question.png)

[SVG](jp_fullwidth_question.svg)

### jp_punctuation_sequence

![jp_punctuation_sequence](jp_punctuation_sequence.png)

[SVG](jp_punctuation_sequence.svg)
