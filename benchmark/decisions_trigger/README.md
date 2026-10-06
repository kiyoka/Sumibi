# Issue #191: Decisions API / Jev 比較ベンチマーク

## 目的と現在の状況

キー入力ごとに「いま日本語変換を開始するか」をOpenAI Decisions APIとJevに判定させ、品質・応答時間・費用を比較します。漢字変換品質の評価やEmacsへのDecisions API搭載は対象外です。

**全件のDecisions実測と保存済みJev結果との比較が完了しました。今回の条件ではJev継続が妥当です。** 閾値0.8で好ましい位置の検出はDecisions 12/28、Jev 25/28。英語685打鍵では両者とも発動0件でした。詳しくは [結論サマリー](SUMMARY.md)、[グラフ付き比較資料](report/REPORT.md)、[全件実測](decisions_all.json) を参照してください。同時期のJev再測定や未見データ等は未実施で、Issue全体は未完了です。

先行した [19打鍵のスモーク結果](decisions_smoke.json) も保持しています。全件測定はタイムアウト1件を再送した1208試行で、費用概算は判明分$0.044551です。キーはキーチェーンから取得し、結果ファイルには保存していません。

既存の [Jevベンチマーク](../jev_trigger/README.md) の自然文47ケース・1239打鍵を変更せず再利用します。shell/gpgの32打鍵は送信せず、各APIへの全件呼び出し数は1207回です。日本語の空白・助詞・句読点、英語15例（長文含む）、コード・数値・混在・修正を含みます。

「好ましい位置」「許容できる早い位置」「待つべき位置」を区別します。`arigatou␣gozaimasu␣` は最初の空白も許容し、最後の空白を好ましい位置として扱います。半角 `!` は現行採用見送りですが、比較のためラベルを保持します。

## 公式仕様（確認時点）

- [Decisions APIガイド](https://developers.openai.com/api/docs/guides/decisions)：公開ベータ、モデル `gpt-6-luna`、`POST https://api.openai.com/v1/decisions`。
- [APIリファレンス](https://developers.openai.com/api/reference/resources/decisions/methods/create)：`input` に入力状態のJSONを文字列で渡し、`questions` の `predicate`、名前 `convert_now` で推定確率を取得します。`answers` 配列の名前を照合し、`probability` を読みます。
- 基本料金は入力100万トークン当たり$0.10。キャッシュ読み書き・出力トークン課金なし。地域処理・長文の追加料金は別で、実行前に最新の公式料金を再確認してください。
- Jevは既存の `noul` 質問文v2を使用。Decisionsには同じ質問文とtrue/false基準を `instructions` にまとめます。入力証拠は両者で同一、APIの形式だけが異なります。将来の文字・正解ラベルは送信しません。

Jevの単価$0.042/100万入力トークンは既存調査の参考値です。同時期の再測定前に請求条件を確認してください。モデル・トークン化・評価値の校正は異なり、同じ0.8が同じ精度を保証するわけではありません。

## 実行場所・APIキー

この作業のルートは次です。以前のJev実装worktreeとは別です。

```sh
cd /Users/kiyoka/.codex/worktrees/issue-191-decisions-benchmark/Sumibi
```

zshで最初に1回だけ設定します。以降、同じシェルでは再入力不要です。キーはファイル・キーチェーンには保存しません。

既にSumibiのmacOSキーチェーンにOpenAIキーがある場合、環境変数の設定は不要です。測定コマンドに `--key-source keychain` を付けてください。ホスト `api.openai.com`、アカウント `apikey` のインターネットパスワードだけを読み、Pythonプロセス内で使用します。キーを表示・保存・環境変数へエクスポートしません。macOSのアクセス許可が必要な場合があります。

```sh
python3 benchmark/decisions_trigger/run.py --key-source keychain --case-id greeting_thanks --dataset dev --max-calls 19 --output benchmark/decisions_trigger/decisions_smoke.json
```

```sh
read -rs "OPENAI_API_KEY?OpenAI API key: "; echo
export OPENAI_API_KEY
```

Jevを再測定する場合のみ、同様に設定します。

```sh
read -rs "TYPESAFE_API_KEY?TypeSafe API key: "; echo
export TYPESAFE_API_KEY
```

Decisions側は `OPENAI_API_KEY` のみ使用します。既存のSumibiキーを使うなら、OpenAI用のキーであることを確認した上で `export OPENAI_API_KEY="$SUMIBI_AI_API_KEY"` が可能です。他社用のキーを流用しないでください。
`SUMIBI_AI_BASEURL` は自動流用しません。接続先を固定して他社ホストにOpenAIキーを誤送信しないようにしています。既存Chat Completions用の呼び出し処理だけでは使えず、専用パスと形式が必要です。アカウント権限・プロキシ互換性・レート制限はスモーク実行で確認します。

## 費用確認と測定

```sh
python3 benchmark/decisions_trigger/run.py --dry-run
```

1207回×仮定600トークン/回ならDecisionsは約$0.07242、Jev参考単価では約$0.03042です。これはプラン用の仮定で、実トークン数や上限額ではありません。漢字変換費用は別です。まず挨拶19打鍵でモデルへのアクセス・実使用量を確認します。

```sh
python3 benchmark/decisions_trigger/run.py --case-id greeting_thanks --dataset dev --max-calls 19 --output benchmark/decisions_trigger/decisions_smoke.json
```

成功を確認して全件へ進みます。標準ライブラリのみで測定できます。`>` によるリダイレクトは不要で、各打鍵の終了時にJSONをアトミックに保存します。

```sh
python3 benchmark/decisions_trigger/run.py --max-calls 1207 --output benchmark/decisions_trigger/decisions_all.json
```

上限で中断した場合、同じファイルに `--resume` で未実行の続きだけを送信します。`--max-calls` はそのコマンドでの追加試行回数上限です。既存ファイルは `--resume` なしでは上書きしません。データ・質問文・設定が変わったファイルへの再開も拒否します。

```sh
python3 benchmark/decisions_trigger/run.py --max-calls 1207 --resume --output benchmark/decisions_trigger/decisions_all.json
```

失敗は0点として扱わず、`score=null` とエラー種別を保存して停止します。失敗時も1試行としてカウントし、自動再試行しません。通常の再開は失敗行を再送しません。原因を確認した上で `--resume --retry-errors` を明示すると、失敗行と未計測行だけを実行します。各試行の履歴・料金・通信失敗を保持し、追加試行も `--max-calls` に含めます。未解消の失敗行を含むファイルは比較グラフに使用できません。使用量不明の試行があれば、費用は判明分だけの概算で、請求総額ではありません。レスポンス本文・キー・生のエラー文字列は保存しません。ただし結果JSONには公開テスト例文の入力本文が含まれます。

## 比較資料とグラフ

Matplotlibが必要です。保存済みJev結果との比較（日時・環境差を含む参考比較）：

```sh
python3 benchmark/decisions_trigger/compare.py benchmark/decisions_trigger/decisions_all.json benchmark/jev_trigger/all_scores.json --output-dir benchmark/decisions_trigger/report
```

同時期のJev再測定を行う場合：

```sh
python3 benchmark/decisions_trigger/run.py --backend jev --max-calls 1207 --output benchmark/decisions_trigger/jev_all.json
python3 benchmark/decisions_trigger/compare.py benchmark/decisions_trigger/decisions_all.json benchmark/decisions_trigger/jev_all.json --output-dir benchmark/decisions_trigger/report_fresh
```

出力は `REPORT.md`、品質・応答時間・費用のグラフ、1文章1枚のPNG/SVG、`comparison.json` です。shell/gpgは未送信と明記し、架空の0点グラフは作りません。データ・ラベル・全打鍵・質問文v2の一致を検証し、欠落や失敗があれば拒否します。過去のJevには打鍵別遅延やp99がないため補間しません。

共通0.8での比較に加え、devだけで閾値を選択し、追加・英語・句読点側で検証します。選択方針は「待つべき発動最小→好ましい検出最大→許容精度最大」です。既存データはJevの調整にも使われているため、真に未見のデータではありません。

## 未完了の評価

- コメント・文字列内 `plain` / コード本体 `code` の専用追加例文（旧データ・過去結果は変更しない）。
- 真に未見の日本語・英語データと複数回の繰り返し計測。
- 実Emacs相当の単一通信・最新入力のみ待機・古い判定の破棄、間引き・デバウンスの評価。現在のスクリプトは全打鍵を独立に逐次判定します。
- 実測後の採用／見送り／追加調査の結論。

## オフラインテスト

```sh
python3 -m unittest discover -s benchmark/decisions_trigger -p 'test_*.py'
python3 -m unittest discover -s benchmark/jev_trigger -p 'test_*.py'
```

API認証・費用の発生を伴う試験は含みません。APIキー入力はチャットには貼り付けないでください。
