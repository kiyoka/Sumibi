# afm_bridge — Apple Foundation Models を OpenAI 互換APIで叩くブリッジ

macOS 27 の [FoundationModels framework](https://developer.apple.com/documentation/foundationmodels)
（オンデバイスの Apple Foundation Model = AFM）を、OpenAI Chat Completions API 互換の
HTTPエンドポイントとして公開する開発用サーバです。

`benchmark/sumibi_bench.py` は OpenAI 互換API前提で作られているため、
`SUMIBI_AI_BASEURL` をこのブリッジに向けるだけで AFM のベンチマークが取得できます (Issue #180)。

## 必要環境

- macOS 27.0 以降（`SystemLanguageModel` の `variant` API が macOS 27 で追加されたため）
- Xcode コマンドラインツール（Swift 6.4 で確認）
- Apple Intelligence が有効で、モデルがダウンロード済みであること

## ビルドと起動

```bash
# benchmark/ から
make afm_bridge          # ビルド
make afm_bridge_run      # ポート8765で起動

# 直接起動する場合
cd afm_bridge
swiftc -target arm64-apple-macos27.0 -swift-version 5 -O main.swift -o afm_bridge
./afm_bridge --port 8765
```

起動時に使用中のモデルvariantを表示します。

```
model variant  : AFM 3 Core Advanced
context size   : 8192
guardrails     : permissiveContentTransformations
prompt mode    : transcript
supports ja_JP : true
listening on http://127.0.0.1:8765
```

## オプション

| オプション | 既定 | 説明 |
| --- | --- | --- |
| `--port N` | 8765 | 待ち受けポート |
| `--permissive-guardrails` | 有効 | `Guardrails.permissiveContentTransformations` を使う。IMEは「生成」ではなく「変換」なのでこちらが適切 |
| `--strict-guardrails` | | `Guardrails.default` を使う |
| `--prompt-mode transcript\|flat` | transcript | few-shot のマッピング方式（下記） |
| `--verbose` | | リクエストごとの所要時間と出力を stderr に出す |

## ベンチマークの実行

```bash
# 別ターミナルでブリッジを起動しておく
export SUMIBI_AI_BASEURL=http://127.0.0.1:8765
export SUMIBI_AI_API_KEY=dummy      # 使われないがopenaiライブラリが要求する
export SUMIBI_AI_MODEL=afm-3-core-advanced

make result_ver2.4.0/afm-3-core-advanced.json            # ローマ字入力
make result_ver2.4.0/afm-3-core-advanced_hiragana.json   # ひらがな入力
make result_ver2.4.0/afm-3-core-advanced_katakana.json   # カタカナ入力
```

## 実装メモ

### モデルの選択

macOS 27 の `SystemLanguageModel.Variant` には `core3` と `coreAdvanced3` があります。
**variant を明示的に指定する公開APIは無く**、`model.variant` は読み取り専用です。
どちらが使われるかはOSがハードウェアに応じて決めます。
M4 / 24GB の MacBook Air では `SystemLanguageModel.default` が
`AFM 3 Core Advanced` になることを確認済みです（起動時のログで実際のvariantを確認してください）。

### messages のマッピング

OpenAI の `messages` を FoundationModels にマップする方式が2つあります。
計測結果に有意差は無かった（20件サブセットで CER 93.0% vs 94.4%）ため、
既定は素直な `transcript` です。

- `transcript`(既定): `system` → `Transcript.Instructions`、
  末尾以外の `user`/`assistant` → `Transcript.Prompt` / `Transcript.Response`、
  末尾の `user` → 実際に応答させるプロンプト
- `flat`: `system` → `Transcript.Instructions`、
  few-shot は `入力: ... / 出力: ...` の形で1つのプロンプト文字列に連結

### エラー時の挙動

`sumibi_bench.py` は `APITimeoutError` 以外の例外を捕捉せず実行全体が落ちるため、
このブリッジは**エラーでもHTTP 200を返し**、本文にセンチネル文字列を入れます。
結果JSONにそのまま残るので、後から件数を数えられます。

| センチネル | 対応する `LanguageModelError` |
| --- | --- |
| `[GUARDRAIL]` | `guardrailViolation` |
| `[REFUSAL]` | `refusal` |
| `[CONTEXT_EXCEEDED]` | `contextSizeExceeded` |
| `[ERROR]` | その他 |

Ctrl-C で終了すると、種別ごとの件数を stderr に出力します。

### 未対応

ベンチマークに不要なため実装していません。

- ストリーミング (`stream: true`)
- `n > 1`（常に1候補）
- `usage` のトークン数（常に0を返す）
- 認証（ローカル専用。`127.0.0.1` 以外にバインドしません）
