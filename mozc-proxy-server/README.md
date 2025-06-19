## mozc-proxy-server: mozc の HTTP/JSON プロキシ実装

### 主な構成

1. `mozc-proxy-server/ipc.py`
   - MozcClient を実装。
   - Mozc の protobuf が見つからない／mozc_server のソケットが見つからない場合は自動で “ダミー（変換なし）” モードにフォールバックし、その旨を WARNING 出力。
   - CREATE_SESSION／SUBMIT のみを実装した最小 IPC クライアント（長さプレフィクス付き protobuf 送受信）。
2. `mozc-proxy-server/server.py`
   - 追加ライブラリ不要（標準ライブラリだけ）で動く HTTP サーバ。
   - エンドポイント: POST /convert  
     - 入力  : {"text": "<ローマ字>"}
     - 出力  : {"text": "<漢字仮名交じり文>"}
   - MozcClient を再利用しつつ、例外発生時は 5xx / JSON で返却。
   - `python3 mozc-proxy-server/server.py --listen 0.0.0.0 --port 8000` で起動。
3. `mozc-proxy-server/__init__.py`
   - パッケージ初期化・エクスポート。

### クライアントからの疎通確認方法

```
curl -X POST http://127.0.0.1:8000/convert -H 'Content-Type: application/json' -d '{"text":"ohayou"}'
```

- 結果

Mozc が有効なら変換、無ければ原文

```
{"text": "おはよう"}
```
