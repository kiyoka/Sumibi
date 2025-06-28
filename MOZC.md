# LLMが使えない環境でのMozcの利用

OpenAIやGoogleの提供するリモートAPIと比べて変換精度は落ちますが、セキュリティ上APIが利用できない場合の代替手段として利用可能です。
利用するには、mozc_serverとmozcのヘルパーアプリをインストールしておく必要があります。

## 動作確認済みOS

- Ubuntu 24.x

## セットアップ手順

mozc関連のパッケージのインストールを行います。

```
sudo apt update
sudo apt upgrade
sudo apt install mozc-server emacs-mozc emacs-mozc-bin
```

## Emacsの設定

1. カスタマイズ変数 `sumibi-backend` を `mozc`に変更してください。

この設定によりSumibiからOpenAIやGoogleに接続することはなくなり、mozc_emacs_helperを経由してmozc_serverに接続します。
※ mozc_serverは必要になった時点で自動起動するため、サーバーの起動設定は不要です。

2. まだ設定していない場合は、`~/.emacs.d/init.el`にSumibiの基本設定を追記してください。

```lisp
(require 'sumibi)
(global-sumibi-mode 1)
```

## トラブルシューティング

- Emacsを起動しモードラインが以下のようになれば成功です。

```
   Sumibi[mozc_server|mozc]
```

- Ctrl+Jを押したとき、以下のエラーが表示された場合は失敗です。

mozc_emacs_helperがインストールされていないか、起動に失敗しています。

```
mozc.el: Failed to start mozc-helper-process.
```

## mozcの個人設定について

MozcはUIツールで設定変更できます。
変換結果が意図通りでないと感じたら、`Clear all history`などを実行して履歴をリセットしてください。

```
/usr/lib/mozc/mozc_tool --mode=config_dialog
```