# Sumibi

Japanese input method powered by ChatGPT API

![image.png](./images/sumibi_image.jpg)

## Sumibiとは

Emacs用の日本語入力システム(IME)です。

Sumibiはモードレスです。
日本語入力モードに切り替えることなく日本語を入力できます。

日本語と英語の相互翻訳もサポートしていますので、英語で文章を書くことが多い人にもおすすめです。

よくある質問はこちら。[FAQ](FAQ.md)

## 利用可能なEmacsバージョン

Emacs version 28.x (Windows/Linux/macOS) で動作します。Emacs以外の追加ソフトウェアは不要です。

## Emacsクライアントのインストール

1. OpenAI AIのサブスクリプションを契約します。

[https://platform.openai.com/account/api-keys](https://platform.openai.com/account/api-keys)
![image.png](./images/img_8.png)

2. 環境変数 `SUMIBI_AI_API_KEY` にOpenAI APIキーを登録します。`SUMIBI_AI_API_KEY` が未定義の場合は、環境変数 `OPENAI_API_KEY` の値を使用します。
3. 任意で環境変数 `SUMIBI_AI_API_HOST` にAPIエンドポイントのホストを指定できます。定義されていない場合は `api.openai.com` を使用します。
3. 任意で環境変数 `SUMIBI_AI_MODEL` にGPTの利用モデルを指定できます。定義されていない場合はデフォルトで gpt-4.1-mini になります。
4. MELPAからパッケージ「sumibi」をインストールします。
5. \~/.emacs.d/init.el に以下のコードを追加します。

```lisp
(require 'sumibi)
(global-sumibi-mode 1)
```

## インストールが成功したかどうかの確認方法

Emacsを再起動するとSumibiがステータスバーに表示されます。
[gpt-4.1-mini] はOpenAI API呼び出しで使用しているGPTのモデルです。
![image.png](./images/img_9.png)

## ローマ字や英語の文章から日本語への変換

1. ローマ字で書いた文章の最後にカーソルを合わせて、Ctrl-J を入力すると日本語の文章に置き換わります。
    ![image.png](./images/img_15.png)
    ![image.png](./images/img_16.png)
2. 変換結果が気に入らない場合は、そのまま Ctrl-J を入力すると変換候補のポップアップが表示されるので、その中から選択できます。
    ![image.png](./images/img_11.png)
3. 英語の文章の最後にカーソルを合わせて、Ctrl-J を入力すると、日本語の文章に変換されます。
    ![image.png](./images/img_13.png)
    ![image.png](./images/img_14.png)

## 日本語から英語への翻訳

日本語の文章をregion選択した状態で、ESC 、 j を順に入力すると選択範囲が英語に翻訳されます。

「ESC 、j」は、代わりに ALT+j でも入力可能です。

## GPTの利用モデルの切り替え

M-x sumibi-switch-modelでポップアップから利用モデルを動的に変更することができます。

デフォルトでは、"gpt-4.1-mini"と"gpt-4"のなど多数のモデル名を登録しています。
選択肢を増やしたい場合は、カスタマイズ変数 `sumibi-model-list` に候補を追加してください。

## Undo

変換結果が気に入らない時は、ESC-u キーを入力することでUndoできます。

または、変換結果に原文ままの選択肢がありますので「原文まま」を選択します。
![image.png](./images/img_10.png)

## その他

### 文脈情報の取り込み

Sumibiでは、変換対象のローマ字や英文をより自然な日本語に変換するために、対象文字列の前後の文章をAIに送信し、文脈情報として活用します。
取り込む周辺行数は、カスタマイズ可能な変数 `sumibi-surrounding-lines` で設定できます。デフォルトは6で、6はカーソル位置から上向きに3行、下向きに3行の合計6行という意味です。
必要に応じて適切な行数に調整し、最適な文脈量を確保してください。
APIの費用が気になる人は小さい数字に、変換精度を上げたい人は大きい数字にすると良いでしょう。

### 利用するAIサービスの切り替え

AIサービスとしてOpenAI以外にも切り替えることができます。
OpenAIのcompletions APIと互換性のあるサービスであれば、環境変数で切り替えることができます。
ローカルのLLMを利用する場合などに活用してください。

- 向先をDeepSeekに変更する場合

  - 環境変数 `SUMIBI_AI_API_KEY` に deepseekのAPIキーを設定する。
  - 環境変数 `SUMIBI_AI_API_HOST` に api.deepseek.com を設定する。
  - 環境変数 `SUMIBI_AI_MODEL` に deepseek-chatを設定する。

- 上記の設定を、init.elに設定するには以下のようになります。

```
(setenv "SUMIBI_AI_API_KEY" "sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
(setenv "SUMIBI_AI_API_HOST" "api.deepseek.com")
(setenv "SUMIBI_AI_MODEL" "deepseek-chat")))
```
