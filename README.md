# SumibiGPT

Japanese input method using OpenAI GPT

## SumibiGPTとは

Emacs用の日本語入力システム(IME)です。
SumibiGPTはモードレスです。
日本語入力モードに切り替えることなく日本語を入力できます。

## 利用可能なEmacsバージョン

Emacs version 28.x (Windows/Linux/macOS)
Emacs以外の追加ソフトウェアは不要です。

## Emacsクライアントのインストール

1. OpenAI AIのサブスクリプションを契約します。

SumibiGPTを使ったOpenAI API使用料の目安としては1ドキュメントを書くのに約5円から10円程度です。

2. 環境変数 OPENAI_API_KEY にOpenAPIのAPIキーを登録します。

3. melpa から以下のパッケージをインストールします。

popup 0.5.9
unicode-escapeo 20230109.1222

4. sumibigpt.el を ~/.emacs.d/ に保存します。

5. ~/.emacs.d/init.el に以下のコードを追加します。

```emacs lisp
(require 'sumibigpt)
(global-sumibigpt-mode 1)
```

## インストールが成功したかどうかの確認方法

1. Emacsを再起動するとSumibiGPTがステータスバーに表示されます。

[gpt-3.5-turbo] は使用しているGPTのモデルです。

## 変換方法

1. ローマ字で書いた文章の最後にカーソルを合わせて、Ctrl-J を入力すると日本語の文章に置き換わります。

2. 変換結果が気に入らない場合は、そのまま Ctrl-J を入力すると変換候補のポップアップが表示されるので、その中から選択できます。

## Undo

変換結果が気に入らない時は、ESC-u キーを入力することでUndoできます。

または、変換結果に原文ままの選択肢がありますので「原文まま」を選択します。

## 英語から日本語への変換

英語の文章の最後にカーソルを合わせて、Ctrl-J を入力すると日本語の文章に翻訳されます。
