# SumibiGPT

Japanese input method using OpenAI GPT

## SumibiGPTとは

Emacs用の日本語入力システム(IME)です。
SumibiGPTはモードレスの日本語入力システムであり、日本語と英語のモードを切り替える操作が不要なシステムです。

## インストール方法

1. OpenAI AIのサービスクリプションを契約します。

2. 環境変数OPENAI_API_KEYにOpenAPIのキーを登録します。

3. sumibigpt.el と unicode-escape.el を ~/.emacs.d/ に保存します。

4. ~/.emacs.d/init.el に以下のコードを追加します。

(require 'sumibigpt)
(global-sumibigpt-mode 1)

## インストールが成功したかどうかの確認

1. Emacsを再起動するとSumibiGPTが有効になります。

## 変換方法

1. ローマ字で書いた文章の最後にカーソルを合わせて、Ctrl-Jを入力すると日本語の文章に置き換わります。

2. 変換結果が気に入らない場合は、そのまま Ctrl-J を押すと他の変換候補から選択できます。

## Undo

変換結果が気に入らない時は、ESC-u キーを入力することでUndoできます。

## 英語から日本語への変換

英語の文章の最後にカーソルを合わせて、Ctrl-Jを入力すると日本語の文章に翻訳されます。

