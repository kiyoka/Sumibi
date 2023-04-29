# FAQよくある質問

## Q. SumibiGPTの特徴は？

A. モードレスの日本語変換システムです。

WindowsやmacOSの標準の日本語入力システムは、日本語入力モードのオンとオフを切り替える必要がありますが、
SumibiGPTはどのような場面であっても、Ctrl-Jキーを押すと直前のローマ字の文章を日本語に変換します。

![image.png](./images/img_15.png)
![image.png](./images/img_16.png)

## Q. どのOSで利用できますか？

A. Windows/Linux/macOSのどれでも動作します。

## Q. 対応しているEmacsバージョンは？

A. Emacs 28.x 以上に対応しています。

## Q. Emacsの他に必要なソフトウェアはありますか？

A. いいえ、必要ありません。

## Q. OpenAIのアカウントは必要ですか？

A. はい、必要です。OpenAI APIの契約をして、APIキーを取得してください。

## Q. SumibiGPTを使うとOpen AIの費用はどれくらいかかりますか？

A. gpt-3.5-turboを使った場合、1ドキュメントを書くのに約1円から5円程度です。
SumibiGPTの設定をGPT-4に変更した場合は、大体10倍の費用がかかります。

## Q. 期待した漢字が出ません。うまく変換するやり方はありますか？

A. なるべく長い文書を入力してから変換するのがコツです。

また、思った漢字が出なかった場合は、間違った漢字の部分を範囲選択した上でCtrl-J気を押すと、
同音異議語が複数出力されます。

![image.png](./images/douon-igigo.gif)

## Q. 漢字よりもひらがなやカタカナで書きたい部分があります。どうすれば良いですか？

単語の部分を範囲選択した上でCtrl-Jキーを押すと、ひらがなとカタカナが出力されます。

![image.png](./images/prefer-to-katakana.gif)

## Q. 変換精度を上げる方法はありますか？

gpt-3.5-turboはまだまだ思ったような漢字が出せませんが、gpt-4に切り替えるとかなり変換精度が上がります。
gpt-3.5-turboしか使えない場合は、なるべく長い文章を作って変換してください。

## Q. GPTを使っているなら、英語から日本語への翻訳もできそうです。どのようにしたら翻訳できますか？

A. ローマ字の文章の代わりに、英語の文章の最後でCtrl-Jを入力すると日本語の文章へ変換してくれます。

## Q. GPTを使っているなら、長文の要約もできそうです。どのようにすれば要約ができますか？

A. SumibiGPT 1.1 では、まだ長文の要約をサポートしていません。将来バージョンでサポート予定です。