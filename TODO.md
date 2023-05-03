# TODO

日本語入力システムとして使いやすくなりそうな実装要望リスト。

## sumibi.elのTODO

* [実装済み] regionを指定した状態で Ctrl-J を入力すると、ひらがな/カタカナの読みを返す。
* [実装済み] regionを指定した状態で Ctrl-J を入力すると、ひらがな/カタカナ/同一義語を複数返す。
* [実装済み] OpenAI API 呼び出しのタイムアウト処理を実装する。

* [実装済み]次の短いキーワード(送り仮名など)が変換できない。

```
nano
shita
```

* [実装済み] (sumibigpt-string-include-kanji "要約") が nil となる。

恐らく、漢字かどうかを判断の方法が間違っている。
```
(string-match "[亜-瑤]" "要") => nil
(string-match "[亜-瑤]" "約") => nil
```

* ローマ字や英語の文章の中に含まれるmarkdown構文が壊れないように保護する。

例えば、以下のようなリンク構文、画像挿入構文が意識されるようにする。

```
[link](https://hostname.aaa.bbb/abc/)
![image](https://hostname.aaa.bbb/abc/image.png)
```
