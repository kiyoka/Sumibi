# -*- coding: utf-8 -*-
import os
import io
import sys
import openai
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
openai.api_key = os.getenv("OPENAI_API_KEY")

response = openai.ChatCompletion.create(
    model="gpt-3.5-turbo",
    temperature=0.8,
    n=1,
    messages=[
        {"role": "system", "content": "あなたはローマ字とひらがなを日本語に変換するアシスタントです。"
         "ローマ字の 「nn」 は 「ん」と読んでください。"
         "[](URL)のようなmarkdown構文は維持してください。"
         "# や ## や ### や #### のようなmarkdown構文は維持してください。"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : watashi no namae ha nakano desu ."},
        {"role": "assistant", "content": "私の名前は中野です。"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : わたしのなまえはなかのです。"},
        {"role": "assistant", "content": "私の名前は中野です。"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : ikano toori desu ."},
        {"role": "assistant", "content": "以下の通りです。"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : hannishitei shimasu"},
        {"role": "assistant", "content": "範囲指定します"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : We succeeded in taking a photo like this:\n![example](https://www.example.com/dir1/dir2/example.png)"},
        {"role": "assistant", "content": "このような写真を撮ることに成功しました：\n![例](https://www.example.com/dir1/dir2/example.png)"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : ## this is markdown section"},
        {"role": "assistant", "content": "## これはMarkdownのセクションです。"},
        {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : {0}".format( 'watashi no namae ha nakano desu .' )}
    ]
)
print(response['choices'][0]['message']['content'])
