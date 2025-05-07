# -*- coding: utf-8 -*-
import os
import io
import sys
from openai import OpenAI

# テストデータ

#surrounding_text  = 'ここは海から近い市場です。イカがたくさん並んでいます。ikano toori desu.'
#henkan_text = 'ikano toori desu.'

#surrounding_text  = '説明は詳細です。実際の例は、ikano toori desu.'
#henkan_text = 'ikano toori desu.'

surrounding_text  = '会社ではTruly Ergonomic CLEAVE keyboardというキーボードを使っています。Truly Ergonomic CLEAVE keyboard ha nyuusyu konnnann desu . '
henkan_text = 'Truly Ergonomic CLEAVE keyboard ha nyuusyu konnnann desu .'

# OpenAIリクエスト処理
api_key = os.getenv("SUMIBI_AI_API_KEY") or os.getenv("OPENAI_API_KEY")
base_url_env = os.getenv("SUMIBI_AI_BASEURL") or "https://api.openai.com"
base_url = base_url_env.rstrip("/") + "/v1"
client = OpenAI(api_key=api_key, base_url=base_url)
response = client.chat.completions.create(model=os.getenv("SUMIBI_AI_MODEL") or "gpt-4.1",
temperature=0.8,
n=1,
messages=[
    {"role": "system", "content": "あなたはローマ字とひらがなを日本語に変換するアシスタントです。"
     "ローマ字の 「nn」 は 「ん」と読んでください。"
     "[](URL)のようなmarkdown構文は維持してください。"
     "# や ## や ### や #### のようなmarkdown構文は維持してください。"},
    {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
     " 周辺の文章は、「こんにちは、中野です。watashi no namae ha nakano desu . どうぞよろしくお願いします。」"
     "のような文章になっています。"
     "周辺の文脈を見てそれに合った語彙を選んでください。: watashi no namae ha nakano desu ."},
    {"role": "assistant", "content": "私の名前は中野です。"},
    {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
     "周辺の文章は、「説明はここまでです。それ以外は ikano toori desu .」"
     "のような文章になっています。"
     "周辺の文脈を見てそれに合った語彙を選んでください。: ikano toori desu ."},
    {"role": "assistant", "content": "以下の通りです。"},
    {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
     "周辺の文章は、「{0}」"
     "のような文章になっています。"
     "周辺の文脈を見てそれに合った語彙を選んでください。: {1}".format( surrounding_text, henkan_text )}
])
print(response.choices[0].message.content)
