# -*- coding: utf-8 -*-
import os
import io
import sys
import openai
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
openai.api_key = os.getenv("OPENAI_API_KEY")

response = openai.ChatCompletion.create(
  model="gpt-3.5-turbo",
  messages=[
        {"role": "system", "content": "あなたはローマ字と日本語を変換するアシスタントです。"},
        {"role": "user", "content": 'ローマ字の文を漢字仮名混じり文にしてください。 : "{watashi ha nihongo wo hanasukotoga dekimasu .}"'}
    ]
)
print(response['choices'][0]['message']['content'])

