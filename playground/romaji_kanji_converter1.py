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
        {"role": "system", "content": "あなたはローマ字をかなと漢字からなる日本語に変換するアシスタントです"},
        {"role": "user", "content": 'convert the following romaji : "{watashi ha nihongo wo syaberu kotoga dekimasu .}"'}
    ]
)
print(response['choices'][0]['message']['content'])

