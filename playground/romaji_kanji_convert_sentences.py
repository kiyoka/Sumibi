# -*- coding: utf-8 -*-
import os
import io
import sys
import openai

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
openai.api_key = os.getenv("OPENAI_API_KEY")


def romaji_to_kanji(romaji):
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content": "あなたはローマ字を仮名と漢字からなる日本語に変換するアシスタントです。[は「で、 ]は」で表記してください。"},
            {"role": "user", "content": 'convert the following romaji : "{0}"'.format(romaji)}
        ]
    )
    return(response['choices'][0]['message']['content'])


romaji_list = [
    'watashi ha nihongo wo syaberu kotoga dekimasu .',
    '1nen ha 1gatu3ka kara hajimarimasu .',
    'kagikakko ha [ to ] de kakomimasu . ',
    'ringo ga 1ko to mikan ga 3ko arimasu . ',
    'koyuumeishi ha tokui deha arimasen . tanaka san to satou san ha yuumei nanode daijyoubu desu . ',
    'AWS Systems Manager no kanritaisyou insutansu nisuru',
    'honkijiha , EC2 insutansu wo Systems Manager no Kanritaisyou insutansu ni surumadeno tejyun desu . ',
    'AWS komyunithi- AMI de teikyou sareteiru Windows Server 2019 no AMI niha, hajimekara SSM Agent ga insuto-ru sareteimasu .'
]


for romaji in romaji_list:
    result = romaji_to_kanji(romaji)
    print('IN : {0}'.format(romaji))
    print('OUT: {0}'.format(result))
    print()
