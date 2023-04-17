# -*- coding: utf-8 -*-
import os
import io
import sys
import openai

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
openai.api_key = os.getenv("OPENAI_API_KEY")

def kanji_to_yomigana(kanji):
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content":
             "あなたは漢字が与えられると、ひらがなに変換するアシスタントです。"
             },
            {"role": "user", "content": '次をひらがなのみで表記してください。 : "{0}"'.format(kanji)}
        ]
    )
    return(response['choices'][0]['message']['content'])
    
    
def romaji_to_kanji(romaji):
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content":
             "あなたはローマ字と日本語を変換するアシスタントです。"
             },
            {"role": "user", "content": 'ローマ字の文を漢字仮名混じり文にしてください。 : "{0}"'.format(romaji)}
        ]
    )
    return(response['choices'][0]['message']['content'])

romaji_list = [
    'watashi no namae ha nakano desu .',
    'watashinonamaehanakanodesu .',
    '1nen ha 1gatu3ka kara hajimarimasu .',
    'kagikakko ha [ to ] de kakomimasu . ',
    'ringo ga 1ko to mikan ga 3ko arimasu . ',
    'koyuu meishi ha tokui deha arimasen . tanaka san to satou san ha yuumei nanode daijyoubu desu . ',
    'AWS Systems Manager no kanritaisyou insutansu nisuru',
    'honkijiha , EC2 insutansu wo Systems Manager no Kanritaisyou insutansu ni surumadeno tejyun desu . ',
    'AWS komyunithi- AMI de teikyou sareteiru Windows Server 2019 no AMI niha, hajimekara SSM Agent ga insuto-ru sareteimasu .',
    'watashi ha nihongo wo syaberu kotoga dekimasu .',
    ]

kanji_list = [
    '漢字',
    '東西南北',
    '行う',
    '東京特許許可局'
]

if True:
    for romaji in romaji_list:
        print('IN  : {0}'.format(romaji))
        print('OUT1: {0}'.format(romaji_to_kanji(romaji)))
        print('OUT2: {0}'.format(romaji_to_kanji(romaji)))
        print()

if True:
    for kanji in kanji_list:
        print('IN  : {0}'.format(kanji))
        print('OUT1: {0}'.format(kanji_to_yomigana(kanji)))
        print('OUT2: {0}'.format(kanji_to_yomigana(kanji)))
        print()
