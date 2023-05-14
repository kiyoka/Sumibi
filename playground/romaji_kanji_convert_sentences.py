# -*- coding: utf-8 -*-
import os
import io
import sys
import openai
import time
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
openai.api_key = os.getenv("OPENAI_API_KEY")


class GptTest:
    romaji_list1 = [
        'watashi no namae ha nakano desu .',
        'watashinonamaehanakanodesu .',
        'わたしのなかえはなかのです。',
        '1nen ha 1gatu3ka kara hajimarimasu .',
        '1ねんは1がつ3かからはじまります。',
        'kagikakko ha [ to ] de kakomimasu . ',
        'ringo ga 1ko to mikan ga 3ko arimasu . ', 
        'りんごが1ことみかんが3こあります。',
        'koyuu meishi ha tokui deha arimasen . tanaka san to satou san ha yuumei nanode daijyoubu desu . ',
        'AWS Systems Manager no kanritaisyou insutansu nisuru',
        'honkijiha , EC2 insutansu wo Systems Manager no Kanritaisyou insutansu ni surumadeno tejyun desu . ',
        'AWS komyunithi- AMI de teikyou sareteiru Windows Server 2019 no AMI niha, hajimekara SSM Agent ga insuto-ru sareteimasu .',
        'watashi ha nihongo wo syaberu kotoga dekimasu .',
        'Emacs kuraianto kara riyou dekiruyouni narimashita .',
        'Emacs kara riyou dekiru kanji henkan enjin desu .',
        'Emacsからりようできるかんじへんかんえんじんです。',
        'toriaezu , ugokuyouni narimashita .',
        'ikano toori desu',
        'hannisentaku shimasu',
        'We succeeded in taking a photo like this:\n![example](https://www.example.com/dir1/dir2/example.png)',
        'We achieved [it](https://www.example.com/aaa/bbb/) by a new method.',
        '# this is markdown file',
        '## this is markdown section',
        '### this is markdown subsection',
        '#### this is markdown subsubsection',
        '# this is markdown file\n## this is markdown section\n### this is markdown subsection',
    ]
    
    romaji_list2 = [
        'hannisentaku shimasu',
        'nano',
        'shita',
        'aiueokakikukeko'
    ]

    kanji_list = [
        '漢字',
        '東西南北',
        '行う',
        '東京特許許可局',
        '要約',
        '体制',
        '制度'
    ]

    japanese_list = [
        '私の名前は中野です。',
        'Emacsから利用できる漢字変換エンジンです。',
        'GPTはOpenAIから2018年に以下の論文で提案されたモデルで、基本的にはTransformerをベースに、事前学習-ファインチューニングをすることで非常に高い精度を達成したモデルです。'
    ]
    
    model = "gpt-3.5-turbo"

    def set_model(self,model):
        self.model = model

    def romaji_to_kanji(self,romaji,arg_n):
        response = openai.ChatCompletion.create(
            model=self.model,
            temperature=0.8,
            n=arg_n,
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
                {"role": "user", "content": "ローマ字とひらがなの文を漢字仮名混じり文にしてください。 : {0}".format(romaji)}
            ]
        )
        arr = []
        for i in range(arg_n):
            arr.append(response['choices'][i]['message']['content'])
        return(arr)

    def romaji_to_yomigana(self,romaji,arg_n):
        response = openai.ChatCompletion.create(
            model=self.model,
            temperature=0.8,
            n=arg_n,
            messages=[
                {"role": "system", "content": "あなたはローマ字をひらがなとカタカナに変換するアシスタントです。ローマ字の 「nn」 は 「ん」と読んでください。"},
                {"role": "user", "content": "ローマ字をひらがなとカタカナにしてください : shita"},
                {"role": "assistant", "content": "した シタ"},
                {"role": "user", "content": "ローマ字をひらがなとカタカナにしてください : nano"},
                {"role": "assistant", "content": "なの ナノ"},
                {"role": "user", "content": "ローマ字をひらがなとカタカナにしてください : aiueokakikukeko"},
                {"role": "assistant", "content": "あいうえおかきくけこ アイウエオカキクケコ"},
                {"role": "user", "content": "ローマ字をひらがなとカタカナにしてください : {0}".format(romaji)}
            ]
        )
        arr = []
        for i in range(arg_n):
            arr.append(response['choices'][i]['message']['content'])
        return(arr)

    def kanji_to_yomigana(self,kanji,arg_n):
        response = openai.ChatCompletion.create(
            model=self.model,
            temperature=0.8,
            n=arg_n,
            messages=[
                {"role": "system", "content": "あなたは漢字が与えられると、ひらがなとカタカナとその漢字の同音異義語を返すアシスタントです。"},
                {"role": "user", "content": "ひらがなとカタカナと同音異義語をなるべく多く列挙してください。 : 東西南北"},
                {"role": "assistant", "content": "とうざいなんぼく トウザイナンボク 東西南北"},
                {"role": "user", "content": "ひらがなとカタカナと同音異義語をなるべく多く列挙してください。 : 漢字"},
                {"role": "assistant", "content": "かんじ カンジ 漢字 感じ 幹事 監事 寛二"},
                {"role": "user", "content": "ひらがなとカタカナと同音異義語をなるべく多く列挙してください。 : {0}".format(kanji)}
                ]
            )
        arr = []
        for i in range(arg_n):
            arr.append(response['choices'][i]['message']['content'])
        return(arr)

    def japanese_to_english(self,japanese,arg_n):
        response = openai.ChatCompletion.create(
            model=self.model,
            temperature=0.8,
            n=arg_n,
            messages=[
                {"role": "system", "content": "あなたは、与えられた文章を英語に翻訳するアシスタントです。"},
                {"role": "user", "content": "文章を英語に翻訳してください。 : 私の名前は中野です。"},
                {"role": "assistant", "content": "My name is Nakano."},
                {"role": "user", "content": "文章を英語に翻訳してください。 : GPTはOpenAIから2018年に以下の論文で提案されたモデルで、基本的にはTransformerをベースに、事前学習-ファインチューニングをすることで非常に高い精度を達成したモデルです。"},
                {"role": "assistant", "content": "GPT is a model proposed by OpenAI in 2018 in the following paper, which is basically based on Transformer and achieves very high accuracy by pre-training - fine tuning."},
                {"role": "user", "content": "文章を英語に翻訳してください。 : {0}".format(japanese)}
                ]
            )
        arr = []
        for i in range(arg_n):
            arr.append(response['choices'][i]['message']['content'])
        return(arr)

    def start_time_func(self):
        self.start_time = time.perf_counter()

    def end_time_func(self):
        self.end_time = time.perf_counter()

    def print_time_func(self):
        elapsed_sec = self.end_time - self.start_time
        print('  => elapsed: {0:.2f} sec'.format(elapsed_sec))

    def romaji_1_task(self,arg_n):
        for romaji in self.romaji_list1:
            self.start_time_func()
            result = self.romaji_to_kanji(romaji,arg_n)
            self.end_time_func()
            print('IN : {0}'.format(romaji))
            for s in result:
                print('OUT: {0}'.format(s))
                self.print_time_func()
            print()

    def romaji_2_task(self,arg_n):
        for romaji in self.romaji_list2:
            self.start_time_func()
            result = self.romaji_to_yomigana(romaji,arg_n)
            self.end_time_func()
            print('IN : {0}'.format(romaji))
            for s in result:
                print('OUT: {0}'.format(s))
                self.print_time_func()
            print()

    def yomigana_task(self,arg_n):
        for kanji in self.kanji_list:
            self.start_time_func()
            result = self.kanji_to_yomigana(kanji,arg_n)
            self.end_time_func()
            print('IN : {0}'.format(kanji))
            for s in result:
                print('OUT: {0}'.format(s))
                self.print_time_func()
            print()

    def to_english_task(self,arg_n):
        for j in self.japanese_list:
            self.start_time_func()
            result = self.japanese_to_english(j,arg_n)
            self.end_time_func()
            print('IN : {0}'.format(j))
            for s in result:
                print('OUT: {0}'.format(s))
                self.print_time_func()                
            print()

def main(argv):
    gptTest = GptTest()
    if(1 < len(argv)):
        gptTest.set_model(argv[1])
    gptTest.romaji_1_task(3)
    gptTest.romaji_2_task(3)    
    gptTest.yomigana_task(3)
    gptTest.to_english_task(3)

if __name__ == "__main__":
     main(sys.argv)
