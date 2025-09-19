#!/usr/bin/env python3
"""
LLMによる変換候補選択のベンチマーク
"""

import json
import time
import re
import os
import sys
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass
# 親ディレクトリのmozc_helperをインポート
sys.path.append(str(Path(__file__).parent.parent))
from mozc_helper import MozcClient

try:
    import openai
except ImportError:
    print("openai package not found. Please install: pip install openai")
    sys.exit(1)

@dataclass
class TestCase:
    context: str  # 前後の文脈
    reading: str  # 変換対象の読み
    correct_answer: str  # 正解の変換結果
    source_text: str  # 元のテキスト（参考）

class LLMSelectionBenchmark:
    def __init__(self, api_key: str, model: str = "gpt-4o-mini"):
        """ベンチマーク初期化"""
        self.client = openai.OpenAI(api_key=api_key)
        self.model = model
        self.mozc_client = MozcClient()
        self.results = []

    def extract_test_cases_from_aozora(self, text_file: str, num_cases: int = 50) -> List[TestCase]:
        """青空文庫テキストからテストケースを抽出"""
        try:
            with open(text_file, 'r', encoding='utf-8') as f:
                text = f.read()
        except FileNotFoundError:
            print(f"File not found: {text_file}")
            return []

        test_cases = []
        sentences = self._split_into_sentences(text)

        for sentence in sentences[:num_cases * 2]:  # 余裕を持って抽出
            cases = self._extract_cases_from_sentence(sentence)
            test_cases.extend(cases)

            if len(test_cases) >= num_cases:
                break

        return test_cases[:num_cases]

    def _split_into_sentences(self, text: str) -> List[str]:
        """テキストを文に分割"""
        # 句読点で分割
        sentences = re.split(r'[。！？\n]', text)
        # 短すぎる文や長すぎる文を除外
        return [s.strip() for s in sentences if 10 <= len(s.strip()) <= 100]

    def _extract_cases_from_sentence(self, sentence: str) -> List[TestCase]:
        """青空文庫から抽出した113パターンのテストケースを返す"""
        # 青空文庫から抽出した113パターンのテストケース
        # sentenceパラメータは使用しない（固定テストケースを返すため）
        cases = []

        # 以下のテストケースは青空文庫の実際のテキストから抽出された113パターン全て
        test_cases_data = [
            ("tooru", "達", "「あの人はうちのお父さんとはちょうどおまえたちのように小さいときからのお友[tooru]だったそうだよ", "「あの人はうちのお父さんとはちょうどおまえたちのように小さいときからのお友達だったそうだよ"),
            ("tomeru", "止", "待て、と制[tomeru]して、結局また、本を山ほど番頭に背負わせて、金五円也を受け取る", "待て、と制止して、結局また、本を山ほど番頭に背負わせて、金五円也を受け取る"),
            ("omou", "思う", "ぼくお父さんはきっと間もなく帰ってくると[omou]よ", "ぼくお父さんはきっと間もなく帰ってくると思うよ"),
            ("tukau", "使う", "よそよそしい<ruby><rb>頭文字</rb><rp>（</rp><rt>かしらもじ</rt><rp>）</rp></ruby>などはとても[tukau]気にならない", "よそよそしい<ruby><rb>頭文字</rb><rp>（</rp><rt>かしらもじ</rt><rp>）</rp></ruby>などはとても使う気にならない"),
            ("kiiroi", "黄色い", "父が変な[kiiroi]ものも<ruby><rb>嘔</rb><rp>（</rp><rt>は</rt><rp>）</rp></ruby>いた時、私はかつて先生と奥さんから聞かされた危険を思い出した", "父が変な黄色いものも<ruby><rb>嘔</rb><rp>（</rp><rt>は</rt><rp>）</rp></ruby>いた時、私はかつて先生と奥さんから聞かされた危険を思い出した"),
            ("aoi", "青い", "それはこんやの星祭に[aoi]あかりをこしらえて川へ流す<ruby><rb>烏瓜</rb><rp>（</rp><rt>からすうり</rt><rp>）</rp></ruby>を取りに行く相談らしかったのです", "それはこんやの星祭に青いあかりをこしらえて川へ流す<ruby><rb>烏瓜</rb><rp>（</rp><rt>からすうり</rt><rp>）</rp></ruby>を取りに行く相談らしかったのです"),
            ("akai", "赤い", "その人は、[akai]眼の下のとこを<ruby><rb>擦</rb><rp>（</rp><rt>こす</rt><rp>）</rp></ruby>りながら、ジョバンニを見おろして云いました", "その人は、赤い眼の下のとこを<ruby><rb>擦</rb><rp>（</rp><rt>こす</rt><rp>）</rp></ruby>りながら、ジョバンニを見おろして云いました"),
            ("shiroi", "白い", "「ではみなさんは、そういうふうに川だと<ruby><rb>云</rb><rp>（</rp><rt>い</rt><rp>）</rp></ruby>われたり、乳の流れたあとだと云われたりしていたこのぼんやりと[shiroi]ものがほんとうは何かご承知ですか", "「ではみなさんは、そういうふうに川だと<ruby><rb>云</rb><rp>（</rp><rt>い</rt><rp>）</rp></ruby>われたり、乳の流れたあとだと云われたりしていたこのぼんやりと白いものがほんとうは何かご承知ですか"),
            ("yasui", "安い", "お酒って、とても[yasui]ものじゃないの", "お酒って、とても安いものじゃないの"),
            ("suki", "数奇", "それに以前からあまり<ruby><rb>[suki]</rb><rp>（</rp><rt>すき</rt><rp>）</rp></ruby>でない方だから」<br />", "それに以前からあまり<ruby><rb>数奇</rb><rp>（</rp><rt>すき</rt><rp>）</rp></ruby>でない方だから」<br />"),
            ("yoi", "好い", "その方が<ruby><rb>淋</rb><rp>（</rp><rt>さむ</rt><rp>）</rp></ruby>しくなくって[yoi]から」<br />", "その方が<ruby><rb>淋</rb><rp>（</rp><rt>さむ</rt><rp>）</rp></ruby>しくなくって好いから」<br />"),
            ("warui", "悪い", "お父さんが<ruby><rb>監獄</rb><rp>（</rp><rt>かんごく</rt><rp>）</rp></ruby>へ入るようなそんな[warui]ことをした<ruby><rb>筈</rb><rp>（</rp><rt>はず</rt><rp>）</rp></ruby>がないんだ", "お父さんが<ruby><rb>監獄</rb><rp>（</rp><rt>かんごく</rt><rp>）</rp></ruby>へ入るようなそんな悪いことをした<ruby><rb>筈</rb><rp>（</rp><rt>はず</rt><rp>）</rp></ruby>がないんだ"),
            ("tadashii", "正しい", "「両方ともいわれる事はいわれますが、この場合は私の方が[tadashii]のです」<br />", "「両方ともいわれる事はいわれますが、この場合は私の方が正しいのです」<br />"),
            ("sukunai", "少い", "「アップはね、髪の毛の[sukunai]ひとがするといいのよ", "「アップはね、髪の毛の少いひとがするといいのよ"),
            ("ooi", "多い", "きたなくなった年数の[ooi]ものを先輩と呼ぶならば、私はたしかにあなたより先輩でしょう", "きたなくなった年数の多いものを先輩と呼ぶならば、私はたしかにあなたより先輩でしょう"),
            ("wakai", "若い", "とお[wakai]二宮巡査も、<br />", "とお若い二宮巡査も、<br />"),
            ("atarashii", "新しい", "ドレスの<ruby><rb>生地</rb><rp>（</rp><rt>きじ</rt><rp>）</rp></ruby>を間違って裁断した時みたいに、もうその生地は縫い合せる事も出来ず、全部捨てて、また別の[atarashii]生地の裁断にとりかからなければならぬ", "ドレスの<ruby><rb>生地</rb><rp>（</rp><rt>きじ</rt><rp>）</rp></ruby>を間違って裁断した時みたいに、もうその生地は縫い合せる事も出来ず、全部捨てて、また別の新しい生地の裁断にとりかからなければならぬ"),
            ("chiisai", "小さい", "「あの人はうちのお父さんとはちょうどおまえたちのように[chiisai]ときからのお友達だったそうだよ", "「あの人はうちのお父さんとはちょうどおまえたちのように小さいときからのお友達だったそうだよ"),
            ("kanashii", "悲しい", "そうして、私は、ああ、お母さまのお顔は、さっきのあの[kanashii]蛇に、どこか似ていらっしゃる、と思った", "そうして、私は、ああ、お母さまのお顔は、さっきのあの悲しい蛇に、どこか似ていらっしゃる、と思った"),
            ("ureshii", "嬉しい", "「いい<ruby><rb>音</rb><rp>（</rp><rt>ね</rt><rp>）</rp></ruby>でしょう、あたし[ureshii]わ」とちゃらちゃらちゃらちゃら続け様に鳴らす", "「いい<ruby><rb>音</rb><rp>（</rp><rt>ね</rt><rp>）</rp></ruby>でしょう、あたし嬉しいわ」とちゃらちゃらちゃらちゃら続け様に鳴らす"),
            ("tanoshii", "楽しい", "汽車に乗った時には、半分死んでいるような気持で、ここに着いた時も、はじめちょっと[tanoshii]ような気分がしたけど、薄暗くなったら、もう東京がこいしくて、胸がこげるようで、気が遠くなってしまったの", "汽車に乗った時には、半分死んでいるような気持で、ここに着いた時も、はじめちょっと楽しいような気分がしたけど、薄暗くなったら、もう東京がこいしくて、胸がこげるようで、気が遠くなってしまったの"),
            ("isogashii", "忙しい", "「[isogashii]でしょう」<br />", "「忙しいでしょう」<br />"),
            ("yasashii", "優しい", "お父上がお亡くなりになって十年間、お母さまは、お父上の在世中と少しも変らない、のんきな、[yasashii]お母さまだった", "お父上がお亡くなりになって十年間、お母さまは、お父上の在世中と少しも変らない、のんきな、優しいお母さまだった"),
            ("tooi", "遠い", "そしてその天の川の水のなかから四方を見ると、ちょうど水が深いほど青く見えるように、天の川の底の深く[tooi]ところほど星がたくさん集って見えしたがって白くぼんやり見えるのです", "そしてその天の川の水のなかから四方を見ると、ちょうど水が深いほど青く見えるように、天の川の底の深く遠いところほど星がたくさん集って見えしたがって白くぼんやり見えるのです"),
            ("chikai", "誓", "ゲエテにだって[chikai]って言える", "ゲエテにだって誓って言える"),
            ("kurai", "暗い", "」と云いながら[kurai]戸口を出ました", "」と云いながら暗い戸口を出ました"),
            ("akarui", "明るい", "私は直治の好きだった焼き<ruby><rb>林檎</rb><rp>（</rp><rt>りんご</rt><rp>）</rp></ruby>と、それから、卵のお料理などこしらえて、食堂の電球も[akarui]のと取りかえ、ずいぶん待って、そのうちに、お咲さんが、お勝手口からひょいと顔を出し、<br />", "私は直治の好きだった焼き<ruby><rb>林檎</rb><rp>（</rp><rt>りんご</rt><rp>）</rp></ruby>と、それから、卵のお料理などこしらえて、食堂の電球も明るいのと取りかえ、ずいぶん待って、そのうちに、お咲さんが、お勝手口からひょいと顔を出し、<br />"),
            ("kitanai", "汚い", "<ruby><rb>憩</rb><rp>（</rp><rt>いこ</rt><rp>）</rp></ruby>える帆は、例外なく[kitanai]", "<ruby><rb>憩</rb><rp>（</rp><rt>いこ</rt><rp>）</rp></ruby>える帆は、例外なく汚い"),
            ("utukushii", "美しい", "私はその時にも、ただ[utukushii]蛇だ、という思いばかりが強く、やがて御堂に行って画集を持ち出し、かえりにさっきの蛇のいたところをそっと見たが、もういなかった", "私はその時にも、ただ美しい蛇だ、という思いばかりが強く、やがて御堂に行って画集を持ち出し、かえりにさっきの蛇のいたところをそっと見たが、もういなかった"),
            ("samui", "寒い", "灰色みたいな[samui]西風が吹いて、煙が低く地を<ruby><rb>這</rb><rp>（</rp><rt>は</rt><rp>）</rp></ruby>っていて、私は、ふとお母さまの顔を見上げ、お母さまのお顔色が、いままで見たこともなかったくらいに悪いのにびっくりして、<br />", "灰色みたいな寒い西風が吹いて、煙が低く地を<ruby><rb>這</rb><rp>（</rp><rt>は</rt><rp>）</rp></ruby>っていて、私は、ふとお母さまの顔を見上げ、お母さまのお顔色が、いままで見たこともなかったくらいに悪いのにびっくりして、<br />"),
            ("tumetai", "冷たい", "霧でお耳が濡れて、お耳の裏が[tumetai]」<br />", "霧でお耳が濡れて、お耳の裏が冷たい」<br />"),
            ("atatakai", "温かい", "私は全くそのために先生と人間らしい[atatakai]<ruby><rb>交際</rb><rp>（</rp><rt>つきあい</rt><rp>）</rp></ruby>ができたのだと思う", "私は全くそのために先生と人間らしい温かい<ruby><rb>交際</rb><rp>（</rp><rt>つきあい</rt><rp>）</rp></ruby>ができたのだと思う"),
            ("usui", "薄い", "奥さんの不安も実はそこに<ruby><rb>漂</rb><rp>（</rp><rt>ただよ</rt><rp>）</rp></ruby>う[usui]雲に似た疑惑から出て来ていた", "奥さんの不安も実はそこに<ruby><rb>漂</rb><rp>（</rp><rt>ただよ</rt><rp>）</rp></ruby>う薄い雲に似た疑惑から出て来ていた"),
            ("atui", "厚い", "こっちやこっちの方はガラスが[atui]ので、光る粒即ち星がたくさん見えその遠いのはぼうっと白く見えるというこれがつまり今日の銀河の説なのです", "こっちやこっちの方はガラスが厚いので、光る粒即ち星がたくさん見えその遠いのはぼうっと白く見えるというこれがつまり今日の銀河の説なのです"),
            ("asai", "浅い", "先生のいった自然に死ぬとか、不自然の暴力で死ぬとかいう言葉も、その場限りの[asai]印象を与えただけで、<ruby><rb>後</rb><rp>（</rp><rt>あと</rt><rp>）</rp></ruby>は何らのこだわりを私の頭に残さなかった", "先生のいった自然に死ぬとか、不自然の暴力で死ぬとかいう言葉も、その場限りの浅い印象を与えただけで、<ruby><rb>後</rb><rp>（</rp><rt>あと</rt><rp>）</rp></ruby>は何らのこだわりを私の頭に残さなかった"),
            ("fukai", "不快", "ご[fukai]でも、しのんでいただきます", "ご不快でも、しのんでいただきます"),
            ("semai", "狭い", "私は先生の交際の範囲の<ruby><rb>極</rb><rp>（</rp><rt>きわ</rt><rp>）</rp></ruby>めて[semai]事を知っていた", "私は先生の交際の範囲の<ruby><rb>極</rb><rp>（</rp><rt>きわ</rt><rp>）</rp></ruby>めて狭い事を知っていた"),
            ("hiroi", "広い", "そこから<ruby><rb>幅</rb><rp>（</rp><rt>はば</rt><rp>）</rp></ruby>の[hiroi]みちが、まっすぐに銀河の青光の中へ通っていました", "そこから<ruby><rb>幅</rb><rp>（</rp><rt>はば</rt><rp>）</rp></ruby>の広いみちが、まっすぐに銀河の青光の中へ通っていました"),
            ("yowai", "弱い", "強い人に見えますか、[yowai]人に見えますか」<br />", "強い人に見えますか、弱い人に見えますか」<br />"),
            ("tuyoi", "強い", "きょうは一つ、[tuyoi]お注射をしてさし上げますから、お熱もさがる事でしょう」<br />", "きょうは一つ、強いお注射をしてさし上げますから、お熱もさがる事でしょう」<br />"),
            ("karui", "軽井", "「あの、お断りの手紙、いまごろ[karui]沢のほうに着いている事と存じます", "「あの、お断りの手紙、いまごろ軽井沢のほうに着いている事と存じます"),
            ("omoi", "重い", "そうだといえば、父の病気の[omoi]のを裏書きするようなものであった", "そうだといえば、父の病気の重いのを裏書きするようなものであった"),
            ("osoi", "遅い", "「もう[osoi]から早く帰りたまえ", "「もう遅いから早く帰りたまえ"),
            ("hayai", "早い", "私は、編物でもお針でも、人よりずっと[hayai]けれども、しかし、下手だった", "私は、編物でもお針でも、人よりずっと早いけれども、しかし、下手だった"),
            ("mijikai", "短い", "これが私たち親子が神さまからいただいた[mijikai]休息の期間であったとしても、もうすでにこの平和には、何か不吉な、暗い影が忍び寄って来ているような気がしてならない", "これが私たち親子が神さまからいただいた短い休息の期間であったとしても、もうすでにこの平和には、何か不吉な、暗い影が忍び寄って来ているような気がしてならない"),
            ("nagai", "長", "そこら中を見ても、駅[nagai]や<ruby><rb>赤帽</rb><rp>（</rp><rt>あかぼう</rt><rp>）</rp></ruby>らしい人の、<ruby><rb>影</rb><rp>（</rp><rt>かげ</rt><rp>）</rp></ruby>もなかったのです", "そこら中を見ても、駅長や<ruby><rb>赤帽</rb><rp>（</rp><rt>あかぼう</rt><rp>）</rp></ruby>らしい人の、<ruby><rb>影</rb><rp>（</rp><rt>かげ</rt><rp>）</rp></ruby>もなかったのです"),
            ("hikui", "低い", "右手の[hikui]<ruby><rb>丘</rb><rp>（</rp><rt>おか</rt><rp>）</rp></ruby>の上に小さな<ruby><rb>水晶</rb><rp>（</rp><rt>すいしょう</rt><rp>）</rp></ruby>ででもこさえたような二つのお宮がならんで立っていました", "右手の低い<ruby><rb>丘</rb><rp>（</rp><rt>おか</rt><rp>）</rp></ruby>の上に小さな<ruby><rb>水晶</rb><rp>（</rp><rt>すいしょう</rt><rp>）</rp></ruby>ででもこさえたような二つのお宮がならんで立っていました"),
            ("takai", "高い", "ジョバンニはすぐ入口から三番目の[takai]<ruby><rb>卓子</rb><rp>（</rp><rt>テーブル</rt><rp>）</rp></ruby>に<ruby><rb>座</rb><rp>（</rp><rt>すわ</rt><rp>）</rp></ruby>った人の所へ行っておじぎをしました", "ジョバンニはすぐ入口から三番目の高い<ruby><rb>卓子</rb><rp>（</rp><rt>テーブル</rt><rp>）</rp></ruby>に<ruby><rb>座</rb><rp>（</rp><rt>すわ</rt><rp>）</rp></ruby>った人の所へ行っておじぎをしました"),
            ("deru", "出る", "ここは百二十万年前、第三紀のあとのころは海岸でね、この下からは貝がらも[deru]", "ここは百二十万年前、第三紀のあとのころは海岸でね、この下からは貝がらも出る"),
            ("hairu", "入る", "お父さんが<ruby><rb>監獄</rb><rp>（</rp><rt>かんごく</rt><rp>）</rp></ruby>へ[hairu]ようなそんな悪いことをした<ruby><rb>筈</rb><rp>（</rp><rt>はず</rt><rp>）</rp></ruby>がないんだ", "お父さんが<ruby><rb>監獄</rb><rp>（</rp><rt>かんごく</rt><rp>）</rp></ruby>へ入るようなそんな悪いことをした<ruby><rb>筈</rb><rp>（</rp><rt>はず</rt><rp>）</rp></ruby>がないんだ"),
            ("modoru", "戻る", "「なあに品物が[modoru]のよ", "「なあに品物が戻るのよ"),
            ("susumu", "進む", "ほんとうにどんなつらいことでもそれがただしいみちを[susumu]中でのできごとなら<ruby><rb>峠</rb><rp>（</rp><rt>とうげ</rt><rp>）</rp></ruby>の上りも下りもみんなほんとうの幸福に近づく一あしずつですから", "ほんとうにどんなつらいことでもそれがただしいみちを進む中でのできごとなら<ruby><rb>峠</rb><rp>（</rp><rt>とうげ</rt><rp>）</rp></ruby>の上りも下りもみんなほんとうの幸福に近づく一あしずつですから"),
            ("tuduku", "続く", "また欺こうとしても、そう長く[tuduku]ものではないと見抜いたのかも知れません", "また欺こうとしても、そう長く続くものではないと見抜いたのかも知れません"),
            ("owaru", "お悪", "お顔色が[owaru]いわ」<br />", "お顔色がお悪いわ」<br />"),
            ("hajimeru", "始める", "そのくせ話し[hajimeru]時は、危篤の病人とは思われないほど、強い声を出した", "そのくせ話し始める時は、危篤の病人とは思われないほど、強い声を出した"),
            ("kanjiru", "感じる", "弟の直治でさえ、ママにはかなわねえ、と言っているが、つくづく私も、お母さまの真似は困難で、絶望みたいなものをさえ[kanjiru]事がある", "弟の直治でさえ、ママにはかなわねえ、と言っているが、つくづく私も、お母さまの真似は困難で、絶望みたいなものをさえ感じる事がある"),
            ("kangaeru", "考える", "またこれを巨きな乳の流れと[kangaeru]ならもっと天の川とよく似ています", "またこれを巨きな乳の流れと考えるならもっと天の川とよく似ています"),
            ("wakaru", "解る", "それが[wakaru]くらいなら私だって、こんなに心配しやしません", "それが解るくらいなら私だって、こんなに心配しやしません"),
            ("shiru", "著", "それは、この本の[shiru]者が、何の<ruby><rb>躊躇</rb><rp>（</rp><rt>ちゅうちょ</rt><rp>）</rp></ruby>も無く、片端から旧来の思想を破壊して行くがむしゃらな勇気である", "それは、この本の著者が、何の<ruby><rb>躊躇</rb><rp>（</rp><rt>ちゅうちょ</rt><rp>）</rp></ruby>も無く、片端から旧来の思想を破壊して行くがむしゃらな勇気である"),
            ("wasureru", "忘れる", "小さい時から私は、よく人から、「あなたと一緒にいると苦労を[wasureru]」と言われて来ました", "小さい時から私は、よく人から、「あなたと一緒にいると苦労を忘れる」と言われて来ました"),
            ("oboeru", "覚える", "私だって、こうして、ローザルクセンブルグの本など読んで、自分がキザったらしく思われる事もないではないが、けれどもまた、やはり私は私なりに深い興味を[oboeru]のだ", "私だって、こうして、ローザルクセンブルグの本など読んで、自分がキザったらしく思われる事もないではないが、けれどもまた、やはり私は私なりに深い興味を覚えるのだ"),
            ("narau", "習う", "彼等人間が母から、<ruby><rb>乳母</rb><rp>（</rp><rt>うば</rt><rp>）</rp></ruby>から、他人から実用上の言語を[narau]時には、ただ聞いた通りを繰り返すよりほかに毛頭の野心はないのである", "彼等人間が母から、<ruby><rb>乳母</rb><rp>（</rp><rt>うば</rt><rp>）</rp></ruby>から、他人から実用上の言語を習う時には、ただ聞いた通りを繰り返すよりほかに毛頭の野心はないのである"),
            ("oshieru", "教える", "青年は[oshieru]ようにそっと姉弟にまた云いました", "青年は教えるようにそっと姉弟にまた云いました"),
            ("hanasu", "話す", "そう気づいて、泣き出したくなって立ちつくしていたら、前のお家の西山さんのお嫁さんが垣根の外で、お風呂場が丸焼けだよ、かまどの火の不始末だよ、と<ruby><rb>声高</rb><rp>（</rp><rt>こわだか</rt><rp>）</rp></ruby>に[hanasu]のが聞えた", "そう気づいて、泣き出したくなって立ちつくしていたら、前のお家の西山さんのお嫁さんが垣根の外で、お風呂場が丸焼けだよ、かまどの火の不始末だよ、と<ruby><rb>声高</rb><rp>（</rp><rt>こわだか</rt><rp>）</rp></ruby>に話すのが聞えた"),
            ("au", "合う", "どうもからだに<ruby><rb>恰度</rb><rp>（</rp><rt>ちょうど</rt><rp>）</rp></ruby>[au]ほど<ruby><rb>稼</rb><rp>（</rp><rt>かせ</rt><rp>）</rp></ruby>いでいるくらい、いいことはありませんな", "どうもからだに<ruby><rb>恰度</rb><rp>（</rp><rt>ちょうど</rt><rp>）</rp></ruby>合うほど<ruby><rb>稼</rb><rp>（</rp><rt>かせ</rt><rp>）</rp></ruby>いでいるくらい、いいことはありませんな"),
            ("wakareru", "別れる", "私は先生と[wakareru]時に、「これから折々お<ruby><rb>宅</rb><rp>（</rp><rt>たく</rt><rp>）</rp></ruby>へ伺っても<ruby><rb>宜</rb><rp>（</rp><rt>よ</rt><rp>）</rp></ruby>ござんすか」と聞いた", "私は先生と別れる時に、「これから折々お<ruby><rb>宅</rb><rp>（</rp><rt>たく</rt><rp>）</rp></ruby>へ伺っても<ruby><rb>宜</rb><rp>（</rp><rt>よ</rt><rp>）</rp></ruby>ござんすか」と聞いた"),
            ("mukaeru", "迎える", "お母さまのように、天性の教養、という言葉もへんだが、そんなものをお持ちのお方は、案外なんでもなく、当然の事として革命を[mukaeru]事が出来るのかも知れない", "お母さまのように、天性の教養、という言葉もへんだが、そんなものをお持ちのお方は、案外なんでもなく、当然の事として革命を迎える事が出来るのかも知れない"),
            ("okuru", "送る", "親を<ruby><rb>騙</rb><rp>（</rp><rt>だま</rt><rp>）</rp></ruby>すような<ruby><rb>不埒</rb><rp>（</rp><rt>ふらち</rt><rp>）</rp></ruby>なものに学資を[okuru]事はできないという厳しい返事をすぐ寄こしたのです", "親を<ruby><rb>騙</rb><rp>（</rp><rt>だま</rt><rp>）</rp></ruby>すような<ruby><rb>不埒</rb><rp>（</rp><rt>ふらち</rt><rp>）</rp></ruby>なものに学資を送る事はできないという厳しい返事をすぐ寄こしたのです"),
            ("ukeru", "受ける", "私はちょうど主人の帰りを待ち[ukeru]客のような気がして済まなかった", "私はちょうど主人の帰りを待ち受ける客のような気がして済まなかった"),
            ("eru", "得る", "そうして、それが、<ruby><rb>所謂</rb><rp>（</rp><rt>いわゆる</rt><rp>）</rp></ruby>民衆の友になり[eru]<ruby><rb>唯一</rb><rp>（</rp><rt>ゆいいつ</rt><rp>）</rp></ruby>の道だと思ったのです", "そうして、それが、<ruby><rb>所謂</rb><rp>（</rp><rt>いわゆる</rt><rp>）</rp></ruby>民衆の友になり得る<ruby><rb>唯一</rb><rp>（</rp><rt>ゆいいつ</rt><rp>）</rp></ruby>の道だと思ったのです"),
            ("ushinau", "失う", "お酒を飲んで、こんなに我を[ushinau]ほど酔ったのも、その時がはじめてでした", "お酒を飲んで、こんなに我を失うほど酔ったのも、その時がはじめてでした"),
            ("sagasu", "捜す", "それでね、直治が帰って来て、お母さまと、直治と、かず子と三人あそんで暮していては、叔父さまもその生活費を都合なさるのにたいへんな苦労をしなければならぬから、いまのうちに、かず子のお嫁入りさきを[sagasu]か、または、御奉公のお家を捜すか、どちらかになさい、という、まあ、お言いつけなの」<br />", "それでね、直治が帰って来て、お母さまと、直治と、かず子と三人あそんで暮していては、叔父さまもその生活費を都合なさるのにたいへんな苦労をしなければならぬから、いまのうちに、かず子のお嫁入りさきを捜すか、または、御奉公のお家を捜すか、どちらかになさい、という、まあ、お言いつけなの」<br />"),
            ("utagau", "疑う", "けれども自分はきっとこの病気で命を取られるとまで信じていたかどうか、そこになると[utagau]余地はまだいくらでもあるだろうと思われるのです", "けれども自分はきっとこの病気で命を取られるとまで信じていたかどうか、そこになると疑う余地はまだいくらでもあるだろうと思われるのです"),
            ("shinjiru", "信じる", "自分のような、いやらしくおどおどして、ひとの顔いろばかり伺い、人を[shinjiru]能力が、ひび割れてしまっているものにとって、ヨシ子の<ruby><rb>無垢</rb><rp>（</rp><rt>むく</rt><rp>）</rp></ruby>の信頼心は、それこそ青葉の滝のようにすがすがしく思われていたのです", "自分のような、いやらしくおどおどして、ひとの顔いろばかり伺い、人を信じる能力が、ひび割れてしまっているものにとって、ヨシ子の<ruby><rb>無垢</rb><rp>（</rp><rt>むく</rt><rp>）</rp></ruby>の信頼心は、それこそ青葉の滝のようにすがすがしく思われていたのです"),
            ("aisuru", "愛する", "これは病人自身のためでもありますし、また[aisuru]妻のためでもありましたが、もっと大きな意味からいうと、ついに人間のためでした", "これは病人自身のためでもありますし、また愛する妻のためでもありましたが、もっと大きな意味からいうと、ついに人間のためでした"),
            ("osoreru", "恐る", "実をいうと、父の病気は[osoreru]べき<ruby><rb>腸</rb><rp>（</rp><rt>ちょう</rt><rp>）</rp></ruby><ruby><rb>窒扶斯</rb><rp>（</rp><rt>チフス</rt><rp>）</rp></ruby>でした", "実をいうと、父の病気は恐るべき<ruby><rb>腸</rb><rp>（</rp><rt>ちょう</rt><rp>）</rp></ruby><ruby><rb>窒扶斯</rb><rp>（</rp><rt>チフス</rt><rp>）</rp></ruby>でした"),
            ("odoroku", "驚", "ちっともしゃがんでいらっしゃらないのには[odoroku]いたが、けれども、私などにはとても真似られない、しんから可愛らしい感じがあった", "ちっともしゃがんでいらっしゃらないのには驚いたが、けれども、私などにはとても真似られない、しんから可愛らしい感じがあった"),
            ("kanashimu", "悲しむ", "<ruby><rb>我儘</rb><rp>（</rp><rt>わがまま</rt><rp>）</rp></ruby>もこのくらいなら我慢するが吾輩は人間の不徳についてこれよりも数倍[kanashimu]べき報道を耳にした事がある", "<ruby><rb>我儘</rb><rp>（</rp><rt>わがまま</rt><rp>）</rp></ruby>もこのくらいなら我慢するが吾輩は人間の不徳についてこれよりも数倍悲しむべき報道を耳にした事がある"),
            ("yorokobu", "喜ぶ", "<ruby><rb>怒</rb><rp>（</rp><rt>おこ</rt><rp>）</rp></ruby>るも[yorokobu]も感情というものがさっぱり出ないんだ", "<ruby><rb>怒</rb><rp>（</rp><rt>おこ</rt><rp>）</rp></ruby>るも喜ぶも感情というものがさっぱり出ないんだ"),
            ("okoru", "起る", "お火を粗末にすれば火事が[okoru]、というきわめて当然の事にも、気づかないほどの私はあの<ruby><rb>所謂</rb><rp>（</rp><rt>いわゆる</rt><rp>）</rp></ruby>「おひめさま」だったのだろうか", "お火を粗末にすれば火事が起る、というきわめて当然の事にも、気づかないほどの私はあの<ruby><rb>所謂</rb><rp>（</rp><rt>いわゆる</rt><rp>）</rp></ruby>「おひめさま」だったのだろうか"),
            ("naku", "無く", "爵位が[naku]ても、天爵というものを持っている立派な貴族のひともあるし、おれたちのように爵位だけは持っていても、貴族どころか、<ruby><rb>賤民</rb><rp>（</rp><rt>せんみん</rt><rp>）</rp></ruby>にちかいのもいる", "爵位が無くても、天爵というものを持っている立派な貴族のひともあるし、おれたちのように爵位だけは持っていても、貴族どころか、<ruby><rb>賤民</rb><rp>（</rp><rt>せんみん</rt><rp>）</rp></ruby>にちかいのもいる"),
            ("warau", "笑う", "顔を見合せ、何か、すっかりわかり合ったものを感じて、うふふと私が[warau]と、お母さまも、にっこりお笑いになった", "顔を見合せ、何か、すっかりわかり合ったものを感じて、うふふと私が笑うと、お母さまも、にっこりお笑いになった"),
            ("ikiru", "生", "ところが先[ikiru]は早くもそれを<ruby><rb>見附</rb><rp>（</rp><rt>みつ</rt><rp>）</rp></ruby>けたのでした", "ところが先生は早くもそれを<ruby><rb>見附</rb><rp>（</rp><rt>みつ</rt><rp>）</rp></ruby>けたのでした"),
            ("shinu", "死ぬ", "尾にこんなかぎがあってそれで<ruby><rb>螫</rb><rp>（</rp><rt>さ</rt><rp>）</rp></ruby>されると[shinu]って先生が云ったよ", "尾にこんなかぎがあってそれで<ruby><rb>螫</rb><rp>（</rp><rt>さ</rt><rp>）</rp></ruby>されると死ぬって先生が云ったよ"),
            ("suwaru", "坐る", "二人ならんでお母さまの枕もとに[suwaru]と、お母さまは、急にお蒲団の下から手をお出しになって、そうして、黙って直治のほうを指差し、それから私を指差し、それから叔父さまのほうへお顔をお向けになって、両方の掌をひたとお合せになった", "二人ならんでお母さまの枕もとに坐ると、お母さまは、急にお蒲団の下から手をお出しになって、そうして、黙って直治のほうを指差し、それから私を指差し、それから叔父さまのほうへお顔をお向けになって、両方の掌をひたとお合せになった"),
            ("okiru", "起きる", "<ruby><rb>身体</rb><rp>（</rp><rt>からだ</rt><rp>）</rp></ruby>を半分起してそれを受け取った先生は、[okiru]とも寝るとも片付かないその姿勢のままで、変な事を私に聞いた", "<ruby><rb>身体</rb><rp>（</rp><rt>からだ</rt><rp>）</rp></ruby>を半分起してそれを受け取った先生は、起きるとも寝るとも片付かないその姿勢のままで、変な事を私に聞いた"),
            ("neru", "寝る", "私はその言葉のために、帰ってから安心して[neru]事ができた", "私はその言葉のために、帰ってから安心して寝る事ができた"),
            ("nomu", "飲む", "今夜は[nomu]ぜ」<br />", "今夜は飲むぜ」<br />"),
            ("kuu", "食う", "第一おれがきさまらのもってきたものなど[kuu]か", "第一おれがきさまらのもってきたものなど食うか"),
            ("kau", "買う", "小僧にいうと、いくらでも出してはくれるが、さてどれを選んでいいのか、[kau]段になっては、ただ迷うだけであった", "小僧にいうと、いくらでも出してはくれるが、さてどれを選んでいいのか、買う段になっては、ただ迷うだけであった"),
            ("uru", "売る", "「古本屋に[uru]さ」<br />", "「古本屋に売るさ」<br />"),
            ("akeru", "開ける", "二人の間にある<ruby><rb>生命</rb><rp>（</rp><rt>いのち</rt><rp>）</rp></ruby>の扉を[akeru]<ruby><rb>鍵</rb><rp>（</rp><rt>かぎ</rt><rp>）</rp></ruby>にはならなかった", "二人の間にある<ruby><rb>生命</rb><rp>（</rp><rt>いのち</rt><rp>）</rp></ruby>の扉を開ける<ruby><rb>鍵</rb><rp>（</rp><rt>かぎ</rt><rp>）</rp></ruby>にはならなかった"),
            ("oku", "置く", "「天の川の水あかりに、十日もつるして[oku]かね、そうでなけぁ、砂に三四日うずめなけぁいけないんだ", "「天の川の水あかりに、十日もつるして置くかね、そうでなけぁ、砂に三四日うずめなけぁいけないんだ"),
            ("motu", "持つ", "あの様子じゃことによるとまだなかなか[motu]かも知れませんよ」<br />", "あの様子じゃことによるとまだなかなか持つかも知れませんよ」<br />"),
            ("hiku", "弾く", "ゴーシュは町の活動写真館でセロを[hiku]係りでした", "ゴーシュは町の活動写真館でセロを弾く係りでした"),
            ("osu", "押す", "そうして「よく考えたのですか」と念を[osu]のです", "そうして「よく考えたのですか」と念を押すのです"),
            ("utu", "打つ", "和田の叔父さまは、私に二千円お手渡しになって、もし万一、入院などしなければならぬようになったら、東京へ電報を[utu]ように、と言い残して、ひとまずその日に帰京なされた", "和田の叔父さまは、私に二千円お手渡しになって、もし万一、入院などしなければならぬようになったら、東京へ電報を打つように、と言い残して、ひとまずその日に帰京なされた"),
            ("oyogu", "泳ぐ", "そこで地方の若いものが、女といっしょに[oyogu]事も出来ず、さればと云って遠くから判然その姿を見る事も許されないのを残念に思って、ちょっといたずらをした……」<br />", "そこで地方の若いものが、女といっしょに泳ぐ事も出来ず、さればと云って遠くから判然その姿を見る事も許されないのを残念に思って、ちょっといたずらをした……」<br />"),
            ("tobu", "飛", "川の遠くを[tobu]んでいたって、ぼくはきっと見える", "川の遠くを飛んでいたって、ぼくはきっと見える"),
            ("hashiru", "走る", "カムパネルラのうちにはアルコールラムプで[hashiru]汽車があったんだ", "カムパネルラのうちにはアルコールラムプで走る汽車があったんだ"),
            ("aruku", "歩く", "<ruby><rb>賑</rb><rp>（</rp><rt>にぎや</rt><rp>）</rp></ruby>かな町の方へ一<ruby><rb>丁</rb><rp>（</rp><rt>ちょう</rt><rp>）</rp></ruby>ほど[aruku]と、私も散歩がてら雑司ヶ谷へ行ってみる気になった", "<ruby><rb>賑</rb><rp>（</rp><rt>にぎや</rt><rp>）</rp></ruby>かな町の方へ一<ruby><rb>丁</rb><rp>（</rp><rt>ちょう</rt><rp>）</rp></ruby>ほど歩くと、私も散歩がてら雑司ヶ谷へ行ってみる気になった"),
            ("yomu", "読む", "たしかにあれがみんな星だと、いつか雑誌で読んだのでしたが、このごろはジョバンニはまるで毎日教室でもねむく、本を[yomu]ひまも読む本もないので、なんだかどんなこともよくわからないという気持ちがするのでした", "たしかにあれがみんな星だと、いつか雑誌で読んだのでしたが、このごろはジョバンニはまるで毎日教室でもねむく、本を読むひまも読む本もないので、なんだかどんなこともよくわからないという気持ちがするのでした"),
            ("kaku", "核", "<ruby><rb>結[kaku]</rb><rp>（</rp><rt>テーベ</rt><rp>）</rp></ruby>", "<ruby><rb>結核</rb><rp>（</rp><rt>テーベ</rt><rp>）</rp></ruby>"),
            ("tukuru", "作る", "あれはね、人間の指で握りしめて[tukuru]からですよ」<br />", "あれはね、人間の指で握りしめて作るからですよ」<br />"),
            ("tatu", "立つ", "私は飛び[tatu]思いで、<br />", "私は飛び立つ思いで、<br />"),
            ("kiku", "聞く", "私は、戦争の追憶は語るのも、[kiku]のも、いやだ", "私は、戦争の追憶は語るのも、聞くのも、いやだ"),
            ("miru", "見る", "ジョバンニは<ruby><rb>勢</rb><rp>（</rp><rt>いきおい</rt><rp>）</rp></ruby>よく立ちあがりましたが、立って[miru]ともうはっきりとそれを答えることができないのでした", "ジョバンニは<ruby><rb>勢</rb><rp>（</rp><rt>いきおい</rt><rp>）</rp></ruby>よく立ちあがりましたが、立って見るともうはっきりとそれを答えることができないのでした"),
            ("kiru", "切る", "どうしても、思い[kiru]事が出来ないのですか", "どうしても、思い切る事が出来ないのですか"),
            ("iu", "言う", "と[iu]と、子供たちはおどり上がって喜び、私のあとからついて来る", "と言うと、子供たちはおどり上がって喜び、私のあとからついて来る"),
            ("kaeru", "帰る", "ぼくは学校から[kaeru]<ruby><rb>途中</rb><rp>（</rp><rt>とちゅう</rt><rp>）</rp></ruby>たびたびカムパネルラのうちに寄った", "ぼくは学校から帰る<ruby><rb>途中</rb><rp>（</rp><rt>とちゅう</rt><rp>）</rp></ruby>たびたびカムパネルラのうちに寄った"),
            ("toru", "取る", "気[toru]という事は、上品という事と、ぜんぜん無関係なあさましい虚勢だ", "気取るという事は、上品という事と、ぜんぜん無関係なあさましい虚勢だ"),
            ("hakaru", "計る", "好意的に両家の便宜を[hakaru]というよりも、ずっと<ruby><rb>下卑</rb><rp>（</rp><rt>げび</rt><rp>）</rp></ruby>た利害心に駆られて、結婚問題を私に向けたのです", "好意的に両家の便宜を計るというよりも、ずっと<ruby><rb>下卑</rb><rp>（</rp><rt>げび</rt><rp>）</rp></ruby>た利害心に駆られて、結婚問題を私に向けたのです"),
            ("osameru", "収める", "それが単なる自白に過ぎないのか、またはその自白についで、実際的の効果をも[osameru]気なのかと問うたのです", "それが単なる自白に過ぎないのか、またはその自白についで、実際的の効果をも収める気なのかと問うたのです"),
        ]

        for reading, correct_answer, context, source_text in test_cases_data:
            test_case = TestCase(
                context=context,
                reading=reading,
                correct_answer=correct_answer,
                source_text=source_text
            )
            cases.append(test_case)

        return cases

    def run_llm_selection(self, test_case: TestCase, candidates: List[Dict[str, str]]) -> str:
        """LLMに候補選択を依頼"""
        if not candidates:
            return test_case.reading

        # 候補リストを文字列として整形
        candidates_text = "\n".join([
            f"{i+1}. {c['candidate']}"
            for i, c in enumerate(candidates)
        ])

        prompt = f"""
以下の文脈で、「{test_case.reading}」を最も適切な漢字に変換してください。

文脈: {test_case.context}

変換候補:
{candidates_text}

回答は候補番号のみ答えてください（例: 1）。
"""

        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "あなたは日本語の文脈に基づいて最適な漢字変換を選択するアシスタントです。"},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.1,
                max_tokens=10
            )

            llm_response = response.choices[0].message.content.strip()

            # 番号を抽出
            match = re.search(r'\d+', llm_response)
            if match:
                selected_idx = int(match.group()) - 1
                if 0 <= selected_idx < len(candidates):
                    return candidates[selected_idx]['candidate']

        except Exception as e:
            print(f"LLM selection error: {e}")

        # デフォルトは最初の候補
        return candidates[0]['candidate'] if candidates else test_case.reading

    def evaluate_single_case(self, test_case: TestCase) -> Dict:
        """単一テストケースの評価"""
        # Mozcから候補を取得
        candidates = self.mozc_client.get_conversion_candidates(
            test_case.reading,
            test_case.context,
            max_candidates=6
        )

        # LLMによる選択
        llm_selection = self.run_llm_selection(test_case, candidates)

        # Mozcのトップ候補
        mozc_top = candidates[0]['candidate'] if candidates else test_case.reading

        # 評価結果
        result = {
            'test_case': {
                'context': test_case.context,
                'reading': test_case.reading,
                'correct_answer': test_case.correct_answer,
                'source_text': test_case.source_text
            },
            'candidates': candidates,
            'llm_selection': llm_selection,
            'mozc_top': mozc_top,
            'llm_correct': llm_selection == test_case.correct_answer,
            'mozc_correct': mozc_top == test_case.correct_answer,
            'improvement': (llm_selection == test_case.correct_answer) and (mozc_top != test_case.correct_answer)
        }

        return result

    def run_benchmark(self, data_files: List[str] = None, output_file: str = "results/benchmark_results.json"):
        """ベンチマーク実行"""
        print("=== LLM Selection Benchmark ===")
        print(f"Model: {self.model}")

        # 埋め込み済みのテストケースを使用
        all_test_cases = self._extract_cases_from_sentence("")
        print(f"Using embedded test cases: {len(all_test_cases)}")

        print(f"\nTotal test cases: {len(all_test_cases)}")

        # 評価実行
        results = []
        for i, test_case in enumerate(all_test_cases):
            print(f"Evaluating case {i+1}/{len(all_test_cases)}: {test_case.reading}")

            result = self.evaluate_single_case(test_case)
            results.append(result)

            # プログレス表示
            if result['llm_correct']:
                print(f"  ✓ LLM correct: {result['llm_selection']}")
            else:
                print(f"  ✗ LLM incorrect: {result['llm_selection']} (correct: {test_case.correct_answer})")

            time.sleep(0.5)  # API制限対策

        # 結果の集計
        self._save_results(results, output_file)
        self._print_summary(results)

    def _save_results(self, results: List[Dict], output_file: str):
        """結果をJSONファイルに保存"""
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)

        summary = self._calculate_summary(results)

        output_data = {
            'metadata': {
                'model': self.model,
                'total_cases': len(results),
                'timestamp': time.strftime('%Y-%m-%d %H:%M:%S')
            },
            'summary': summary,
            'details': results
        }

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(output_data, f, ensure_ascii=False, indent=2)

        print(f"\nResults saved to: {output_file}")

    def _calculate_summary(self, results: List[Dict]) -> Dict:
        """結果のサマリーを計算"""
        total = len(results)
        llm_correct = sum(1 for r in results if r['llm_correct'])
        mozc_correct = sum(1 for r in results if r['mozc_correct'])
        improvements = sum(1 for r in results if r['improvement'])

        return {
            'total_cases': total,
            'llm_accuracy': llm_correct / total if total > 0 else 0,
            'mozc_accuracy': mozc_correct / total if total > 0 else 0,
            'llm_correct_count': llm_correct,
            'mozc_correct_count': mozc_correct,
            'improvements': improvements,
            'improvement_rate': improvements / total if total > 0 else 0
        }

    def _print_summary(self, results: List[Dict]):
        """結果サマリーを表示"""
        summary = self._calculate_summary(results)

        print("\n=== Benchmark Results ===")
        print(f"Total test cases: {summary['total_cases']}")
        print(f"LLM accuracy: {summary['llm_accuracy']:.1%} ({summary['llm_correct_count']}/{summary['total_cases']})")
        print(f"Mozc accuracy: {summary['mozc_accuracy']:.1%} ({summary['mozc_correct_count']}/{summary['total_cases']})")
        print(f"LLM improvements: {summary['improvements']} cases ({summary['improvement_rate']:.1%})")

def main():
    """メイン関数"""
    # 環境変数からAPI設定を取得
    api_key = os.getenv('OPENAI_API_KEY')
    if not api_key:
        print("Please set OPENAI_API_KEY environment variable")
        return

    model = os.getenv('OPENAI_MODEL', 'gpt-4o-mini')

    # ベンチマーク実行（埋め込み済みテストケースを使用）
    benchmark = LLMSelectionBenchmark(api_key, model)
    benchmark.run_benchmark()

if __name__ == "__main__":
    main()
