#!/usr/bin/env python3
"""
変換候補をシミュレートするクライアント（Mozcの動作をシミュレート）
実際のMozc履歴データに基づく126パターン（scoreパラメーター削除版）
"""

from typing import List, Dict

class MozcClient:
    def __init__(self):
        """シミュレーションクライアントを初期化"""
        pass

    def get_conversion_candidates(self,
                                reading: str,
                                context: str = "",
                                max_candidates: int = 10) -> List[Dict[str, str]]:
        """
        指定された読みの変換候補を取得（シミュレーション）

        Args:
            reading: 変換対象の読み (ひらがな)
            context: 前後の文脈
            max_candidates: 最大候補数

        Returns:
            候補のリスト [{"candidate": "候補"}, ...]
        """
        return self._simulate_conversion(reading, max_candidates)

    def _simulate_conversion(self, reading: str, max_candidates: int) -> List[Dict[str, str]]:
        """
        変換候補をシミュレート（実際のMozc履歴データベース 126パターン）
        """

        # 実際のMozc履歴に基づく変換候補データベース（scoreなし）
        conversion_map = {
            "tooru": ['通る', 'とおる', '透る', '達', '徹', '理', 'tooru'],
            "tomaru": ['止まる', '泊まる', 'とまる', '留まる', '停まる', '泊る', 'tomaru'],
            "tomeru": ['止める', 'とめる', '停める', '止', '留める', '泊める', 'tomeru'],
            "omou": ['思う', 'おもう', '想う', '謂う', '重う', '懐う', 'omou'],
            "tukau": ['使う', 'つかう', '遣う', '付かう', '着かう', '点かう', 'tukau'],
            "kiiroi": ['黄色い', 'きいろい', '黄いろい', 'kiiroi'],
            "aoi": ['青い', 'あおい', '蒼い', '蒼', '蒼井', '碧い', 'aoi'],
            "akai": ['赤い', '紅い', 'あかい', '朱い', '明い', '紅井', 'akai'],
            "kuroi": ['黒い', 'くろい', 'kuroi'],
            "shiroi": ['白い', 'しろい', '城井', '皓', '知ろい', '私ロイ', 'shiroi'],
            "yasui": ['安い', 'やすい', '易い', '易居', '安居', '矢水', 'yasui'],
            "kirai": ['きらい', '喜来', '起来', '切らい', '生ライ', '機雷', 'kirai'],
            "suki": ['すき', '空き', '数奇', '漉き', '隙', '鋤', 'suki'],
            "yoi": ['よい', '好い', '善い', '佳い', '宵', '酔', 'yoi'],
            "warui": ['悪い', 'わるい', '惡い', '兇い', '悪井', '話類', 'warui'],
            "tadashii": ['正しい', 'ただしい', 'tadashii'],
            "sukunai": ['すくない', '少い', '尠い', '空くない', 'す区内', 'す宮内', 'sukunai'],
            "ooi": ['多い', 'おおい', 'お負い', 'お追い', 'お老い', '覆い', 'ooi'],
            "wakai": ['若い', 'わかい', '沸かい', '若生', '湧かい', '話界', 'wakai'],
            "furui": ['ふるい', '旧い', '振るい', '奮い', '震い', '篩い', 'furui'],
            "atarashii": ['新しい', 'あたらしい', '阿多らしい', 'atarashii'],
            "chiisai": ['小さい', 'ちいさい', 'chiisai'],
            "ookii": ['おおきい', 'お起きい', '大忌諱', '大貴意', '大きい', 'ookii'],
            "kanashii": ['悲しい', '哀しい', 'かなしい', 'kanashii'],
            "ureshii": ['嬉しい', 'うれしい', 'ureshii'],
            "tanoshii": ['楽しい', 'たのしい', '愉しい', '樂しい', 'tanoshii'],
            "isogashii": ['忙しい', 'いそがしい', 'いそがし胃', 'いそがし意', 'isogashii'],
            "yasashii": ['優しい', 'やさしい', '易しい', 'yasashii'],
            "muzukashii": ['難しい', 'むずかしい', 'むづかしい', 'muzukashii'],
            "tooi": ['遠い', 'とおい', '十威', '悠い', '遠井', 'tooi'],
            "chikai": ['近い', 'ちかい', '誓', '血界', '智会', '盟', 'chikai'],
            "kurai": ['くらい', '暗い', '喰らい', '食らい', '昏い', '儚い', 'kurai'],
            "akarui": ['明るい', 'あかるい', 'akarui'],
            "kitanai": ['汚い', 'きたない', '穢い', 'kitanai'],
            "utukushii": ['美しい', 'うつくしい', '鬱くしい', 'utukushii'],
            "samui": ['寒い', 'さむい', 'samui'],
            "tumetai": ['冷たい', 'つめたい', '詰めたい', '積めたい', '摘めたい', 'tumetai'],
            "atatakai": ['温かい', '暖かい', 'あたたかい', 'atatakai'],
            "usui": ['薄い', 'うすい', '笛吹', '羽水', '雨水', '臼井', 'usui'],
            "atui": ['暑い', '熱い', '厚い', 'あつい', '篤い', 'atui'],
            "asai": ['浅い', 'あさい', '朝生', '浅生', '浅井', '朝井', 'asai'],
            "fukai": ['ふかい', '不快', '深井', '腐海', '付会', '府会', 'fukai'],
            "semai": ['狭い', 'せまい', '狹い', '施米', 'semai'],
            "hiroi": ['広い', '拾い', 'ひろい', '寛い', '廣井', '廣い', 'hiroi'],
            "yowai": ['よわい', '歯い', '弱井', '弱い', 'yowai'],
            "tuyoi": ['強い', 'つよい', '勁い', 'つ酔い', '勍', 'tuyoi'],
            "karui": ['軽い', 'かるい', '軽井', '過類', '可類', '過塁', 'karui'],
            "omoi": ['重い', 'おもい', '懐い', '思井', '謂い', '思い', 'omoi'],
            "osoi": ['遅い', 'おそい', '襲い', 'お沿い', '晩い', 'お添い', 'osoi'],
            "hayai": ['早い', 'はやい', '疾い', '捷い', '疾医', '速い', 'hayai'],
            "mijikai": ['短い', 'みじかい', '身近い', '未自戒', 'み次回', '未自壊', 'mijikai'],
            "nagai": ['長い', 'ながい', '長', '镸', '永い', '永井', 'nagai'],
            "hikui": ['低い', 'ひくい', '非杭', '被杭', '非悔い', '被悔い', 'hikui'],
            "takai": ['高い', 'たかい', '高', '高価い', '喬', '鷹居', 'takai'],
            "deru": ['出る', 'でる', 'deru'],
            "hairu": ['入る', 'はいる', '這入る', '配流', 'hairu'],
            "sagaru": ['さがる', 'sagaru'],
            "agaru": ['あがる', '挙がる', '揚がる', '騰がる', '和了る', 'agaru'],
            "modoru": ['戻る', 'もどる', 'modoru'],
            "susumu": ['進む', 'すすむ', '攻', '勧', '生', '前', 'susumu'],
            "tuduku": ['つづく', '都竹', '通津区', '続く', 'tuduku'],
            "owaru": ['終わる', '終る', 'おわる', 'お悪', '畢る', 'お割る', 'owaru'],
            "hajimeru": ['始める', 'はじめる', '初める', '創める', 'hajimeru'],
            "erabu": ['選ぶ', 'えらぶ', '撰ぶ', '択ぶ', 'えら部', 'erabu'],
            "kimeru": ['決める', '極める', 'きめる', '决める', '生メル', '既メル', 'kimeru'],
            "kanjiru": ['感じる', 'かんじる', '観じる', '缶汁', 'kanjiru'],
            "kangaeru": ['考える', 'かんがえる', 'kangaeru'],
            "wakaru": ['わかる', '分かる', '判る', '解る', '分る', '解かる', 'wakaru'],
            "shiru": ['知る', 'しる', '識る', '著', '私る', '記', 'shiru'],
            "wasureru": ['忘れる', 'わすれる', 'wasureru'],
            "oboeru": ['覚える', 'おぼえる', '憶える', 'oboeru'],
            "narau": ['習う', '倣う', 'ならう', '鳴らう', '成らう', '生らう', 'narau'],
            "oshieru": ['教える', 'おしえる', '押し得る', '訓える', '推し得る', 'おし得る', 'oshieru'],
            "hanasu": ['話す', '離す', '放す', 'はなす', 'hanasu'],
            "au": ['合う', '会う', 'あう', '遭う', '逢う', '遇う', 'au'],
            "wakareru": ['分かれる', '別れる', 'わかれる', '沸かれる', '湧かれる', '涌かれる', 'wakareru'],
            "mukaeru": ['迎える', 'むかえる', '向かえる', '逢える', '無カエル', '無蛙', 'mukaeru'],
            "okuru": ['送る', '贈る', 'おくる', '御クル', 'お佝僂', '雄クル', 'okuru'],
            "ukeru": ['受ける', 'うける', '請ける', '承ける', '享ける', 'ukeru'],
            "ataeru": ['あたえる', '與える', 'ataeru'],
            "eru": ['得る', 'える', '獲る', 'eru'],
            "ushinau": ['失う', '喪う', 'うしなう', '牛なう', '武氏なう', 'ushinau'],
            "mitukeru": ['見つける', 'みつける', '見付ける', 'mitukeru'],
            "sagasu": ['探す', 'さがす', '捜す', 'sagasu'],
            "utagau": ['疑う', 'うたがう', 'utagau'],
            "shinjiru": ['信じる', 'しんじる', 'shinjiru'],
            "aisuru": ['愛する', '会いする', 'あいする', '合いする', '逢いする', '遭いする', 'aisuru'],
            "osoreru": ['恐れる', '恐る', '怖れる', 'おそれる', '畏れる', 'お逸れる', 'osoreru'],
            "odoroku": ['驚く', 'おどろく', '驚', '踊ろく', '小土呂区', 'odoroku'],
            "kanashimu": ['悲しむ', '哀しむ', 'かなしむ', 'kanashimu'],
            "yorokobu": ['喜ぶ', 'よろこぶ', '悦ぶ', '歓ぶ', '慶ぶ', 'yorokobu'],
            "okoru": ['起こる', '怒る', 'おこる', '起る', '興る', '煽る', 'okoru'],
            "naku": ['なく', '無く', '泣く', '（泣）', '(泣)', '鳴く', 'naku'],
            "warau": ['笑う', 'わらう', '嗤う', '嘲笑う', '微笑う', '咲う', 'warau'],
            "ikiru": ['生きる', '活きる', 'いきる', '生', '異キル', 'ikiru'],
            "shinu": ['死ぬ', 'しぬ', '私ぬ', 'shinu'],
            "suwaru": ['座る', 'すわる', '坐る', '据わる', 'す悪', 'suwaru'],
            "okiru": ['起きる', 'おきる', 'お切る', '熾きる', '御キル', 'お斬る', 'okiru'],
            "neru": ['寝る', 'ねる', '練る', '錬る', '煉る', 'neru'],
            "nomu": ['飲む', 'のむ', '呑む', 'nomu'],
            "kuu": ['食う', 'くう', '喰う', 'く雨', '区雨', '倥', 'kuu'],
            "kau": ['買う', '飼う', 'かう', '交う', '支う', '過兎', 'kau'],
            "uru": ['売る', 'うる', '賣る', '粳', '得る', '憂る', 'uru'],
            "shimeru": ['占める', '締める', '閉める', 'しめる', '絞める', '湿る', 'shimeru'],
            "akeru": ['開ける', 'あける', '空ける', '明ける', '開る', 'akeru'],
            "oku": ['置く', 'おく', '於く', '措く', '奥', '億', 'oku'],
            "motu": ['持つ', 'もつ', '保つ', '沒', '没', '縺', 'motu'],
            "hiku": ['ひく', '惹く', '弾く', '挽く', '牽く', '低', 'hiku'],
            "osu": ['押す', 'おす', '推す', '押忍', '捺す', '雄', 'osu'],
            "utu": ['打つ', 'うつ', '撃つ', '討つ', '射つ', '伐つ', 'utu'],
            "oyogu": ['泳ぐ', 'およぐ', '游ぐ', '御ヨグ', '雄ヨグ', 'oyogu'],
            "tobu": ['飛ぶ', 'とぶ', '跳ぶ', '飛', '翔ぶ', 'tobu'],
            "hashiru": ['走る', 'はしる', '奔る', '走', '疾走る', 'hashiru'],
            "aruku": ['歩く', 'あるく', '[全] かたかな', '[全] カタカナ', 'aruku'],
            "yomu": ['読む', 'よむ', '詠む', '訓む', '讀む', 'yomu'],
            "kaku": ['書く', '描く', 'かく', '核', '格', '各', 'kaku'],
            "tukuru": ['つくる', '作る', '創る', '造る', '作', '創', 'tukuru'],
            "tatu": ['立つ', '経つ', 'たつ', '建つ', '断つ', '竜', 'tatu'],
            "kiku": ['聞く', '聴く', '効く', 'きく', '利く', '訊く', 'kiku'],
            "miru": ['見る', 'みる', '観る', '診る', '視る', '見', 'miru'],
            "kiru": ['着る', '切る', 'きる', '斬る', '伐る', '剪る', 'kiru'],
            "iu": ['いう', '言う', '謂う', '意宇', '居う', '井生', 'iu'],
            "kaeru": ['変える', '買える', '帰る', 'かえる', '替える', '換える', 'kaeru'],
            "toru": ['取る', 'とる', '撮る', '摂る', '採る', '執る', 'toru'],
            "hakaru": ['図る', '測る', 'はかる', '計る', '量る', '諮る', 'hakaru'],
            "osameru": ['治める', '収める', '納める', 'おさめる', '修める', 'お覚める', 'osameru']
        }

        candidates = conversion_map.get(reading, [reading])  # デフォルトはそのまま

        # Dict形式に変換
        result = []
        for candidate in candidates[:max_candidates]:
            result.append({"candidate": candidate})

        return result

def test_mozc_client():
    """テスト用関数（scoreなし版）"""
    client = MozcClient()

    # 履歴データから代表的なテストケースを選択
    test_cases = [
        ("kawaru", "時代が"),
        ("tooru", "道を"),
        ("tomaru", "駅に"),
        ("tomeru", "車を"),
        ("omou", "そう"),
        ("tukau", "道具を"),
        ("kiiroi", ""),
        ("aoi", "空が"),
        ("akai", "花が"),
        ("kuroi", "髪が"),
        ("shiroi", "雲が"),
        ("yasui", "値段が"),
        ("kirai", ""),
        ("suki", ""),
        ("yoi", "天気が"),
        ("warui", "結果が"),
        ("tadashii", "答えが"),
        ("sukunai", "時間が"),
        ("ooi", "人が"),
        ("wakai", "")
    ]

    for reading, context in test_cases:
        print(f"\n読み: {reading}")
        print(f"文脈: {context}")
        candidates = client.get_conversion_candidates(reading, context)

        for i, candidate in enumerate(candidates, 1):
            print(f"  {i}. {candidate['candidate']}")

if __name__ == "__main__":
    test_mozc_client()
