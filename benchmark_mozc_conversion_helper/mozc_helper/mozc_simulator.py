#!/usr/bin/env python3
"""
変換候補をシミュレートするクライアント（Mozcの動作をシミュレート）
実際のMozc履歴データに基づく127パターン
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
            候補のリスト [{"candidate": "候補", "score": "スコア"}, ...]
        """
        return self._simulate_conversion(reading, max_candidates)

    def _simulate_conversion(self, reading: str, max_candidates: int) -> List[Dict[str, str]]:
        """
        変換候補をシミュレート（実際のMozc履歴データベース 127パターン）
        """

        # 実際のMozc履歴に基づく変換候補データベース
        conversion_map = {
            "kawaru": [{'candidate': '変わる', 'score': '1.0'}, {'candidate': '代わる', 'score': '0.8'}, {'candidate': 'かわる', 'score': '0.6'}, {'candidate': '替わる', 'score': '0.4'}, {'candidate': '変る', 'score': '0.2'}, {'candidate': '替る', 'score': '0.0'}, {'candidate': 'kawaru', 'score': '0.1'}],
            "tooru": [{'candidate': '通る', 'score': '1.0'}, {'candidate': 'とおる', 'score': '0.8'}, {'candidate': '透る', 'score': '0.6'}, {'candidate': '達', 'score': '0.4'}, {'candidate': '徹', 'score': '0.2'}, {'candidate': '理', 'score': '0.0'}, {'candidate': 'tooru', 'score': '0.1'}],
            "tomaru": [{'candidate': '止まる', 'score': '1.0'}, {'candidate': '泊まる', 'score': '0.8'}, {'candidate': 'とまる', 'score': '0.6'}, {'candidate': '留まる', 'score': '0.4'}, {'candidate': '停まる', 'score': '0.2'}, {'candidate': '泊る', 'score': '0.0'}, {'candidate': 'tomaru', 'score': '0.1'}],
            "tomeru": [{'candidate': '止める', 'score': '1.0'}, {'candidate': 'とめる', 'score': '0.8'}, {'candidate': '停める', 'score': '0.6'}, {'candidate': '止', 'score': '0.4'}, {'candidate': '留める', 'score': '0.2'}, {'candidate': '泊める', 'score': '0.0'}, {'candidate': 'tomeru', 'score': '0.1'}],
            "omou": [{'candidate': '思う', 'score': '1.0'}, {'candidate': 'おもう', 'score': '0.8'}, {'candidate': '想う', 'score': '0.6'}, {'candidate': '謂う', 'score': '0.4'}, {'candidate': '重う', 'score': '0.2'}, {'candidate': '懐う', 'score': '0.0'}, {'candidate': 'omou', 'score': '0.1'}],
            "tukau": [{'candidate': '使う', 'score': '1.0'}, {'candidate': 'つかう', 'score': '0.8'}, {'candidate': '遣う', 'score': '0.6'}, {'candidate': '付かう', 'score': '0.4'}, {'candidate': '着かう', 'score': '0.2'}, {'candidate': '点かう', 'score': '0.0'}, {'candidate': 'tukau', 'score': '0.1'}],
            "kiiroi": [{'candidate': '黄色い', 'score': '1.0'}, {'candidate': 'きいろい', 'score': '0.8'}, {'candidate': '黄いろい', 'score': '0.6'}, {'candidate': 'kiiroi', 'score': '0.1'}],
            "aoi": [{'candidate': '青い', 'score': '1.0'}, {'candidate': 'あおい', 'score': '0.8'}, {'candidate': '蒼い', 'score': '0.6'}, {'candidate': '蒼', 'score': '0.4'}, {'candidate': '蒼井', 'score': '0.2'}, {'candidate': '碧い', 'score': '0.0'}, {'candidate': 'aoi', 'score': '0.1'}],
            "akai": [{'candidate': '赤い', 'score': '1.0'}, {'candidate': '紅い', 'score': '0.8'}, {'candidate': 'あかい', 'score': '0.6'}, {'candidate': '朱い', 'score': '0.4'}, {'candidate': '明い', 'score': '0.2'}, {'candidate': '紅井', 'score': '0.0'}, {'candidate': 'akai', 'score': '0.1'}],
            "kuroi": [{'candidate': 'くろい', 'score': '1.0'}, {'candidate': 'kuroi', 'score': '0.1'}],
            "shiroi": [{'candidate': '白い', 'score': '1.0'}, {'candidate': 'しろい', 'score': '0.8'}, {'candidate': '城井', 'score': '0.6'}, {'candidate': '皓', 'score': '0.4'}, {'candidate': '知ろい', 'score': '0.2'}, {'candidate': '私ロイ', 'score': '0.0'}, {'candidate': 'shiroi', 'score': '0.1'}],
            "yasui": [{'candidate': '安い', 'score': '1.0'}, {'candidate': 'やすい', 'score': '0.8'}, {'candidate': '易い', 'score': '0.6'}, {'candidate': '易居', 'score': '0.4'}, {'candidate': '安居', 'score': '0.2'}, {'candidate': '矢水', 'score': '0.0'}, {'candidate': 'yasui', 'score': '0.1'}],
            "kirai": [{'candidate': 'きらい', 'score': '1.0'}, {'candidate': '喜来', 'score': '0.8'}, {'candidate': '起来', 'score': '0.6'}, {'candidate': '切らい', 'score': '0.4'}, {'candidate': '生ライ', 'score': '0.2'}, {'candidate': '機雷', 'score': '0.0'}, {'candidate': 'kirai', 'score': '0.1'}],
            "suki": [{'candidate': 'すき', 'score': '1.0'}, {'candidate': '空き', 'score': '0.8'}, {'candidate': '数奇', 'score': '0.6'}, {'candidate': '漉き', 'score': '0.4'}, {'candidate': '隙', 'score': '0.2'}, {'candidate': '鋤', 'score': '0.0'}, {'candidate': 'suki', 'score': '0.1'}],
            "yoi": [{'candidate': 'よい', 'score': '1.0'}, {'candidate': '好い', 'score': '0.8'}, {'candidate': '善い', 'score': '0.6'}, {'candidate': '佳い', 'score': '0.4'}, {'candidate': '宵', 'score': '0.2'}, {'candidate': '酔', 'score': '0.0'}, {'candidate': 'yoi', 'score': '0.1'}],
            "warui": [{'candidate': '悪い', 'score': '1.0'}, {'candidate': 'わるい', 'score': '0.8'}, {'candidate': '惡い', 'score': '0.6'}, {'candidate': '兇い', 'score': '0.4'}, {'candidate': '悪井', 'score': '0.2'}, {'candidate': '話類', 'score': '0.0'}, {'candidate': 'warui', 'score': '0.1'}],
            "tadashii": [{'candidate': '正しい', 'score': '1.0'}, {'candidate': 'ただしい', 'score': '0.8'}, {'candidate': 'tadashii', 'score': '0.1'}],
            "sukunai": [{'candidate': 'すくない', 'score': '1.0'}, {'candidate': '少い', 'score': '0.8'}, {'candidate': '尠い', 'score': '0.6'}, {'candidate': '空くない', 'score': '0.4'}, {'candidate': 'す区内', 'score': '0.2'}, {'candidate': 'す宮内', 'score': '0.0'}, {'candidate': 'sukunai', 'score': '0.1'}],
            "ooi": [{'candidate': '多い', 'score': '1.0'}, {'candidate': 'おおい', 'score': '0.8'}, {'candidate': 'お負い', 'score': '0.6'}, {'candidate': 'お追い', 'score': '0.4'}, {'candidate': 'お老い', 'score': '0.2'}, {'candidate': '覆い', 'score': '0.0'}, {'candidate': 'ooi', 'score': '0.1'}],
            "wakai": [{'candidate': '若い', 'score': '1.0'}, {'candidate': 'わかい', 'score': '0.8'}, {'candidate': '沸かい', 'score': '0.6'}, {'candidate': '若生', 'score': '0.4'}, {'candidate': '湧かい', 'score': '0.2'}, {'candidate': '話界', 'score': '0.0'}, {'candidate': 'wakai', 'score': '0.1'}],
            "furui": [{'candidate': 'ふるい', 'score': '1.0'}, {'candidate': '旧い', 'score': '0.8'}, {'candidate': '振るい', 'score': '0.6'}, {'candidate': '奮い', 'score': '0.4'}, {'candidate': '震い', 'score': '0.2'}, {'candidate': '篩い', 'score': '0.0'}, {'candidate': 'furui', 'score': '0.1'}],
            "atarashii": [{'candidate': '新しい', 'score': '1.0'}, {'candidate': 'あたらしい', 'score': '0.8'}, {'candidate': '阿多らしい', 'score': '0.6'}, {'candidate': 'atarashii', 'score': '0.1'}],
            "chiisai": [{'candidate': '小さい', 'score': '1.0'}, {'candidate': 'ちいさい', 'score': '0.8'}, {'candidate': 'chiisai', 'score': '0.1'}],
            "ookii": [{'candidate': 'おおきい', 'score': '1.0'}, {'candidate': 'お起きい', 'score': '0.8'}, {'candidate': '大忌諱', 'score': '0.6'}, {'candidate': '大貴意', 'score': '0.4'}, {'candidate': '大きい', 'score': '0.2'}, {'candidate': 'ookii', 'score': '0.1'}],
            "kanashii": [{'candidate': '悲しい', 'score': '1.0'}, {'candidate': '哀しい', 'score': '0.8'}, {'candidate': 'かなしい', 'score': '0.6'}, {'candidate': 'kanashii', 'score': '0.1'}],
            "ureshii": [{'candidate': '嬉しい', 'score': '1.0'}, {'candidate': 'うれしい', 'score': '0.8'}, {'candidate': 'ureshii', 'score': '0.1'}],
            "tanoshii": [{'candidate': '楽しい', 'score': '1.0'}, {'candidate': 'たのしい', 'score': '0.8'}, {'candidate': '愉しい', 'score': '0.6'}, {'candidate': '樂しい', 'score': '0.4'}, {'candidate': 'tanoshii', 'score': '0.1'}],
            "isogashii": [{'candidate': '忙しい', 'score': '1.0'}, {'candidate': 'いそがしい', 'score': '0.8'}, {'candidate': 'いそがし胃', 'score': '0.6'}, {'candidate': 'いそがし意', 'score': '0.4'}, {'candidate': 'isogashii', 'score': '0.1'}],
            "yasashii": [{'candidate': '優しい', 'score': '1.0'}, {'candidate': 'やさしい', 'score': '0.8'}, {'candidate': '易しい', 'score': '0.6'}, {'candidate': 'yasashii', 'score': '0.1'}],
            "muzukashii": [{'candidate': '難しい', 'score': '1.0'}, {'candidate': 'むずかしい', 'score': '0.8'}, {'candidate': 'むづかしい', 'score': '0.6'}, {'candidate': 'muzukashii', 'score': '0.1'}],
            "tooi": [{'candidate': '遠い', 'score': '1.0'}, {'candidate': 'とおい', 'score': '0.8'}, {'candidate': '十威', 'score': '0.6'}, {'candidate': '悠い', 'score': '0.4'}, {'candidate': '遠井', 'score': '0.2'}, {'candidate': 'tooi', 'score': '0.1'}],
            "chikai": [{'candidate': '近い', 'score': '1.0'}, {'candidate': 'ちかい', 'score': '0.8'}, {'candidate': '誓', 'score': '0.6'}, {'candidate': '血界', 'score': '0.4'}, {'candidate': '智会', 'score': '0.2'}, {'candidate': '盟', 'score': '0.0'}, {'candidate': 'chikai', 'score': '0.1'}],
            "kurai": [{'candidate': 'くらい', 'score': '1.0'}, {'candidate': '暗い', 'score': '0.8'}, {'candidate': '喰らい', 'score': '0.6'}, {'candidate': '食らい', 'score': '0.4'}, {'candidate': '昏い', 'score': '0.2'}, {'candidate': '儚い', 'score': '0.0'}, {'candidate': 'kurai', 'score': '0.1'}],
            "akarui": [{'candidate': '明るい', 'score': '1.0'}, {'candidate': 'あかるい', 'score': '0.8'}, {'candidate': 'akarui', 'score': '0.1'}],
            "kitanai": [{'candidate': '汚い', 'score': '1.0'}, {'candidate': 'きたない', 'score': '0.8'}, {'candidate': '穢い', 'score': '0.6'}, {'candidate': 'kitanai', 'score': '0.1'}],
            "utukushii": [{'candidate': '美しい', 'score': '1.0'}, {'candidate': 'うつくしい', 'score': '0.8'}, {'candidate': '鬱くしい', 'score': '0.6'}, {'candidate': 'utukushii', 'score': '0.1'}],
            "samui": [{'candidate': '寒い', 'score': '1.0'}, {'candidate': 'さむい', 'score': '0.8'}, {'candidate': 'samui', 'score': '0.1'}],
            "tumetai": [{'candidate': '冷たい', 'score': '1.0'}, {'candidate': 'つめたい', 'score': '0.8'}, {'candidate': '詰めたい', 'score': '0.6'}, {'candidate': '積めたい', 'score': '0.4'}, {'candidate': '摘めたい', 'score': '0.2'}, {'candidate': 'tumetai', 'score': '0.1'}],
            "atatakai": [{'candidate': '温かい', 'score': '1.0'}, {'candidate': '暖かい', 'score': '0.8'}, {'candidate': 'あたたかい', 'score': '0.6'}, {'candidate': 'atatakai', 'score': '0.1'}],
            "usui": [{'candidate': '薄い', 'score': '1.0'}, {'candidate': 'うすい', 'score': '0.8'}, {'candidate': '笛吹', 'score': '0.6'}, {'candidate': '羽水', 'score': '0.4'}, {'candidate': '雨水', 'score': '0.2'}, {'candidate': '臼井', 'score': '0.0'}, {'candidate': 'usui', 'score': '0.1'}],
            "atui": [{'candidate': '暑い', 'score': '1.0'}, {'candidate': '熱い', 'score': '0.8'}, {'candidate': '厚い', 'score': '0.6'}, {'candidate': 'あつい', 'score': '0.4'}, {'candidate': '篤い', 'score': '0.2'}, {'candidate': 'atui', 'score': '0.1'}],
            "asai": [{'candidate': '浅い', 'score': '1.0'}, {'candidate': 'あさい', 'score': '0.8'}, {'candidate': '朝生', 'score': '0.6'}, {'candidate': '浅生', 'score': '0.4'}, {'candidate': '浅井', 'score': '0.2'}, {'candidate': '朝井', 'score': '0.0'}, {'candidate': 'asai', 'score': '0.1'}],
            "fukai": [{'candidate': 'ふかい', 'score': '1.0'}, {'candidate': '不快', 'score': '0.8'}, {'candidate': '深井', 'score': '0.6'}, {'candidate': '腐海', 'score': '0.4'}, {'candidate': '付会', 'score': '0.2'}, {'candidate': '府会', 'score': '0.0'}, {'candidate': 'fukai', 'score': '0.1'}],
            "semai": [{'candidate': '狭い', 'score': '1.0'}, {'candidate': 'せまい', 'score': '0.8'}, {'candidate': '狹い', 'score': '0.6'}, {'candidate': '施米', 'score': '0.4'}, {'candidate': 'semai', 'score': '0.1'}],
            "hiroi": [{'candidate': '広い', 'score': '1.0'}, {'candidate': '拾い', 'score': '0.8'}, {'candidate': 'ひろい', 'score': '0.6'}, {'candidate': '寛い', 'score': '0.4'}, {'candidate': '廣井', 'score': '0.2'}, {'candidate': '廣い', 'score': '0.0'}, {'candidate': 'hiroi', 'score': '0.1'}],
            "yowai": [{'candidate': 'よわい', 'score': '1.0'}, {'candidate': '歯い', 'score': '0.8'}, {'candidate': '弱井', 'score': '0.6'}, {'candidate': '弱い', 'score': '0.4'}, {'candidate': 'yowai', 'score': '0.1'}],
            "tuyoi": [{'candidate': '強い', 'score': '1.0'}, {'candidate': 'つよい', 'score': '0.8'}, {'candidate': '勁い', 'score': '0.6'}, {'candidate': 'つ酔い', 'score': '0.4'}, {'candidate': '勍', 'score': '0.2'}, {'candidate': 'tuyoi', 'score': '0.1'}],
            "karui": [{'candidate': '軽い', 'score': '1.0'}, {'candidate': 'かるい', 'score': '0.8'}, {'candidate': '軽井', 'score': '0.6'}, {'candidate': '過類', 'score': '0.4'}, {'candidate': '可類', 'score': '0.2'}, {'candidate': '過塁', 'score': '0.0'}, {'candidate': 'karui', 'score': '0.1'}],
            "omoi": [{'candidate': '重い', 'score': '1.0'}, {'candidate': 'おもい', 'score': '0.8'}, {'candidate': '懐い', 'score': '0.6'}, {'candidate': '思井', 'score': '0.4'}, {'candidate': '謂い', 'score': '0.2'}, {'candidate': '思い', 'score': '0.0'}, {'candidate': 'omoi', 'score': '0.1'}],
            "osoi": [{'candidate': '遅い', 'score': '1.0'}, {'candidate': 'おそい', 'score': '0.8'}, {'candidate': '襲い', 'score': '0.6'}, {'candidate': 'お沿い', 'score': '0.4'}, {'candidate': '晩い', 'score': '0.2'}, {'candidate': 'お添い', 'score': '0.0'}, {'candidate': 'osoi', 'score': '0.1'}],
            "hayai": [{'candidate': '早い', 'score': '1.0'}, {'candidate': 'はやい', 'score': '0.8'}, {'candidate': '疾い', 'score': '0.6'}, {'candidate': '捷い', 'score': '0.4'}, {'candidate': '疾医', 'score': '0.2'}, {'candidate': '速い', 'score': '0.0'}, {'candidate': 'hayai', 'score': '0.1'}],
            "mijikai": [{'candidate': '短い', 'score': '1.0'}, {'candidate': 'みじかい', 'score': '0.8'}, {'candidate': '身近い', 'score': '0.6'}, {'candidate': '未自戒', 'score': '0.4'}, {'candidate': 'み次回', 'score': '0.2'}, {'candidate': '未自壊', 'score': '0.0'}, {'candidate': 'mijikai', 'score': '0.1'}],
            "nagai": [{'candidate': '長い', 'score': '1.0'}, {'candidate': 'ながい', 'score': '0.8'}, {'candidate': '長', 'score': '0.6'}, {'candidate': '镸', 'score': '0.4'}, {'candidate': '永い', 'score': '0.2'}, {'candidate': '永井', 'score': '0.0'}, {'candidate': 'nagai', 'score': '0.1'}],
            "hikui": [{'candidate': '低い', 'score': '1.0'}, {'candidate': 'ひくい', 'score': '0.8'}, {'candidate': '非杭', 'score': '0.6'}, {'candidate': '被杭', 'score': '0.4'}, {'candidate': '非悔い', 'score': '0.2'}, {'candidate': '被悔い', 'score': '0.0'}, {'candidate': 'hikui', 'score': '0.1'}],
            "takai": [{'candidate': '高い', 'score': '1.0'}, {'candidate': 'たかい', 'score': '0.8'}, {'candidate': '高', 'score': '0.6'}, {'candidate': '高価い', 'score': '0.4'}, {'candidate': '喬', 'score': '0.2'}, {'candidate': '鷹居', 'score': '0.0'}, {'candidate': 'takai', 'score': '0.1'}],
            "deru": [{'candidate': '出る', 'score': '1.0'}, {'candidate': 'でる', 'score': '0.8'}, {'candidate': 'deru', 'score': '0.1'}],
            "hairu": [{'candidate': '入る', 'score': '1.0'}, {'candidate': 'はいる', 'score': '0.8'}, {'candidate': '這入る', 'score': '0.6'}, {'candidate': '配流', 'score': '0.4'}, {'candidate': 'hairu', 'score': '0.1'}],
            "sagaru": [{'candidate': 'さがる', 'score': '1.0'}, {'candidate': 'sagaru', 'score': '0.1'}],
            "agaru": [{'candidate': 'あがる', 'score': '1.0'}, {'candidate': '挙がる', 'score': '0.8'}, {'candidate': '揚がる', 'score': '0.6'}, {'candidate': '騰がる', 'score': '0.4'}, {'candidate': '和了る', 'score': '0.2'}, {'candidate': 'agaru', 'score': '0.1'}],
            "modoru": [{'candidate': '戻る', 'score': '1.0'}, {'candidate': 'もどる', 'score': '0.8'}, {'candidate': 'modoru', 'score': '0.1'}],
            "susumu": [{'candidate': '進む', 'score': '1.0'}, {'candidate': 'すすむ', 'score': '0.8'}, {'candidate': '攻', 'score': '0.6'}, {'candidate': '勧', 'score': '0.4'}, {'candidate': '生', 'score': '0.2'}, {'candidate': '前', 'score': '0.0'}, {'candidate': 'susumu', 'score': '0.1'}],
            "tuduku": [{'candidate': 'つづく', 'score': '1.0'}, {'candidate': '都竹', 'score': '0.8'}, {'candidate': '通津区', 'score': '0.6'}, {'candidate': '続く', 'score': '0.4'}, {'candidate': 'tuduku', 'score': '0.1'}],
            "owaru": [{'candidate': '終わる', 'score': '1.0'}, {'candidate': '終る', 'score': '0.8'}, {'candidate': 'おわる', 'score': '0.6'}, {'candidate': 'お悪', 'score': '0.4'}, {'candidate': '畢る', 'score': '0.2'}, {'candidate': 'お割る', 'score': '0.0'}, {'candidate': 'owaru', 'score': '0.1'}],
            "hajimeru": [{'candidate': '始める', 'score': '1.0'}, {'candidate': 'はじめる', 'score': '0.8'}, {'candidate': '初める', 'score': '0.6'}, {'candidate': '創める', 'score': '0.4'}, {'candidate': 'hajimeru', 'score': '0.1'}],
            "erabu": [{'candidate': '選ぶ', 'score': '1.0'}, {'candidate': 'えらぶ', 'score': '0.8'}, {'candidate': '撰ぶ', 'score': '0.6'}, {'candidate': '択ぶ', 'score': '0.4'}, {'candidate': 'えら部', 'score': '0.2'}, {'candidate': 'erabu', 'score': '0.1'}],
            "kimeru": [{'candidate': '決める', 'score': '1.0'}, {'candidate': '極める', 'score': '0.8'}, {'candidate': 'きめる', 'score': '0.6'}, {'candidate': '决める', 'score': '0.4'}, {'candidate': '生メル', 'score': '0.2'}, {'candidate': '既メル', 'score': '0.0'}, {'candidate': 'kimeru', 'score': '0.1'}],
            "kanjiru": [{'candidate': '感じる', 'score': '1.0'}, {'candidate': 'かんじる', 'score': '0.8'}, {'candidate': '観じる', 'score': '0.6'}, {'candidate': '缶汁', 'score': '0.4'}, {'candidate': 'kanjiru', 'score': '0.1'}],
            "kangaeru": [{'candidate': '考える', 'score': '1.0'}, {'candidate': 'かんがえる', 'score': '0.8'}, {'candidate': 'kangaeru', 'score': '0.1'}],
            "wakaru": [{'candidate': 'わかる', 'score': '1.0'}, {'candidate': '分かる', 'score': '0.8'}, {'candidate': '判る', 'score': '0.6'}, {'candidate': '解る', 'score': '0.4'}, {'candidate': '分る', 'score': '0.2'}, {'candidate': '解かる', 'score': '0.0'}, {'candidate': 'wakaru', 'score': '0.1'}],
            "shiru": [{'candidate': '知る', 'score': '1.0'}, {'candidate': 'しる', 'score': '0.8'}, {'candidate': '識る', 'score': '0.6'}, {'candidate': '著', 'score': '0.4'}, {'candidate': '私る', 'score': '0.2'}, {'candidate': '記', 'score': '0.0'}, {'candidate': 'shiru', 'score': '0.1'}],
            "wasureru": [{'candidate': '忘れる', 'score': '1.0'}, {'candidate': 'わすれる', 'score': '0.8'}, {'candidate': 'wasureru', 'score': '0.1'}],
            "oboeru": [{'candidate': '覚える', 'score': '1.0'}, {'candidate': 'おぼえる', 'score': '0.8'}, {'candidate': '憶える', 'score': '0.6'}, {'candidate': 'oboeru', 'score': '0.1'}],
            "narau": [{'candidate': '習う', 'score': '1.0'}, {'candidate': '倣う', 'score': '0.8'}, {'candidate': 'ならう', 'score': '0.6'}, {'candidate': '鳴らう', 'score': '0.4'}, {'candidate': '成らう', 'score': '0.2'}, {'candidate': '生らう', 'score': '0.0'}, {'candidate': 'narau', 'score': '0.1'}],
            "oshieru": [{'candidate': '教える', 'score': '1.0'}, {'candidate': 'おしえる', 'score': '0.8'}, {'candidate': '押し得る', 'score': '0.6'}, {'candidate': '訓える', 'score': '0.4'}, {'candidate': '推し得る', 'score': '0.2'}, {'candidate': 'おし得る', 'score': '0.0'}, {'candidate': 'oshieru', 'score': '0.1'}],
            "hanasu": [{'candidate': '話す', 'score': '1.0'}, {'candidate': '離す', 'score': '0.8'}, {'candidate': '放す', 'score': '0.6'}, {'candidate': 'はなす', 'score': '0.4'}, {'candidate': 'hanasu', 'score': '0.1'}],
            "au": [{'candidate': '合う', 'score': '1.0'}, {'candidate': '会う', 'score': '0.8'}, {'candidate': 'あう', 'score': '0.6'}, {'candidate': '遭う', 'score': '0.4'}, {'candidate': '逢う', 'score': '0.2'}, {'candidate': '遇う', 'score': '0.0'}, {'candidate': 'au', 'score': '0.1'}],
            "wakareru": [{'candidate': '分かれる', 'score': '1.0'}, {'candidate': '別れる', 'score': '0.8'}, {'candidate': 'わかれる', 'score': '0.6'}, {'candidate': '沸かれる', 'score': '0.4'}, {'candidate': '湧かれる', 'score': '0.2'}, {'candidate': '涌かれる', 'score': '0.0'}, {'candidate': 'wakareru', 'score': '0.1'}],
            "mukaeru": [{'candidate': '迎える', 'score': '1.0'}, {'candidate': 'むかえる', 'score': '0.8'}, {'candidate': '向かえる', 'score': '0.6'}, {'candidate': '逢える', 'score': '0.4'}, {'candidate': '無カエル', 'score': '0.2'}, {'candidate': '無蛙', 'score': '0.0'}, {'candidate': 'mukaeru', 'score': '0.1'}],
            "okuru": [{'candidate': '送る', 'score': '1.0'}, {'candidate': '贈る', 'score': '0.8'}, {'candidate': 'おくる', 'score': '0.6'}, {'candidate': '御クル', 'score': '0.4'}, {'candidate': 'お佝僂', 'score': '0.2'}, {'candidate': '雄クル', 'score': '0.0'}, {'candidate': 'okuru', 'score': '0.1'}],
            "ukeru": [{'candidate': '受ける', 'score': '1.0'}, {'candidate': 'うける', 'score': '0.8'}, {'candidate': '請ける', 'score': '0.6'}, {'candidate': '承ける', 'score': '0.4'}, {'candidate': '享ける', 'score': '0.2'}, {'candidate': 'ukeru', 'score': '0.1'}],
            "ataeru": [{'candidate': 'あたえる', 'score': '1.0'}, {'candidate': '與える', 'score': '0.8'}, {'candidate': 'ataeru', 'score': '0.1'}],
            "eru": [{'candidate': '得る', 'score': '1.0'}, {'candidate': 'える', 'score': '0.8'}, {'candidate': '獲る', 'score': '0.6'}, {'candidate': 'eru', 'score': '0.1'}],
            "ushinau": [{'candidate': '失う', 'score': '1.0'}, {'candidate': '喪う', 'score': '0.8'}, {'candidate': 'うしなう', 'score': '0.6'}, {'candidate': '牛なう', 'score': '0.4'}, {'candidate': '武氏なう', 'score': '0.2'}, {'candidate': 'ushinau', 'score': '0.1'}],
            "mitukeru": [{'candidate': '見つける', 'score': '1.0'}, {'candidate': 'みつける', 'score': '0.8'}, {'candidate': '見付ける', 'score': '0.6'}, {'candidate': 'mitukeru', 'score': '0.1'}],
            "sagasu": [{'candidate': '探す', 'score': '1.0'}, {'candidate': 'さがす', 'score': '0.8'}, {'candidate': '捜す', 'score': '0.6'}, {'candidate': 'sagasu', 'score': '0.1'}],
            "utagau": [{'candidate': '疑う', 'score': '1.0'}, {'candidate': 'うたがう', 'score': '0.8'}, {'candidate': 'utagau', 'score': '0.1'}],
            "shinjiru": [{'candidate': '信じる', 'score': '1.0'}, {'candidate': 'しんじる', 'score': '0.8'}, {'candidate': 'shinjiru', 'score': '0.1'}],
            "aisuru": [{'candidate': '愛する', 'score': '1.0'}, {'candidate': '会いする', 'score': '0.8'}, {'candidate': 'あいする', 'score': '0.6'}, {'candidate': '合いする', 'score': '0.4'}, {'candidate': '逢いする', 'score': '0.2'}, {'candidate': '遭いする', 'score': '0.0'}, {'candidate': 'aisuru', 'score': '0.1'}],
            "osoreru": [{'candidate': '恐れる', 'score': '1.0'}, {'candidate': '恐る', 'score': '0.8'}, {'candidate': '怖れる', 'score': '0.6'}, {'candidate': 'おそれる', 'score': '0.4'}, {'candidate': '畏れる', 'score': '0.2'}, {'candidate': 'お逸れる', 'score': '0.0'}, {'candidate': 'osoreru', 'score': '0.1'}],
            "odoroku": [{'candidate': '驚く', 'score': '1.0'}, {'candidate': 'おどろく', 'score': '0.8'}, {'candidate': '驚', 'score': '0.6'}, {'candidate': '踊ろく', 'score': '0.4'}, {'candidate': '小土呂区', 'score': '0.2'}, {'candidate': 'odoroku', 'score': '0.1'}],
            "kanashimu": [{'candidate': '悲しむ', 'score': '1.0'}, {'candidate': '哀しむ', 'score': '0.8'}, {'candidate': 'かなしむ', 'score': '0.6'}, {'candidate': 'kanashimu', 'score': '0.1'}],
            "yorokobu": [{'candidate': '喜ぶ', 'score': '1.0'}, {'candidate': 'よろこぶ', 'score': '0.8'}, {'candidate': '悦ぶ', 'score': '0.6'}, {'candidate': '歓ぶ', 'score': '0.4'}, {'candidate': '慶ぶ', 'score': '0.2'}, {'candidate': 'yorokobu', 'score': '0.1'}],
            "okoru": [{'candidate': '起こる', 'score': '1.0'}, {'candidate': '怒る', 'score': '0.8'}, {'candidate': 'おこる', 'score': '0.6'}, {'candidate': '起る', 'score': '0.4'}, {'candidate': '興る', 'score': '0.2'}, {'candidate': '煽る', 'score': '0.0'}, {'candidate': 'okoru', 'score': '0.1'}],
            "naku": [{'candidate': 'なく', 'score': '1.0'}, {'candidate': '無く', 'score': '0.8'}, {'candidate': '泣く', 'score': '0.6'}, {'candidate': '（泣）', 'score': '0.4'}, {'candidate': '(泣)', 'score': '0.2'}, {'candidate': '鳴く', 'score': '0.0'}, {'candidate': 'naku', 'score': '0.1'}],
            "warau": [{'candidate': '笑う', 'score': '1.0'}, {'candidate': 'わらう', 'score': '0.8'}, {'candidate': '嗤う', 'score': '0.6'}, {'candidate': '嘲笑う', 'score': '0.4'}, {'candidate': '微笑う', 'score': '0.2'}, {'candidate': '咲う', 'score': '0.0'}, {'candidate': 'warau', 'score': '0.1'}],
            "ikiru": [{'candidate': '生きる', 'score': '1.0'}, {'candidate': '活きる', 'score': '0.8'}, {'candidate': 'いきる', 'score': '0.6'}, {'candidate': '生', 'score': '0.4'}, {'candidate': '異キル', 'score': '0.2'}, {'candidate': 'ikiru', 'score': '0.1'}],
            "shinu": [{'candidate': '死ぬ', 'score': '1.0'}, {'candidate': 'しぬ', 'score': '0.8'}, {'candidate': '私ぬ', 'score': '0.6'}, {'candidate': 'shinu', 'score': '0.1'}],
            "suwaru": [{'candidate': '座る', 'score': '1.0'}, {'candidate': 'すわる', 'score': '0.8'}, {'candidate': '坐る', 'score': '0.6'}, {'candidate': '据わる', 'score': '0.4'}, {'candidate': 'す悪', 'score': '0.2'}, {'candidate': 'suwaru', 'score': '0.1'}],
            "okiru": [{'candidate': '起きる', 'score': '1.0'}, {'candidate': 'おきる', 'score': '0.8'}, {'candidate': 'お切る', 'score': '0.6'}, {'candidate': '熾きる', 'score': '0.4'}, {'candidate': '御キル', 'score': '0.2'}, {'candidate': 'お斬る', 'score': '0.0'}, {'candidate': 'okiru', 'score': '0.1'}],
            "neru": [{'candidate': '寝る', 'score': '1.0'}, {'candidate': 'ねる', 'score': '0.8'}, {'candidate': '練る', 'score': '0.6'}, {'candidate': '錬る', 'score': '0.4'}, {'candidate': '煉る', 'score': '0.2'}, {'candidate': 'neru', 'score': '0.1'}],
            "nomu": [{'candidate': '飲む', 'score': '1.0'}, {'candidate': 'のむ', 'score': '0.8'}, {'candidate': '呑む', 'score': '0.6'}, {'candidate': 'nomu', 'score': '0.1'}],
            "kuu": [{'candidate': '食う', 'score': '1.0'}, {'candidate': 'くう', 'score': '0.8'}, {'candidate': '喰う', 'score': '0.6'}, {'candidate': 'く雨', 'score': '0.4'}, {'candidate': '区雨', 'score': '0.2'}, {'candidate': '倥', 'score': '0.0'}, {'candidate': 'kuu', 'score': '0.1'}],
            "kau": [{'candidate': '買う', 'score': '1.0'}, {'candidate': '飼う', 'score': '0.8'}, {'candidate': 'かう', 'score': '0.6'}, {'candidate': '交う', 'score': '0.4'}, {'candidate': '支う', 'score': '0.2'}, {'candidate': '過兎', 'score': '0.0'}, {'candidate': 'kau', 'score': '0.1'}],
            "uru": [{'candidate': '売る', 'score': '1.0'}, {'candidate': 'うる', 'score': '0.8'}, {'candidate': '賣る', 'score': '0.6'}, {'candidate': '粳', 'score': '0.4'}, {'candidate': '得る', 'score': '0.2'}, {'candidate': '憂る', 'score': '0.0'}, {'candidate': 'uru', 'score': '0.1'}],
            "shimeru": [{'candidate': '占める', 'score': '1.0'}, {'candidate': '締める', 'score': '0.8'}, {'candidate': '閉める', 'score': '0.6'}, {'candidate': 'しめる', 'score': '0.4'}, {'candidate': '絞める', 'score': '0.2'}, {'candidate': '湿る', 'score': '0.0'}, {'candidate': 'shimeru', 'score': '0.1'}],
            "akeru": [{'candidate': '開ける', 'score': '1.0'}, {'candidate': 'あける', 'score': '0.8'}, {'candidate': '空ける', 'score': '0.6'}, {'candidate': '明ける', 'score': '0.4'}, {'candidate': '開る', 'score': '0.2'}, {'candidate': 'akeru', 'score': '0.1'}],
            "oku": [{'candidate': '置く', 'score': '1.0'}, {'candidate': 'おく', 'score': '0.8'}, {'candidate': '於く', 'score': '0.6'}, {'candidate': '措く', 'score': '0.4'}, {'candidate': '奥', 'score': '0.2'}, {'candidate': '億', 'score': '0.0'}, {'candidate': 'oku', 'score': '0.1'}],
            "motu": [{'candidate': '持つ', 'score': '1.0'}, {'candidate': 'もつ', 'score': '0.8'}, {'candidate': '保つ', 'score': '0.6'}, {'candidate': '沒', 'score': '0.4'}, {'candidate': '没', 'score': '0.2'}, {'candidate': '縺', 'score': '0.0'}, {'candidate': 'motu', 'score': '0.1'}],
            "hiku": [{'candidate': 'ひく', 'score': '1.0'}, {'candidate': '惹く', 'score': '0.8'}, {'candidate': '弾く', 'score': '0.6'}, {'candidate': '挽く', 'score': '0.4'}, {'candidate': '牽く', 'score': '0.2'}, {'candidate': '低', 'score': '0.0'}, {'candidate': 'hiku', 'score': '0.1'}],
            "osu": [{'candidate': '押す', 'score': '1.0'}, {'candidate': 'おす', 'score': '0.8'}, {'candidate': '推す', 'score': '0.6'}, {'candidate': '押忍', 'score': '0.4'}, {'candidate': '捺す', 'score': '0.2'}, {'candidate': '雄', 'score': '0.0'}, {'candidate': 'osu', 'score': '0.1'}],
            "utu": [{'candidate': '打つ', 'score': '1.0'}, {'candidate': 'うつ', 'score': '0.8'}, {'candidate': '撃つ', 'score': '0.6'}, {'candidate': '討つ', 'score': '0.4'}, {'candidate': '射つ', 'score': '0.2'}, {'candidate': '伐つ', 'score': '0.0'}, {'candidate': 'utu', 'score': '0.1'}],
            "oyogu": [{'candidate': '泳ぐ', 'score': '1.0'}, {'candidate': 'およぐ', 'score': '0.8'}, {'candidate': '游ぐ', 'score': '0.6'}, {'candidate': '御ヨグ', 'score': '0.4'}, {'candidate': '雄ヨグ', 'score': '0.2'}, {'candidate': 'oyogu', 'score': '0.1'}],
            "tobu": [{'candidate': '飛ぶ', 'score': '1.0'}, {'candidate': 'とぶ', 'score': '0.8'}, {'candidate': '跳ぶ', 'score': '0.6'}, {'candidate': '飛', 'score': '0.4'}, {'candidate': '翔ぶ', 'score': '0.2'}, {'candidate': 'tobu', 'score': '0.1'}],
            "hashiru": [{'candidate': '走る', 'score': '1.0'}, {'candidate': 'はしる', 'score': '0.8'}, {'candidate': '奔る', 'score': '0.6'}, {'candidate': '走', 'score': '0.4'}, {'candidate': '疾走る', 'score': '0.2'}, {'candidate': 'hashiru', 'score': '0.1'}],
            "aruku": [{'candidate': '歩く', 'score': '1.0'}, {'candidate': 'あるく', 'score': '0.8'}, {'candidate': '[全] かたかな', 'score': '0.6'}, {'candidate': '[全] カタカナ', 'score': '0.4'}, {'candidate': 'aruku', 'score': '0.1'}],
            "yomu": [{'candidate': '読む', 'score': '1.0'}, {'candidate': 'よむ', 'score': '0.8'}, {'candidate': '詠む', 'score': '0.6'}, {'candidate': '訓む', 'score': '0.4'}, {'candidate': '讀む', 'score': '0.2'}, {'candidate': 'yomu', 'score': '0.1'}],
            "kaku": [{'candidate': '書く', 'score': '1.0'}, {'candidate': '描く', 'score': '0.8'}, {'candidate': 'かく', 'score': '0.6'}, {'candidate': '核', 'score': '0.4'}, {'candidate': '格', 'score': '0.2'}, {'candidate': '各', 'score': '0.0'}, {'candidate': 'kaku', 'score': '0.1'}],
            "tukuru": [{'candidate': 'つくる', 'score': '1.0'}, {'candidate': '作る', 'score': '0.8'}, {'candidate': '創る', 'score': '0.6'}, {'candidate': '造る', 'score': '0.4'}, {'candidate': '作', 'score': '0.2'}, {'candidate': '創', 'score': '0.0'}, {'candidate': 'tukuru', 'score': '0.1'}],
            "tatu": [{'candidate': '立つ', 'score': '1.0'}, {'candidate': '経つ', 'score': '0.8'}, {'candidate': 'たつ', 'score': '0.6'}, {'candidate': '建つ', 'score': '0.4'}, {'candidate': '断つ', 'score': '0.2'}, {'candidate': '竜', 'score': '0.0'}, {'candidate': 'tatu', 'score': '0.1'}],
            "kiku": [{'candidate': '聞く', 'score': '1.0'}, {'candidate': '聴く', 'score': '0.8'}, {'candidate': '効く', 'score': '0.6'}, {'candidate': 'きく', 'score': '0.4'}, {'candidate': '利く', 'score': '0.2'}, {'candidate': '訊く', 'score': '0.0'}, {'candidate': 'kiku', 'score': '0.1'}],
            "miru": [{'candidate': '見る', 'score': '1.0'}, {'candidate': 'みる', 'score': '0.8'}, {'candidate': '観る', 'score': '0.6'}, {'candidate': '診る', 'score': '0.4'}, {'candidate': '視る', 'score': '0.2'}, {'candidate': '見', 'score': '0.0'}, {'candidate': 'miru', 'score': '0.1'}],
            "kiru": [{'candidate': '着る', 'score': '1.0'}, {'candidate': '切る', 'score': '0.8'}, {'candidate': 'きる', 'score': '0.6'}, {'candidate': '斬る', 'score': '0.4'}, {'candidate': '伐る', 'score': '0.2'}, {'candidate': '剪る', 'score': '0.0'}, {'candidate': 'kiru', 'score': '0.1'}],
            "iu": [{'candidate': 'いう', 'score': '1.0'}, {'candidate': '言う', 'score': '0.8'}, {'candidate': '謂う', 'score': '0.6'}, {'candidate': '意宇', 'score': '0.4'}, {'candidate': '居う', 'score': '0.2'}, {'candidate': '井生', 'score': '0.0'}, {'candidate': 'iu', 'score': '0.1'}],
            "kaeru": [{'candidate': '変える', 'score': '1.0'}, {'candidate': '買える', 'score': '0.8'}, {'candidate': '帰る', 'score': '0.6'}, {'candidate': 'かえる', 'score': '0.4'}, {'candidate': '替える', 'score': '0.2'}, {'candidate': '換える', 'score': '0.0'}, {'candidate': 'kaeru', 'score': '0.1'}],
            "toru": [{'candidate': '取る', 'score': '1.0'}, {'candidate': 'とる', 'score': '0.8'}, {'candidate': '撮る', 'score': '0.6'}, {'candidate': '摂る', 'score': '0.4'}, {'candidate': '採る', 'score': '0.2'}, {'candidate': '執る', 'score': '0.0'}, {'candidate': 'toru', 'score': '0.1'}],
            "hakaru": [{'candidate': '図る', 'score': '1.0'}, {'candidate': '測る', 'score': '0.8'}, {'candidate': 'はかる', 'score': '0.6'}, {'candidate': '計る', 'score': '0.4'}, {'candidate': '量る', 'score': '0.2'}, {'candidate': '諮る', 'score': '0.0'}, {'candidate': 'hakaru', 'score': '0.1'}],
            "osameru": [{'candidate': '治める', 'score': '1.0'}, {'candidate': '収める', 'score': '0.8'}, {'candidate': '納める', 'score': '0.6'}, {'candidate': 'おさめる', 'score': '0.4'}, {'candidate': '修める', 'score': '0.2'}, {'candidate': 'お覚める', 'score': '0.0'}, {'candidate': 'osameru', 'score': '0.1'}]
        }

        candidates = conversion_map.get(reading, [
            {"candidate": reading, "score": "1.0"}  # デフォルトはそのまま
        ])

        return candidates[:max_candidates]

def test_mozc_client():
    """テスト用関数（実際のMozc履歴データ対応版）"""
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
            print(f"  {i}. {candidate['candidate']} (スコア: {candidate['score']})")

if __name__ == "__main__":
    test_mozc_client()
