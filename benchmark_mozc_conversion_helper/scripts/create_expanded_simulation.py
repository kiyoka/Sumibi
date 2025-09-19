#!/usr/bin/env python3
"""
100件の動詞・形容詞パターンでシミュレーションデータを作成
"""

import json

def create_expanded_simulation_data():
    """100件の動詞・形容詞・副詞パターンを作成"""

    expanded_patterns = {
        # 既存パターン（23件）を含め、さらに拡充
        "おさめる": [
            {"candidate": "納める", "score": "1.0"},
            {"candidate": "収める", "score": "0.8"},
            {"candidate": "治める", "score": "0.6"},
            {"candidate": "修める", "score": "0.4"},
            {"candidate": "おさめる", "score": "0.2"}
        ],
        "はかる": [
            {"candidate": "測る", "score": "1.0"},
            {"candidate": "計る", "score": "0.9"},
            {"candidate": "図る", "score": "0.8"},
            {"candidate": "量る", "score": "0.7"},
            {"candidate": "諮る", "score": "0.5"},
            {"candidate": "謀る", "score": "0.3"}
        ],
        "とる": [
            {"candidate": "捕", "score": "1.0"},
            {"candidate": "取", "score": "0.8"},
            {"candidate": "撮", "score": "0.6"},
            {"candidate": "採", "score": "0.4"},
            {"candidate": "とる", "score": "0.1"}
        ],
        "かえる": [
            {"candidate": "返", "score": "1.0"},
            {"candidate": "帰", "score": "0.8"},
            {"candidate": "変", "score": "0.7"},
            {"candidate": "かえる", "score": "0.1"}
        ],
        "いう": [
            {"candidate": "云", "score": "1.0"},
            {"candidate": "言", "score": "0.8"},
            {"candidate": "謂", "score": "0.6"},
            {"candidate": "いう", "score": "0.1"}
        ],
        "きる": [
            {"candidate": "着", "score": "1.0"},
            {"candidate": "斬", "score": "0.8"},
            {"candidate": "切", "score": "0.7"},
            {"candidate": "伐", "score": "0.6"},
            {"candidate": "きる", "score": "0.1"}
        ],
        "みる": [
            {"candidate": "見", "score": "1.0"},
            {"candidate": "視", "score": "0.8"},
            {"candidate": "観", "score": "0.7"},
            {"candidate": "診", "score": "0.6"},
            {"candidate": "みる", "score": "0.1"}
        ],
        "きく": [
            {"candidate": "聞", "score": "1.0"},
            {"candidate": "聴", "score": "0.8"},
            {"candidate": "訊", "score": "0.6"},
            {"candidate": "きく", "score": "0.1"}
        ],
        "たつ": [
            {"candidate": "立", "score": "1.0"},
            {"candidate": "建", "score": "0.8"},
            {"candidate": "起", "score": "0.7"},
            {"candidate": "経", "score": "0.6"},
            {"candidate": "たつ", "score": "0.1"}
        ],
        "つくる": [
            {"candidate": "作", "score": "1.0"},
            {"candidate": "造", "score": "0.8"},
            {"candidate": "創", "score": "0.6"},
            {"candidate": "つくる", "score": "0.1"}
        ],
        "かく": [
            {"candidate": "書", "score": "1.0"},
            {"candidate": "描", "score": "0.8"},
            {"candidate": "掻", "score": "0.6"},
            {"candidate": "かく", "score": "0.1"}
        ],
        "よむ": [
            {"candidate": "読", "score": "1.0"},
            {"candidate": "詠", "score": "0.8"},
            {"candidate": "よむ", "score": "0.1"}
        ],
        "あるく": [
            {"candidate": "歩", "score": "1.0"},
            {"candidate": "歩", "score": "0.8"},
            {"candidate": "あるく", "score": "0.1"}
        ],
        "はしる": [
            {"candidate": "走", "score": "1.0"},
            {"candidate": "奔", "score": "0.8"},
            {"candidate": "はしる", "score": "0.1"}
        ],
        "とぶ": [
            {"candidate": "飛", "score": "1.0"},
            {"candidate": "跳", "score": "0.8"},
            {"candidate": "とぶ", "score": "0.1"}
        ],
        "およぐ": [
            {"candidate": "泳", "score": "1.0"},
            {"candidate": "游", "score": "0.8"},
            {"candidate": "およぐ", "score": "0.1"}
        ],
        "うつ": [
            {"candidate": "打", "score": "1.0"},
            {"candidate": "撃", "score": "0.8"},
            {"candidate": "討", "score": "0.6"},
            {"candidate": "うつ", "score": "0.1"}
        ],
        "おす": [
            {"candidate": "押", "score": "1.0"},
            {"candidate": "推", "score": "0.8"},
            {"candidate": "おす", "score": "0.1"}
        ],
        "ひく": [
            {"candidate": "引", "score": "1.0"},
            {"candidate": "牽", "score": "0.8"},
            {"candidate": "曳", "score": "0.6"},
            {"candidate": "ひく", "score": "0.1"}
        ],
        "もつ": [
            {"candidate": "持", "score": "1.0"},
            {"candidate": "保", "score": "0.8"},
            {"candidate": "維", "score": "0.6"},
            {"candidate": "もつ", "score": "0.1"}
        ],
        "おく": [
            {"candidate": "置", "score": "1.0"},
            {"candidate": "措", "score": "0.8"},
            {"candidate": "おく", "score": "0.1"}
        ],
        "あける": [
            {"candidate": "明", "score": "1.0"},
            {"candidate": "開", "score": "0.8"},
            {"candidate": "空", "score": "0.6"},
            {"candidate": "あける", "score": "0.1"}
        ],
        "しめる": [
            {"candidate": "閉", "score": "1.0"},
            {"candidate": "締", "score": "0.8"},
            {"candidate": "湿", "score": "0.6"},
            {"candidate": "しめる", "score": "0.1"}
        ],
        "うる": [
            {"candidate": "売", "score": "1.0"},
            {"candidate": "得", "score": "0.8"},
            {"candidate": "うる", "score": "0.1"}
        ],
        "かう": [
            {"candidate": "買", "score": "1.0"},
            {"candidate": "飼", "score": "0.8"},
            {"candidate": "かう", "score": "0.1"}
        ],
        "くう": [
            {"candidate": "食", "score": "1.0"},
            {"candidate": "喰", "score": "0.8"},
            {"candidate": "くう", "score": "0.1"}
        ],
        "のむ": [
            {"candidate": "飲", "score": "1.0"},
            {"candidate": "呑", "score": "0.8"},
            {"candidate": "のむ", "score": "0.1"}
        ],
        "ねる": [
            {"candidate": "寝", "score": "1.0"},
            {"candidate": "練", "score": "0.8"},
            {"candidate": "ねる", "score": "0.1"}
        ],
        "おきる": [
            {"candidate": "起", "score": "1.0"},
            {"candidate": "興", "score": "0.8"},
            {"candidate": "おきる", "score": "0.1"}
        ],
        "すわる": [
            {"candidate": "座", "score": "1.0"},
            {"candidate": "坐", "score": "0.8"},
            {"candidate": "すわる", "score": "0.1"}
        ],
        "しぬ": [
            {"candidate": "死", "score": "1.0"},
            {"candidate": "しぬ", "score": "0.1"}
        ],
        "いきる": [
            {"candidate": "生", "score": "1.0"},
            {"candidate": "活", "score": "0.8"},
            {"candidate": "いきる", "score": "0.1"}
        ],
        "わらう": [
            {"candidate": "笑", "score": "1.0"},
            {"candidate": "嗤", "score": "0.8"},
            {"candidate": "わらう", "score": "0.1"}
        ],
        "なく": [
            {"candidate": "泣", "score": "1.0"},
            {"candidate": "鳴", "score": "0.8"},
            {"candidate": "なく", "score": "0.1"}
        ],
        "おこる": [
            {"candidate": "怒", "score": "1.0"},
            {"candidate": "起", "score": "0.8"},
            {"candidate": "おこる", "score": "0.1"}
        ],
        "よろこぶ": [
            {"candidate": "喜", "score": "1.0"},
            {"candidate": "歓", "score": "0.8"},
            {"candidate": "よろこぶ", "score": "0.1"}
        ],
        "かなしむ": [
            {"candidate": "悲", "score": "1.0"},
            {"candidate": "哀", "score": "0.8"},
            {"candidate": "かなしむ", "score": "0.1"}
        ],
        "おどろく": [
            {"candidate": "驚", "score": "1.0"},
            {"candidate": "愕", "score": "0.8"},
            {"candidate": "おどろく", "score": "0.1"}
        ],
        "おそれる": [
            {"candidate": "恐", "score": "1.0"},
            {"candidate": "畏", "score": "0.8"},
            {"candidate": "おそれる", "score": "0.1"}
        ],
        "あいする": [
            {"candidate": "愛", "score": "1.0"},
            {"candidate": "愛", "score": "0.8"},
            {"candidate": "あいする", "score": "0.1"}
        ],
        "しんじる": [
            {"candidate": "信", "score": "1.0"},
            {"candidate": "伸", "score": "0.8"},
            {"candidate": "しんじる", "score": "0.1"}
        ],
        "うたがう": [
            {"candidate": "疑", "score": "1.0"},
            {"candidate": "うたがう", "score": "0.1"}
        ],
        "さがす": [
            {"candidate": "探", "score": "1.0"},
            {"candidate": "捜", "score": "0.8"},
            {"candidate": "さがす", "score": "0.1"}
        ],
        "みつける": [
            {"candidate": "見", "score": "1.0"},
            {"candidate": "発", "score": "0.8"},
            {"candidate": "みつける", "score": "0.1"}
        ],
        "うしなう": [
            {"candidate": "失", "score": "1.0"},
            {"candidate": "喪", "score": "0.8"},
            {"candidate": "うしなう", "score": "0.1"}
        ],
        "える": [
            {"candidate": "得", "score": "1.0"},
            {"candidate": "獲", "score": "0.8"},
            {"candidate": "える", "score": "0.1"}
        ],
        "あたえる": [
            {"candidate": "与", "score": "1.0"},
            {"candidate": "給", "score": "0.8"},
            {"candidate": "あたえる", "score": "0.1"}
        ],
        "うける": [
            {"candidate": "受", "score": "1.0"},
            {"candidate": "承", "score": "0.8"},
            {"candidate": "うける", "score": "0.1"}
        ],
        "おくる": [
            {"candidate": "送", "score": "1.0"},
            {"candidate": "贈", "score": "0.8"},
            {"candidate": "おくる", "score": "0.1"}
        ],
        "むかえる": [
            {"candidate": "迎", "score": "1.0"},
            {"candidate": "向", "score": "0.8"},
            {"candidate": "むかえる", "score": "0.1"}
        ],
        "わかれる": [
            {"candidate": "別", "score": "1.0"},
            {"candidate": "分", "score": "0.8"},
            {"candidate": "わかれる", "score": "0.1"}
        ],
        "あう": [
            {"candidate": "会", "score": "1.0"},
            {"candidate": "合", "score": "0.8"},
            {"candidate": "あう", "score": "0.1"}
        ],
        "はなす": [
            {"candidate": "話", "score": "1.0"},
            {"candidate": "放", "score": "0.8"},
            {"candidate": "はなす", "score": "0.1"}
        ],
        "おしえる": [
            {"candidate": "教", "score": "1.0"},
            {"candidate": "示", "score": "0.8"},
            {"candidate": "おしえる", "score": "0.1"}
        ],
        "ならう": [
            {"candidate": "習", "score": "1.0"},
            {"candidate": "倣", "score": "0.8"},
            {"candidate": "ならう", "score": "0.1"}
        ],
        "おぼえる": [
            {"candidate": "覚", "score": "1.0"},
            {"candidate": "憶", "score": "0.8"},
            {"candidate": "おぼえる", "score": "0.1"}
        ],
        "わすれる": [
            {"candidate": "忘", "score": "1.0"},
            {"candidate": "亡", "score": "0.8"},
            {"candidate": "わすれる", "score": "0.1"}
        ],
        "しる": [
            {"candidate": "知", "score": "1.0"},
            {"candidate": "汁", "score": "0.8"},
            {"candidate": "しる", "score": "0.1"}
        ],
        "わかる": [
            {"candidate": "分", "score": "1.0"},
            {"candidate": "判", "score": "0.8"},
            {"candidate": "わかる", "score": "0.1"}
        ],
        "かんがえる": [
            {"candidate": "考", "score": "1.0"},
            {"candidate": "想", "score": "0.8"},
            {"candidate": "かんがえる", "score": "0.1"}
        ],
        "かんじる": [
            {"candidate": "感", "score": "1.0"},
            {"candidate": "漢", "score": "0.8"},
            {"candidate": "かんじる", "score": "0.1"}
        ],
        "きめる": [
            {"candidate": "決", "score": "1.0"},
            {"candidate": "極", "score": "0.8"},
            {"candidate": "きめる", "score": "0.1"}
        ],
        "えらぶ": [
            {"candidate": "選", "score": "1.0"},
            {"candidate": "択", "score": "0.8"},
            {"candidate": "えらぶ", "score": "0.1"}
        ],
        "はじめる": [
            {"candidate": "始", "score": "1.0"},
            {"candidate": "初", "score": "0.8"},
            {"candidate": "はじめる", "score": "0.1"}
        ],
        "おわる": [
            {"candidate": "終", "score": "1.0"},
            {"candidate": "了", "score": "0.8"},
            {"candidate": "おわる", "score": "0.1"}
        ],
        "つづく": [
            {"candidate": "続", "score": "1.0"},
            {"candidate": "継", "score": "0.8"},
            {"candidate": "つづく", "score": "0.1"}
        ],
        "すすむ": [
            {"candidate": "進", "score": "1.0"},
            {"candidate": "薦", "score": "0.8"},
            {"candidate": "すすむ", "score": "0.1"}
        ],
        "もどる": [
            {"candidate": "戻", "score": "1.0"},
            {"candidate": "還", "score": "0.8"},
            {"candidate": "もどる", "score": "0.1"}
        ],
        "あがる": [
            {"candidate": "上", "score": "1.0"},
            {"candidate": "挙", "score": "0.8"},
            {"candidate": "あがる", "score": "0.1"}
        ],
        "さがる": [
            {"candidate": "下", "score": "1.0"},
            {"candidate": "探", "score": "0.8"},
            {"candidate": "さがる", "score": "0.1"}
        ],
        "はいる": [
            {"candidate": "入", "score": "1.0"},
            {"candidate": "這", "score": "0.8"},
            {"candidate": "はいる", "score": "0.1"}
        ],
        "でる": [
            {"candidate": "出", "score": "1.0"},
            {"candidate": "発", "score": "0.8"},
            {"candidate": "でる", "score": "0.1"}
        ],

        # 形容詞パターン
        "たかい": [
            {"candidate": "高", "score": "1.0"},
            {"candidate": "貴", "score": "0.8"},
            {"candidate": "たかい", "score": "0.1"}
        ],
        "ひくい": [
            {"candidate": "低", "score": "1.0"},
            {"candidate": "安", "score": "0.8"},
            {"candidate": "ひくい", "score": "0.1"}
        ],
        "ながい": [
            {"candidate": "長", "score": "1.0"},
            {"candidate": "永", "score": "0.8"},
            {"candidate": "ながい", "score": "0.1"}
        ],
        "みじかい": [
            {"candidate": "短", "score": "1.0"},
            {"candidate": "暫", "score": "0.8"},
            {"candidate": "みじかい", "score": "0.1"}
        ],
        "はやい": [
            {"candidate": "早", "score": "1.0"},
            {"candidate": "速", "score": "0.8"},
            {"candidate": "はやい", "score": "0.1"}
        ],
        "おそい": [
            {"candidate": "遅", "score": "1.0"},
            {"candidate": "晩", "score": "0.8"},
            {"candidate": "おそい", "score": "0.1"}
        ],
        "おもい": [
            {"candidate": "重", "score": "1.0"},
            {"candidate": "思", "score": "0.8"},
            {"candidate": "おもい", "score": "0.1"}
        ],
        "かるい": [
            {"candidate": "軽", "score": "1.0"},
            {"candidate": "軽", "score": "0.8"},
            {"candidate": "かるい", "score": "0.1"}
        ],
        "つよい": [
            {"candidate": "強", "score": "1.0"},
            {"candidate": "協", "score": "0.8"},
            {"candidate": "つよい", "score": "0.1"}
        ],
        "よわい": [
            {"candidate": "弱", "score": "1.0"},
            {"candidate": "若", "score": "0.8"},
            {"candidate": "よわい", "score": "0.1"}
        ],
        "ひろい": [
            {"candidate": "広", "score": "1.0"},
            {"candidate": "弘", "score": "0.8"},
            {"candidate": "ひろい", "score": "0.1"}
        ],
        "せまい": [
            {"candidate": "狭", "score": "1.0"},
            {"candidate": "挟", "score": "0.8"},
            {"candidate": "せまい", "score": "0.1"}
        ],
        "ふかい": [
            {"candidate": "深", "score": "1.0"},
            {"candidate": "新", "score": "0.8"},
            {"candidate": "ふかい", "score": "0.1"}
        ],
        "あさい": [
            {"candidate": "浅", "score": "1.0"},
            {"candidate": "朝", "score": "0.8"},
            {"candidate": "あさい", "score": "0.1"}
        ],
        "あつい": [
            {"candidate": "厚", "score": "1.0"},
            {"candidate": "暑", "score": "0.8"},
            {"candidate": "熱", "score": "0.7"},
            {"candidate": "あつい", "score": "0.1"}
        ],
        "うすい": [
            {"candidate": "薄", "score": "1.0"},
            {"candidate": "臼", "score": "0.8"},
            {"candidate": "うすい", "score": "0.1"}
        ],
        "あたたかい": [
            {"candidate": "暖", "score": "1.0"},
            {"candidate": "温", "score": "0.8"},
            {"candidate": "あたたかい", "score": "0.1"}
        ],
        "つめたい": [
            {"candidate": "冷", "score": "1.0"},
            {"candidate": "礼", "score": "0.8"},
            {"candidate": "つめたい", "score": "0.1"}
        ],
        "さむい": [
            {"candidate": "寒", "score": "1.0"},
            {"candidate": "彼", "score": "0.8"},
            {"candidate": "さむい", "score": "0.1"}
        ],
        "うつくしい": [
            {"candidate": "美", "score": "1.0"},
            {"candidate": "麗", "score": "0.8"},
            {"candidate": "うつくしい", "score": "0.1"}
        ],
        "きたない": [
            {"candidate": "汚", "score": "1.0"},
            {"candidate": "穢", "score": "0.8"},
            {"candidate": "きたない", "score": "0.1"}
        ],
        "あかるい": [
            {"candidate": "明", "score": "1.0"},
            {"candidate": "光", "score": "0.8"},
            {"candidate": "あかるい", "score": "0.1"}
        ],
        "くらい": [
            {"candidate": "暗", "score": "1.0"},
            {"candidate": "鞍", "score": "0.8"},
            {"candidate": "くらい", "score": "0.1"}
        ],
        "ちかい": [
            {"candidate": "近", "score": "1.0"},
            {"candidate": "金", "score": "0.8"},
            {"candidate": "ちかい", "score": "0.1"}
        ],
        "とおい": [
            {"candidate": "遠", "score": "1.0"},
            {"candidate": "縁", "score": "0.8"},
            {"candidate": "とおい", "score": "0.1"}
        ],
        "むずかしい": [
            {"candidate": "難", "score": "1.0"},
            {"candidate": "南", "score": "0.8"},
            {"candidate": "むずかしい", "score": "0.1"}
        ],
        "やさしい": [
            {"candidate": "易", "score": "1.0"},
            {"candidate": "安", "score": "0.8"},
            {"candidate": "やさしい", "score": "0.1"}
        ],
        "いそがしい": [
            {"candidate": "忙", "score": "1.0"},
            {"candidate": "亡", "score": "0.8"},
            {"candidate": "いそがしい", "score": "0.1"}
        ],
        "たのしい": [
            {"candidate": "楽", "score": "1.0"},
            {"candidate": "落", "score": "0.8"},
            {"candidate": "たのしい", "score": "0.1"}
        ],
        "うれしい": [
            {"candidate": "嬉", "score": "1.0"},
            {"candidate": "裏", "score": "0.8"},
            {"candidate": "うれしい", "score": "0.1"}
        ],
        "かなしい": [
            {"candidate": "悲", "score": "1.0"},
            {"candidate": "哀", "score": "0.8"},
            {"candidate": "かなしい", "score": "0.1"}
        ],
        "おおきい": [
            {"candidate": "大", "score": "1.0"},
            {"candidate": "太", "score": "0.8"},
            {"candidate": "おおきい", "score": "0.1"}
        ],
        "ちいさい": [
            {"candidate": "小", "score": "1.0"},
            {"candidate": "少", "score": "0.8"},
            {"candidate": "ちいさい", "score": "0.1"}
        ],
        "あたらしい": [
            {"candidate": "新", "score": "1.0"},
            {"candidate": "真", "score": "0.8"},
            {"candidate": "あたらしい", "score": "0.1"}
        ],
        "ふるい": [
            {"candidate": "古", "score": "1.0"},
            {"candidate": "故", "score": "0.8"},
            {"candidate": "ふるい", "score": "0.1"}
        ],
        "わかい": [
            {"candidate": "若", "score": "1.0"},
            {"candidate": "弱", "score": "0.8"},
            {"candidate": "わかい", "score": "0.1"}
        ],
        "おおい": [
            {"candidate": "多", "score": "1.0"},
            {"candidate": "太", "score": "0.8"},
            {"candidate": "おおい", "score": "0.1"}
        ],
        "すくない": [
            {"candidate": "少", "score": "1.0"},
            {"candidate": "寡", "score": "0.8"},
            {"candidate": "すくない", "score": "0.1"}
        ],
        "ただしい": [
            {"candidate": "正", "score": "1.0"},
            {"candidate": "性", "score": "0.8"},
            {"candidate": "ただしい", "score": "0.1"}
        ],
        "わるい": [
            {"candidate": "悪", "score": "1.0"},
            {"candidate": "惡", "score": "0.8"},
            {"candidate": "わるい", "score": "0.1"}
        ],
        "よい": [
            {"candidate": "良", "score": "1.0"},
            {"candidate": "好", "score": "0.8"},
            {"candidate": "よい", "score": "0.1"}
        ],
        "すき": [
            {"candidate": "好", "score": "1.0"},
            {"candidate": "隙", "score": "0.8"},
            {"candidate": "すき", "score": "0.1"}
        ],
        "きらい": [
            {"candidate": "嫌", "score": "1.0"},
            {"candidate": "きらい", "score": "0.1"}
        ],
        "やすい": [
            {"candidate": "安", "score": "1.0"},
            {"candidate": "易", "score": "0.8"},
            {"candidate": "やすい", "score": "0.1"}
        ],
        "しろい": [
            {"candidate": "白", "score": "1.0"},
            {"candidate": "城", "score": "0.8"},
            {"candidate": "しろい", "score": "0.1"}
        ],
        "くろい": [
            {"candidate": "黒", "score": "1.0"},
            {"candidate": "玄", "score": "0.8"},
            {"candidate": "くろい", "score": "0.1"}
        ],
        "あかい": [
            {"candidate": "赤", "score": "1.0"},
            {"candidate": "紅", "score": "0.8"},
            {"candidate": "あかい", "score": "0.1"}
        ],
        "あおい": [
            {"candidate": "青", "score": "1.0"},
            {"candidate": "碧", "score": "0.8"},
            {"candidate": "あおい", "score": "0.1"}
        ],
        "きいろい": [
            {"candidate": "黄", "score": "1.0"},
            {"candidate": "金", "score": "0.8"},
            {"candidate": "きいろい", "score": "0.1"}
        ],

        # 動詞（継続・状態変化）
        "つかう": [
            {"candidate": "使", "score": "1.0"},
            {"candidate": "遣", "score": "0.8"},
            {"candidate": "つかう", "score": "0.1"}
        ],
        "おもう": [
            {"candidate": "思", "score": "1.0"},
            {"candidate": "想", "score": "0.8"},
            {"candidate": "念", "score": "0.6"},
            {"candidate": "おもう", "score": "0.1"}
        ],
        "とめる": [
            {"candidate": "止", "score": "1.0"},
            {"candidate": "留", "score": "0.8"},
            {"candidate": "停", "score": "0.6"},
            {"candidate": "とめる", "score": "0.1"}
        ],
        "とまる": [
            {"candidate": "止", "score": "1.0"},
            {"candidate": "留", "score": "0.8"},
            {"candidate": "停", "score": "0.6"},
            {"candidate": "とまる", "score": "0.1"}
        ],
        "とおる": [
            {"candidate": "通", "score": "1.0"},
            {"candidate": "透", "score": "0.8"},
            {"candidate": "徹", "score": "0.6"},
            {"candidate": "とおる", "score": "0.1"}
        ],
        "かわる": [
            {"candidate": "変", "score": "1.0"},
            {"candidate": "代", "score": "0.8"},
            {"candidate": "替", "score": "0.6"},
            {"candidate": "かわる", "score": "0.1"}
        ],
    }

    print(f"Generated {len(expanded_patterns)} patterns")

    # JSONファイルに保存
    with open('expanded_simulation_data.json', 'w', encoding='utf-8') as f:
        json.dump(expanded_patterns, f, ensure_ascii=False, indent=2)

    print("Saved to: expanded_simulation_data.json")

    # 統計表示
    for i, (reading, candidates) in enumerate(expanded_patterns.items()):
        if i < 10:
            print(f"  {reading}: {[c['candidate'] for c in candidates[:3]]}...")

if __name__ == "__main__":
    create_expanded_simulation_data()