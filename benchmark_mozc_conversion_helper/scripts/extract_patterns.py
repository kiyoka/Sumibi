#!/usr/bin/env python3
"""
青空文庫データから同音異義語パターンを抽出し、シミュレーション用データを作成
"""

import re
import json
from pathlib import Path
from collections import defaultdict

class PatternExtractor:
    def __init__(self):
        """パターン抽出器を初期化"""
        self.extracted_patterns = defaultdict(set)

    def clean_aozora_text(self, text: str) -> str:
        """青空文庫のHTMLタグ・ルビを除去"""
        # HTMLタグを除去
        text = re.sub(r'<[^>]*>', '', text)

        # ルビ記号を除去（漢字のみ残す）
        text = re.sub(r'<ruby><rb>([^<]*)</rb>.*?</ruby>', r'\1', text)

        # 青空文庫の注記を除去
        text = re.sub(r'［＃[^］]*］', '', text)

        # 連続する空白・改行を整理
        text = re.sub(r'\s+', ' ', text)
        text = re.sub(r'\n+', '\n', text)

        return text.strip()

    def extract_patterns_from_text(self, text: str):
        """テキストから動詞・形容詞・副詞の同音異義語パターンを抽出（名詞除外）"""

        # 動詞・形容詞・副詞パターンのみ（100パターン目標）
        patterns_to_find = [
            # 動詞パターン（基本動詞）
            (r'(見|視|観|診|看)る', 'みる'),
            (r'(聞|聴|訊)く', 'きく'),
            (r'(立|建|起|経|絶|断)つ', 'たつ'),
            (r'(取|撮|採|執|捕)る', 'とる'),
            (r'(作|造|創)る', 'つくる'),
            (r'(出|発|現)る', 'でる'),
            (r'(入|這|舐)る', 'はいる'),
            (r'(帰|返|還)る', 'かえる'),
            (r'(来|来)る', 'くる'),
            (r'(止|留|停)まる', 'とまる'),
            (r'(止|留|停)める', 'とめる'),
            (r'(通|透|徹)る', 'とおる'),
            (r'(変|代|替)わる', 'かわる'),
            (r'(変|代|替)える', 'かえる'),
            (r'(使|遣)う', 'つかう'),
            (r'(思|想|念)う', 'おもう'),
            (r'(言|云|謂)う', 'いう'),
            (r'(書|描)く', 'かく'),
            (r'(読|詠)む', 'よむ'),
            (r'(歩|歩)く', 'あるく'),
            (r'(走|奔)る', 'はしる'),
            (r'(飛|跳)ぶ', 'とぶ'),
            (r'(泳|游)ぐ', 'およぐ'),
            (r'(打|撃|討)つ', 'うつ'),
            (r'(押|推)す', 'おす'),
            (r'(引|牽|曳)く', 'ひく'),
            (r'(持|保|維)つ', 'もつ'),
            (r'(置|措)く', 'おく'),
            (r'(開|空|明)ける', 'あける'),
            (r'(閉|締)める', 'しめる'),
            (r'(切|斬|伐)る', 'きる'),
            (r'(着|来)る', 'きる'),
            (r'(売|売)る', 'うる'),
            (r'(買|海)う', 'かう'),
            (r'(食|食)う', 'くう'),
            (r'(飲|飲)む', 'のむ'),
            (r'(寝|寝)る', 'ねる'),
            (r'(起|起)きる', 'おきる'),
            (r'(座|座)る', 'すわる'),
            (r'(立|立)つ', 'たつ'),
            (r'(死|死)ぬ', 'しぬ'),
            (r'(生|生)きる', 'いきる'),
            (r'(笑|笑)う', 'わらう'),
            (r'(泣|泣)く', 'なく'),
            (r'(怒|怒)る', 'おこる'),
            (r'(喜|喜)ぶ', 'よろこぶ'),
            (r'(悲|悲)しむ', 'かなしむ'),
            (r'(驚|驚)く', 'おどろく'),
            (r'(恐|恐)れる', 'おそれる'),
            (r'(愛|愛)する', 'あいする'),
            (r'(信|信)じる', 'しんじる'),
            (r'(疑|疑)う', 'うたがう'),
            (r'(探|探)す', 'さがす'),
            (r'(見|見)つける', 'みつける'),
            (r'(失|失)う', 'うしなう'),
            (r'(得|得)る', 'える'),
            (r'(与|与)える', 'あたえる'),
            (r'(受|受)ける', 'うける'),
            (r'(送|送)る', 'おくる'),
            (r'(迎|迎)える', 'むかえる'),
            (r'(別|別)れる', 'わかれる'),
            (r'(会|会)う', 'あう'),
            (r'(話|話)す', 'はなす'),
            (r'(教|教)える', 'おしえる'),
            (r'(習|習)う', 'ならう'),
            (r'(覚|覚)える', 'おぼえる'),
            (r'(忘|忘)れる', 'わすれる'),
            (r'(知|知)る', 'しる'),
            (r'(分|分)かる', 'わかる'),
            (r'(考|考)える', 'かんがえる'),
            (r'(感|感)じる', 'かんじる'),
            (r'(決|決)める', 'きめる'),
            (r'(選|選)ぶ', 'えらぶ'),
            (r'(始|始)める', 'はじめる'),
            (r'(終|終)わる', 'おわる'),
            (r'(続|続)く', 'つづく'),
            (r'(進|進)む', 'すすむ'),
            (r'(戻|戻)る', 'もどる'),
            (r'(上|上)がる', 'あがる'),
            (r'(下|下)がる', 'さがる'),
            (r'(入|入)る', 'はいる'),
            (r'(出|出)る', 'でる'),

            # 形容詞パターン（基本形容詞）
            (r'(大|太)きい', 'おおきい'),
            (r'(小|少)さい', 'ちいさい'),
            (r'(高|貴)い', 'たかい'),
            (r'(低|安)い', 'ひくい'),
            (r'(長|永)い', 'ながい'),
            (r'(短|暫)い', 'みじかい'),
            (r'(早|速)い', 'はやい'),
            (r'(遅|晩)い', 'おそい'),
            (r'(重|思)い', 'おもい'),
            (r'(軽|経)い', 'かるい'),
            (r'(強|協)い', 'つよい'),
            (r'(弱|若)い', 'よわい'),
            (r'(広|弘)い', 'ひろい'),
            (r'(狭|挟)い', 'せまい'),
            (r'(深|新)い', 'ふかい'),
            (r'(浅|朝)い', 'あさい'),
            (r'(厚|暑)い', 'あつい'),
            (r'(薄|臼)い', 'うすい'),
            (r'(暖|温)かい', 'あたたかい'),
            (r'(冷|礼)たい', 'つめたい'),
            (r'(熱|熱)い', 'あつい'),
            (r'(寒|彼)い', 'さむい'),
            (r'(美|美)しい', 'うつくしい'),
            (r'(汚|記)たない', 'きたない'),
            (r'(明|光)るい', 'あかるい'),
            (r'(暗|鞍)い', 'くらい'),
            (r'(近|金)い', 'ちかい'),
            (r'(遠|縁)い', 'とおい'),
            (r'(難|南)しい', 'むずかしい'),
            (r'(易|安)しい', 'やさしい'),
            (r'(忙|亡)しい', 'いそがしい'),
            (r'(楽|落)しい', 'たのしい'),
            (r'(嬉|裏)しい', 'うれしい'),
            (r'(悲|悲)しい', 'かなしい'),
            (r'(新|真)しい', 'あたらしい'),
            (r'(古|故)い', 'ふるい'),
            (r'(若|弱)い', 'わかい'),
            (r'(多|太)い', 'おおい'),
            (r'(少|少)ない', 'すくない'),
            (r'(正|性)しい', 'ただしい'),
            (r'(悪|悪)い', 'わるい'),
            (r'(良|良)い', 'よい'),
            (r'(好|好)き', 'すき'),
            (r'(嫌|嫌)い', 'きらい'),
            (r'(安|安)い', 'やすい'),
            (r'(白|白)い', 'しろい'),
            (r'(黒|黒)い', 'くろい'),
            (r'(赤|赤)い', 'あかい'),
            (r'(青|青)い', 'あおい'),
            (r'(黄|黄)色い', 'きいろい'),
            (r'(緑|緑)', 'みどり'),

            # 副詞パターン
            (r'(早|速)く', 'はやく'),
            (r'(遅|遅)く', 'おそく'),
            (r'(高|高)く', 'たかく'),
            (r'(低|低)く', 'ひくく'),
            (r'(大|大)きく', 'おおきく'),
            (r'(小|小)さく', 'ちいさく'),
            (r'(強|強)く', 'つよく'),
            (r'(弱|弱)く', 'よわく'),
            (r'(明|明)るく', 'あかるく'),
            (r'(暗|暗)く', 'くらく'),
        ]

        for pattern, reading in patterns_to_find:
            matches = re.finditer(pattern, text)
            for match in matches:
                candidate = match.group(1) if match.groups() else match.group()
                if candidate and len(candidate) <= 4:  # 長すぎる候補は除外
                    self.extracted_patterns[reading].add(candidate)

    def process_data_files(self, data_dir: str = "data"):
        """データディレクトリ内のファイルを処理"""
        data_path = Path(data_dir)

        if not data_path.exists():
            print(f"Data directory not found: {data_dir}")
            return

        for file_path in data_path.glob("*.txt"):
            print(f"Processing: {file_path.name}")

            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            # テキストをクリーンアップ
            clean_text = self.clean_aozora_text(content)

            # パターンを抽出
            self.extract_patterns_from_text(clean_text)

    def generate_simulation_data(self, min_candidates: int = 2) -> dict:
        """シミュレーション用のデータを生成"""
        simulation_data = {}

        for reading, candidates in self.extracted_patterns.items():
            candidate_list = list(candidates)

            # 最低候補数をチェック
            if len(candidate_list) >= min_candidates:
                # スコアを生成（頻出順に高スコア）
                scored_candidates = []
                for i, candidate in enumerate(candidate_list[:6]):  # 最大6候補
                    score = round(1.0 - (i * 0.15), 1)  # 1.0, 0.85, 0.7, 0.55, 0.4, 0.25
                    scored_candidates.append({
                        "candidate": candidate,
                        "score": str(max(score, 0.1))  # 最低0.1
                    })

                # ひらがなも最後に追加
                scored_candidates.append({
                    "candidate": reading,
                    "score": "0.1"
                })

                simulation_data[reading] = scored_candidates

        return simulation_data

    def save_to_file(self, output_file: str = "mozc_simulation_data.json"):
        """抽出結果をファイルに保存"""
        simulation_data = self.generate_simulation_data()

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(simulation_data, f, ensure_ascii=False, indent=2)

        print(f"Generated {len(simulation_data)} patterns")
        print(f"Saved to: {output_file}")

        # 統計を表示
        for reading, candidates in list(simulation_data.items())[:10]:
            print(f"  {reading}: {[c['candidate'] for c in candidates[:3]]}...")

def main():
    """メイン処理"""
    extractor = PatternExtractor()
    extractor.process_data_files()
    extractor.save_to_file()

if __name__ == "__main__":
    main()