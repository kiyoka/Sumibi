#!/usr/bin/env python3
"""
青空文庫からテキストデータをダウンロードし、ベンチマーク用データセットを作成する
"""

import requests
import re
import os
import time
from pathlib import Path

class AozoraDownloader:
    def __init__(self, data_dir="data"):
        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(exist_ok=True)

    def download_text(self, url, filename):
        """指定されたURLからテキストファイルをダウンロード"""
        try:
            response = requests.get(url)
            response.encoding = 'shift_jis'  # 青空文庫は通常Shift_JIS

            # 青空文庫の注記記号を除去
            text = self.clean_aozora_text(response.text)

            filepath = self.data_dir / filename
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(text)

            print(f"Downloaded: {filename}")
            return True

        except Exception as e:
            print(f"Error downloading {filename}: {e}")
            return False

    def clean_aozora_text(self, text):
        """青空文庫の注記記号を除去"""
        # ルビ記号を除去 (例: 漢字《かんじ》 → 漢字)
        text = re.sub(r'《[^》]*》', '', text)

        # 注記を除去 (例: ［＃改ページ］)
        text = re.sub(r'［＃[^］]*］', '', text)

        # 底本情報などのメタデータを除去
        text = re.sub(r'-------.*?-------', '', text, flags=re.DOTALL)

        # 連続する空行を1つにまとめる
        text = re.sub(r'\n\s*\n\s*\n', '\n\n', text)

        return text.strip()

    def download_sample_texts(self):
        """ベンチマーク用の代表的なテキストをダウンロード"""

        # 夏目漱石「こころ」
        self.download_text(
            "https://www.aozora.gr.jp/cards/000148/files/773_14560.html",
            "kokoro_natsume.txt"
        )
        time.sleep(1)

        # 芥川龍之介「羅生門」
        self.download_text(
            "https://www.aozora.gr.jp/cards/000879/files/127_15260.html",
            "rashomon_akutagawa.txt"
        )
        time.sleep(1)

        # 宮沢賢治「銀河鉄道の夜」
        self.download_text(
            "https://www.aozora.gr.jp/cards/000081/files/456_15050.html",
            "ginga_tetsudo_miyazawa.txt"
        )
        time.sleep(1)

if __name__ == "__main__":
    downloader = AozoraDownloader()
    downloader.download_sample_texts()
    print("青空文庫テキストのダウンロードが完了しました。")