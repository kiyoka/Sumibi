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
        """ベンチマーク用のテキストをダウンロード（拡充版）"""

        texts_to_download = [
            # 夏目漱石作品
            ("https://www.aozora.gr.jp/cards/000148/files/773_14560.html", "kokoro_natsume.txt"),
            ("https://www.aozora.gr.jp/cards/000148/files/752_14559.html", "botchan_natsume.txt"),
            ("https://www.aozora.gr.jp/cards/000148/files/789_14547.html", "wagahai_natsume.txt"),

            # 芥川龍之介作品
            ("https://www.aozora.gr.jp/cards/000879/files/127_15260.html", "rashomon_akutagawa.txt"),
            ("https://www.aozora.gr.jp/cards/000879/files/92_14545.html", "kumo_akutagawa.txt"),
            ("https://www.aozora.gr.jp/cards/000879/files/158_15269.html", "jigokuhen_akutagawa.txt"),

            # 宮沢賢治作品
            ("https://www.aozora.gr.jp/cards/000081/files/456_15050.html", "ginga_tetsudo_miyazawa.txt"),
            ("https://www.aozora.gr.jp/cards/000081/files/43737_17659.html", "cello_miyazawa.txt"),
            ("https://www.aozora.gr.jp/cards/000081/files/470_15407.html", "kaze_miyazawa.txt"),

            # 太宰治作品
            ("https://www.aozora.gr.jp/cards/000035/files/301_14912.html", "ningen_dazai.txt"),
            ("https://www.aozora.gr.jp/cards/000035/files/1565_8559.html", "hashire_dazai.txt"),
            ("https://www.aozora.gr.jp/cards/000035/files/2281_13942.html", "shayo_dazai.txt"),

            # 森鴎外作品
            ("https://www.aozora.gr.jp/cards/000129/files/695_18648.html", "takase_ogai.txt"),
            ("https://www.aozora.gr.jp/cards/000129/files/698_20685.html", "vita_ogai.txt"),
            ("https://www.aozora.gr.jp/cards/000129/files/45630_21868.html", "okitsu_ogai.txt"),

            # 樋口一葉作品
            ("https://www.aozora.gr.jp/cards/000064/files/392_19967.html", "takekurabe_ichiyo.txt"),
            ("https://www.aozora.gr.jp/cards/000064/files/393_19968.html", "nigorie_ichiyo.txt"),
        ]

        for url, filename in texts_to_download:
            print(f"Downloading: {filename}")
            success = self.download_text(url, filename)
            if success:
                print(f"  ✓ Success: {filename}")
            else:
                print(f"  ✗ Failed: {filename}")
            time.sleep(1)  # API制限対策

if __name__ == "__main__":
    downloader = AozoraDownloader()
    downloader.download_sample_texts()
    print("青空文庫テキストのダウンロードが完了しました。")