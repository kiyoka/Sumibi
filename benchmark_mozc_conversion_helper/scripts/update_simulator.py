#!/usr/bin/env python3
"""
拡張されたシミュレーションデータでmozc_simulator.pyを更新
"""

import json

def update_simulator():
    """拡張データでシミュレータを更新"""

    # 拡張データを読み込み
    with open('expanded_simulation_data.json', 'r', encoding='utf-8') as f:
        expanded_data = json.load(f)

    print(f"Loaded {len(expanded_data)} patterns from expanded_simulation_data.json")

    # mozc_simulator.pyのテンプレート
    simulator_content = '''#!/usr/bin/env python3
"""
変換候補をシミュレートするクライアント（Mozcの動作をシミュレート）
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
        変換候補をシミュレート（127パターンの動詞・形容詞・副詞データベース）
        """

        # 127パターンの動詞・形容詞・副詞データベース（名詞除外）
        conversion_map = {
''' + ',\n'.join(f'            "{reading}": {repr(candidates)}' for reading, candidates in expanded_data.items()) + '''
        }

        candidates = conversion_map.get(reading, [
            {"candidate": reading, "score": "1.0"}  # デフォルトはそのまま
        ])

        return candidates[:max_candidates]

def test_mozc_client():
    """テスト用関数（127パターン対応版）"""
    client = MozcClient()

    # 代表的なテストケースを選択
    test_cases = [
        ("おさめる", "税金を"),
        ("はかる", "時間を"),
        ("あたらしい", ""),
        ("とる", "写真を"),
        ("かえる", "家に"),
        ("あつい", "今日は"),
        ("みる", "映画を"),
        ("きく", "音楽を"),
        ("いう", "みんなに"),
        ("つくる", "料理を"),
        ("かく", "手紙を"),
        ("よむ", "本を"),
        ("うつ", "ボールを"),
        ("ひく", "綱を"),
        ("あける", "窓を"),
        ("たかい", "値段が"),
        ("やすい", "商品が"),
        ("うつくしい", "景色が"),
        ("むずかしい", "問題が"),
        ("たのしい", "時間が")
    ]

    for reading, context in test_cases:
        print(f"\\n読み: {reading}")
        print(f"文脈: {context}")
        candidates = client.get_conversion_candidates(reading, context)

        for i, candidate in enumerate(candidates, 1):
            print(f"  {i}. {candidate['candidate']} (スコア: {candidate['score']})")

if __name__ == "__main__":
    test_mozc_client()
'''

    # ファイルに書き込み
    with open('mozc_helper/mozc_simulator.py', 'w', encoding='utf-8') as f:
        f.write(simulator_content)

    print("Updated mozc_simulator.py with 127 patterns")
    print("Sample patterns:")
    for i, (reading, candidates) in enumerate(expanded_data.items()):
        if i < 10:
            print(f"  {reading}: {[c['candidate'] for c in candidates[:3]]}...")

if __name__ == "__main__":
    update_simulator()