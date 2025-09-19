#!/usr/bin/env python3
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
        変換候補をシミュレート
        """

        # よくある読みと変換候補のマッピング (シミュレーション用)
        conversion_map = {
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
            "あたらしい": [
                {"candidate": "新しい", "score": "1.0"},
                {"candidate": "新らしい", "score": "0.1"}
            ],
            "こころ": [
                {"candidate": "心", "score": "1.0"},
                {"candidate": "志", "score": "0.3"},
                {"candidate": "こころ", "score": "0.1"}
            ],
            "いく": [
                {"candidate": "行く", "score": "1.0"},
                {"candidate": "逝く", "score": "0.4"},
                {"candidate": "往く", "score": "0.2"}
            ]
        }

        candidates = conversion_map.get(reading, [
            {"candidate": reading, "score": "1.0"}  # デフォルトはそのまま
        ])

        return candidates[:max_candidates]

def test_mozc_client():
    """テスト用関数"""
    client = MozcClient()

    test_cases = [
        ("おさめる", "税金を"),
        ("はかる", "時間を"),
        ("あたらしい", ""),
        ("こころ", "人の"),
        ("いく", "学校に")
    ]

    for reading, context in test_cases:
        print(f"\n読み: {reading}")
        print(f"文脈: {context}")
        candidates = client.get_conversion_candidates(reading, context)

        for i, candidate in enumerate(candidates, 1):
            print(f"  {i}. {candidate['candidate']} (スコア: {candidate['score']})")

if __name__ == "__main__":
    test_mozc_client()