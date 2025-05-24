# -*- coding: utf-8 -*-
# 辞書を使用してカタカナ文字列をアルファベット（ローマ字）に変換するクラス
    
class KatakanaToRomajiConverter:
    """
    辞書を使用してカタカナ文字列をアルファベット（ローマ字）に変換するクラス
    このクラスは、指定された日本語の文字とローマ字の対応表に基づいて変換を行います。
    全角記号や全角数字も変換対象となります。
    """

    # クラス属性として変換辞書を定義 (キー:かな/記号/数字、値:ローマ字)
    # Define the conversion dictionary as a class attribute.
    # (Key: Kana/Symbol/Number, Value: Romaji)
    # This dictionary is derived from the original list structure,
    # ensuring that later definitions for the same Kana key override earlier ones.
    _FIXED_RAW_TABLE = {
        "あ": "a", "い": "i", "う": "u", "え": "e", "お": "o", # "う" is "wu" due to ("wu", "う") being last for "う"
        "か": "ka", "き": "ki", "く": "ku", "け": "ke", "こ": "ko",
        "さ": "sa", "し": "shi", "す": "su", "せ": "se", "そ": "so", # "し" is "shi"
        "た": "ta", "ち": "chi", "つ": "tsu", "て": "te", "と": "to", # "ち" is "chi", "つ" is "tsu"
        "な": "na", "に": "ni", "ぬ": "nu", "ね": "ne", "の": "no",
        "は": "ha", "ひ": "hi", "ふ": "fu", "へ": "he", "ほ": "ho", # "ふ" is "fu"
        "ま": "ma", "み": "mi", "む": "mu", "め": "me", "も": "mo",
        "や": "ya", "ゆ": "yu", "よ": "yo",
        "ら": "ra", "り": "ri", "る": "ru", "れ": "re", "ろ": "ro", # 'ら'行 etc. use 'r' versions
        "わ": "wa", "ゐ": "wi", "ゑ": "we", "を": "wo",
        "ん": "nn", # "ん" is "nn"
        "が": "ga", "ぎ": "gi", "ぐ": "gu", "げ": "ge", "ご": "go",
        "ざ": "za", "じ": "ji", "ず": "zu", "ぜ": "ze", "ぞ": "zo", # "じ" is "ji"
        "だ": "da", "ぢ": "di", "づ": "du", "で": "de", "ど": "do",
        "ば": "ba", "び": "bi", "ぶ": "bu", "べ": "be", "ぼ": "bo",
        "ぱ": "pa", "ぴ": "pi", "ぷ": "pu", "ぺ": "pe", "ぽ": "po",

        # 拗音 (ようおん) - Contracted sounds
        "きゃ": "kya", "きゅ": "kyu", "きぇ": "kye", "きょ": "kyo",
        "しゃ": "sha", "しゅ": "shu", "しぇ": "she", "しょ": "sho", # 'しゃ'行 use 'sh' versions
        "ちゃ": "tya", "ちゅ": "tyu", "ちぇ": "tye", "ちょ": "tyo", # 'ちゃ'行 use 'ty' versions
        "にゃ": "nya", "にゅ": "nyu", "にぇ": "nye", "にょ": "nyo",
        "ひゃ": "hya", "ひゅ": "hyu", "ひぇ": "hye", "ひょ": "hyo",
        "みゃ": "mya", "みゅ": "myu", "みぇ": "mye", "みょ": "myo",
        "りゃ": "rya", "りゅ": "ryu", "りぇ": "rye", "りょ": "ryo", # 'りゃ'行 use 'ry' versions
        "ぎゃ": "gya", "ぎゅ": "gyu", "ぎぇ": "gye", "ぎょ": "gyo",
        "じゃ": "ja", "じゅ": "ju", "じぇ": "je", "じょ": "jo",    # 'じゃ'行 use 'j' versions
        "ぢゃ": "ja", "ぢゅ": "ju", "ぢぇ": "je", "ぢょ": "jo",    # 'ぢゃ'行 treated same as 'じゃ'
        "びゃ": "bya", "びゅ": "byu", "びぇ": "bye", "びょ": "byo",
        "ぴゃ": "pya", "ぴゅ": "pyu", "ぴぇ": "pye", "ぴょ": "pyo",

        # 外来音 (がいらいおん) - Sounds for foreign words
        "くゎ": "kwa", "くぃ": "kwi", "くぇ": "kwe", "くぉ": "kwo",
        "つぁ": "tsa", "つぃ": "tsi", "つぇ": "tse", "つぉ": "tso",
        "ふぁ": "fa", "ふぃ": "fi", "ふぇ": "fe", "ふぉ": "fo",
        "ぐゎ": "gwa", "ぐぃ": "gwi", "ぐぇ": "gwe", "ぐぉ": "gwo",

        "でぃ": "dyi", "どぅ": "dyu", "でぇ": "dye", "どぉ": "dyo",
        "うぃ": "xwi", "うぇ": "xwe", "うぉ": "xwo",

        # その他の一般的なローマ字表記 (Other common romaji spellings)
        "てぃ": "tyi",
        "いぇ": "ye",

        # ヴ行 (va, vi, vu, ve, vo)
        "ヴぁ": "va", "ヴぃ": "vi", "ヴ": "vu", "ヴぇ": "ve", "ヴぉ": "vo",

        # 小さい仮名 (ちいさいかな) - Small kana
        "ぁ": "xa", "ぃ": "xi", "ぅ": "xu", "ぇ": "xe", "ぉ": "xo",
        "っ": "xtu", # 促音 (そくおん) - Double consonant sound
        "ゃ": "xya", "ゅ": "xyu", "ょ": "xyo", "ゎ": "xwa",
        "ヵ": "xka", "ヶ": "xke", # 固有名詞などで使われる

        # 全角数字 (ぜんかくすうじ) - Full-width numbers
        "１": "1", "２": "2", "３": "3", "４": "4", "５": "5",
        "６": "6", "７": "7", "８": "8", "９": "9", "０": "0",

        # 全角記号 (ぜんかくきごう) - Full-width symbols
        "！": "!", "＠": "@", "＃": "#", "＄": "$", "％": "%",
        "＾": "^", "＆": "&", "＊": "*", "（": "(", "）": ")",
        "ー": "-", # 長音符 (ちょうおんぷ) - Prolonged sound mark
        "＝": "=", "｀": "`", "￥": "\\", "｜": "|",
        "＿": "_", "＋": "+", "￣": "~", "「": "[", "」": "]",
        "｛": "{", "｝": "}", "：": ":", "；": ";", "”": "\"",
        "’": "'", "。": ".", "、": ",", "＜": "<", "＞": ">",
        "？": "?", "／": "/"
    }

    def __init__(self):
        """
        コンバータを初期化し、マッピング辞書を準備します。
        (Initializes the converter and prepares the mapping dictionaries.)
        """
        self.romaji_map = {}
        self.sorted_romaji_map_keys = []
        self._initialize_maps()

    def _initialize_maps(self):
        """
        内部マッピング辞書とソート済みキーリストを準備します。
        _FIXED_RAW_TABLE (クラス属性の辞書) をインスタンスのマッピングに使用します。
        (Prepares the internal mapping dictionary and sorted key list.
         Uses _FIXED_RAW_TABLE (the class attribute dictionary) for instance mapping.)
        """
        # _FIXED_RAW_TABLE は既に {かな: ローマ字} の辞書形式です。
        # _FIXED_RAW_TABLE is already a dictionary of {kana: romaji}.
        # インスタンスの romaji_map としてコピーを保持します。
        # Hold a copy as the instance's romaji_map for potential instance-specific modification (though not used here).
        self.romaji_map = self._FIXED_RAW_TABLE.copy()

        # 最長一致のために、キー（日本語表記）を文字数の降順でソートする。
        # Sort keys (Japanese script from self.romaji_map) by length in descending order for longest match.
        self.sorted_romaji_map_keys = sorted(self.romaji_map.keys(), key=len, reverse=True)

    def convert(self, katakana_string: str) -> str:
        """
        指定されたカタカナ文字列をアルファベット（ローマ字）に変換します。
        テーブルに含まれる全角記号や全角数字も変換対象となります。

        Args:
            katakana_string: 変換対象のカタカナ文字列。
                             テーブルに定義された全角記号や数字を含むことがあります。
                             (The input katakana string, which may also contain
                              full-width symbols and numbers defined in the table.)

        Returns:
            アルファベットに変換された文字列。
            (The converted string in alphabet (Romanji).)
        """
        if not isinstance(katakana_string, str):
            raise TypeError("入力は文字列である必要があります。 (Input must be a string.)")

        # 1. 入力カタカナ文字列の前処理：カタカナをひらがなに変換（必要な場合）
        #    Preprocess the input string: Convert katakana to hiragana where applicable.
        processed_parts = []
        for char_k in katakana_string:
            if '\u30A1' <= char_k <= '\u30F3': # 一般的なカタカナの範囲 (Covers ァ to ン)
                processed_parts.append(chr(ord(char_k) - 0x60)) # ひらがなに変換
            else:
                # カタカナ範囲外（ヴ, ヵ, ヶ, ー, 記号, 数字など）はそのまま
                # These characters are expected to be keys in self.romaji_map if they are convertible.
                processed_parts.append(char_k)
        processed_string = "".join(processed_parts)

        # 2. 前処理済み文字列を最長一致でアルファベットに変換
        #    Convert the preprocessed string to alphabet using the longest match algorithm.
        result_romaji_list = []
        current_idx = 0
        text_len = len(processed_string)

        while current_idx < text_len:
            # Handle small tsu (促音): geminate the following consonant
            if processed_string[current_idx] == 'っ':
                if current_idx + 1 < text_len:
                    # Find the next romaji mapping to determine consonant to double
                    for next_key in self.sorted_romaji_map_keys:
                        if processed_string.startswith(next_key, current_idx + 1):
                            next_romaji = self.romaji_map[next_key]
                            if next_romaji:
                                result_romaji_list.append(next_romaji[0])
                            break
                current_idx += 1
                continue
            matched = False
            # self.sorted_romaji_map_keys は長いキーから順に試す
            # Iterate through sorted keys (longest first) from instance attribute.
            for map_key in self.sorted_romaji_map_keys:
                if processed_string.startswith(map_key, current_idx):
                    result_romaji_list.append(self.romaji_map[map_key]) # Use instance attribute
                    current_idx += len(map_key)
                    matched = True
                    break
            
            if not matched:
                # マッピングにない文字シーケンスの場合、元の文字（前処理済み）をそのまま追加
                # If no match is found, append the original (preprocessed) character.
                result_romaji_list.append(processed_string[current_idx])
                current_idx += 1
                
        return "".join(result_romaji_list)

# --- 以下、使用例 (Example Usage) ---
if __name__ == '__main__':
    # コンバータのインスタンスを作成
    # Create an instance of the converter.
    converter = KatakanaToRomajiConverter()

    # テストケース (Test cases)
    # 期待値は、_FIXED_RAW_TABLE で定義されたマッピング（特に重複定義の場合は後者が優先）に基づきます。
    # Expected values are based on the mappings defined in _FIXED_RAW_TABLE
    # (where latter definitions take precedence for duplicate keys).
    test_cases = {
        "アメリカ": "amerika",
        "コンピュータ": "konnpyu-ta", # コ(ko)ン(nn)ピ(pi)ュ(xyu->yu, but ぴゅ is pyu)ー(ー)タ(ta)
                                       # In _FIXED_RAW_TABLE: "ん":"nn", "ー":"-", "ぴゅ":"pyu" (from ぴゃ行)
                                       # The preprocessor turns ピュ to ぴゅ.
                                       # The map has "ぴゅ": "pyu"
        "ヴァイオリン": "vaiorinn",   # ヴぁ(va)イ(i)オ(o)リ(ri)ン(nn)
        "サーバー１００％": "sa-ba-100%", # サ(sa)ー(ー)バ(ba)ー(ー)１(1)０(0)０(0)％(%)
        "キッテクダサイネ": "kittekudasaine", # キ(ki)ッ(xtu)テ(te)ク(ku)ダ(da)サ(sa)イ(i)ネ(ne)
        "ヂャーナル": "ja-naru",      # ヂャ(ja)ー(ー)ナ(na)ル(ru) (ぢゃ -> ja)
        "ヵヶ": "xkaxke",             # ヵ(xka), ヶ(xke)
        "ワープロ": "wa-puro",        # ワ(wa)ー(ー)プ(pu)ロ(lo) (ろ -> lo)
        "シンフォニー": "shinnfoni-", # シ(shi)ン(nn)フォ(fo)ニ(ni)ー(ー)
        "クィーン": "kwi-nn",         # クィ(kwi)ー(ー)ン(nn) (くぃ -> kwi)
        "ハロー・ワールド": "haro-・wa-rudo" # 「・」はテーブルにないのでそのまま
                                           # (The middle dot '・' is not in the table, so it remains.)
    }

    print("テスト実行中... (Running tests...)")
    all_tests_passed = True
    for katakana, expected_romaji in test_cases.items():
        # インスタンスのconvertメソッドを呼び出す
        # Call the convert method on the instance.
        result = converter.convert(katakana)
        print(f"\n入力 (Input): '{katakana}'")
        print(f"  期待値 (Expected): '{expected_romaji}'")
        print(f"  変換結果 (Actual):   '{result}'")
        if result == expected_romaji:
            print("  結果 (Result): OK")
        else:
            print(f"  結果 (Result): NG (期待値と異なります - Differs from expected)")
            all_tests_passed = False
        print("-" * 30)

    if all_tests_passed:
        print("\n全てのテストケースに成功しました！ (All test cases passed!)")
    else:
        print("\nいくつかのテストケースに失敗しました。 (Some test cases failed.)")
