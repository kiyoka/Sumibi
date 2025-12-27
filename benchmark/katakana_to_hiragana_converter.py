# -*- coding: utf-8 -*-
# Katakana to Hiragana Converter

class KatakanaToHiraganaConverter:
    """
    Convert katakana strings to hiragana.
    """

    def convert(self, katakana_string):
        """
        Convert katakana to hiragana.

        Args:
            katakana_string: The katakana string to convert

        Returns:
            The converted hiragana string
        """
        if not isinstance(katakana_string, str):
            raise TypeError("Input must be a string.")

        result = []
        for char in katakana_string:
            # Katakana range: U+30A1 to U+30F3 (ァ to ン)
            if '\u30A1' <= char <= '\u30F3':
                # Convert to hiragana by subtracting 0x60
                hiragana_char = chr(ord(char) - 0x60)
                result.append(hiragana_char)
            else:
                # Keep non-katakana characters as-is (ヴ, ー, symbols, etc.)
                result.append(char)

        return "".join(result)


if __name__ == '__main__':
    converter = KatakanaToHiraganaConverter()

    test_cases = {
        "ワタシノナマエハニシヤマデス。": "わたしのなまえはにしやまです。",
        "コンニチハ": "こんにちは",
        "アリガトウゴザイマス": "ありがとうございます",
        "カタカナ": "かたかな",
    }

    print("Running katakana to hiragana conversion tests...")
    all_tests_passed = True
    for katakana, expected_hiragana in test_cases.items():
        result = converter.convert(katakana)
        print(f"\nInput: '{katakana}'")
        print(f"  Expected: '{expected_hiragana}'")
        print(f"  Result:   '{result}'")
        if result == expected_hiragana:
            print("  Status: OK")
        else:
            print(f"  Status: NG")
            all_tests_passed = False
        print("-" * 30)

    if all_tests_passed:
        print("\nAll tests passed!")
    else:
        print("\nSome tests failed.")
