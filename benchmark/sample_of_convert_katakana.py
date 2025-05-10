#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Sample program to convert a single katakana sentence to romaji
using KatakanaToRomajiConverter.
"""

from katakana_to_romaji_converter import KatakanaToRomajiConverter

def main():
    katakana_text = "ハロー・ワールド"
    converter = KatakanaToRomajiConverter()
    romaji_text = converter.convert(katakana_text)
    print("Original:", katakana_text)
    print("Romaji: ", romaji_text)

if __name__ == "__main__":
    main()
