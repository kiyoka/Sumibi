#!/usr/bin/python3
import sys
import re

# 文字列が、ひらがなの範囲内かどうかをチェックする
def is_hiragana(s):
    return bool(re.match(r'^[\u3040-\u309F]+ ', s))

# 辞書ファイルを読み込んで、同音異義語として使える部分だけを取り出し、配列で返す
def load(dic_filename):
    arr = []
    with open(dic_filename) as f:
        for line in f.readlines():
            if is_hiragana(line):
                line = line.rstrip('\n\r')
                arr.append(line)
    return arr


# "/a/b/c/"という文字列を '("a" "b" "c")「という文字列に変換する
def convert_skkdic_value(s):
    # 初めと終わりの'/'を除去
    s = s.strip('/')
    # '/'で分割
    arr = s.split('/')
    # 一つ一つの値をダブルクォーテーションで囲む
    newarr = []
    for i in arr:
        newarr.append('"{0}"'.format(i))
    return newarr

# SKK辞書の1行のフォーマットをLisp式に変換する
def skkdic_to_lisp(line):
    arr = line.split()
    newarr = convert_skkdic_value(arr[1])
    return '("{0}" ({1}))'.format(arr[0],' '.join(newarr))

def generate_emacslisp(dic_array):
    print(';; This file is generated from SKK-JISYO.L.unannotated.')
    print(';; As SKK-JISYO.L.unannotated is under GPL-2, this file will also be under GPL-2.')
    print(";;")
    print("(defvar sumibi-localdic")
    print("  '(")
    for line in dic_array:
        line = skkdic_to_lisp(line)
        print('    ' + line)
    print("))")
    print("(provide 'sumibi-localdic)")

def generate_localdic(dic_filename):
    dic_array = load(dic_filename)
    generate_emacslisp(dic_array)

def main(argv):
    if len(argv) < 2:
        print("正しく引数を指定してください。")
        sys.exit(1)
    generate_localdic(argv[1])

if __name__ == "__main__":
    main(sys.argv)
