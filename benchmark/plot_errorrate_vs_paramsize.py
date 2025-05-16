#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
plot_errorrate_vs_paramsize.py

各モデルの変換エラー率とパラメータ数(単位: ビリオン)をプロットする
Usage:
  モジュール名、error_rates、param_sizes リストを編集後、実行
"""
import matplotlib.pyplot as plt
import argparse

# モデル名リスト
modules = [
    'japanese-stablelm-instruct-gamma-7b',
    'gemma-3-12b-it-qat'
]
# エラー率 (比率: 0.05 == 5%)
error_rates = [
    0.981412,
    0.853688
]
# パラメータ数 (ビリオン単位)
param_sizes = [
    7,
    12
]

# カラー設定 (モデル数に合わせて調整)
base_colors = ['skyblue', 'orange', 'gray', 'green', 'pink', 'red']
colors = base_colors[:len(modules)]

plt.figure(figsize=(8, 6))
# 散布図: x=Parameter Size (Billion), y=Error Rate (%)
for name, err, size, c in zip(modules, error_rates, param_sizes, colors):
    pct = err * 100
    plt.scatter(size, pct, s=150, color=c)
    plt.annotate(name,
                 xy=(size, pct),
                 xytext=(5, 5),
                 textcoords='offset points',
                 ha='left', va='bottom', clip_on=False)

plt.xlabel('Parameter Size (Billion)')
plt.ylabel('Error Rate (%)')
plt.title('Error Rate vs Parameter Size for Local LLMs')
plt.grid(True)
# y 軸を 0%～110% の範囲に設定
plt.ylim(0, 110)
plt.margins(x=0.05)
plt.tight_layout()

# コマンドライン引数による出力先指定
parser = argparse.ArgumentParser(description='Plot error rate vs parameter size')
parser.add_argument('-o', '--output', help='Output image file path')
args = parser.parse_args()
if args.output:
    plt.savefig(args.output, dpi=300, bbox_inches='tight')
else:
    plt.show()
