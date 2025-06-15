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
    'gemma-3-12b-it-qat',
    'gemma-3-27b-it-qat',
    'gemma-3-27b-it-Q8_0',
    'japanese-stablelm-instruct-gamma-7b',
    'hermes-3-llama-3.2-3b',
    'llama-4-scout-17b-16e-instruct',
    'llama-3.3-70b-instruct',
    'stockmark-2-100b-instruct-beta@q3_k_m',
    'llm-jp-3.1-8x13b-instruct4',
    'llm-jp-3.1-13b-instruct4'
]
# エラー率 (mean CER)
error_rates = [
    0.844450,
    0.803389,
    0.839824,
    0.939121,
    0.989908,
    0.874311,
    0.902066,
    0.891659,
    0.735276,
    0.903498,
]
# パラメータ数 (ビリオン単位)
param_sizes = [
    12,
    27,
    27,
    7,
    3,
    17,
    70,
    100,
    104,
    13,
]
markers = [
    's',
    's',
    's',
    'o',
    'o',
    's',
    's',
    's',
    'o',
    'o',
]

# カラー設定 (モデル数に合わせて調整)
base_colors = [
    'tab:blue',    # 青
    'tab:orange',  # オレンジ
    'tab:green',   # 緑
    'tab:red',     # 赤
    'tab:purple',  # 紫
    'tab:brown',   # 茶
    'tab:pink',    # ピンク
    'tab:gray',    # グレー
    'tab:cyan',    # シアン
    'tab:olive',   # オリーブ (for 13b)
]
colors = base_colors[:len(modules)]

plt.figure(figsize=(8, 6))
# 散布図: x=Parameter Size (Billion), y=Error Rate (%)
for name, err, size, marker, c in zip(modules, error_rates, param_sizes, markers, colors):
    pct = err * 100
    plt.scatter(size, pct, s=150, color=c, marker=marker)
    plt.annotate(name,
                 xy=(size, pct),
                 xytext=(5, 5),
                 textcoords='offset points',
                 ha='left', va='bottom', clip_on=False)

plt.xlabel('Parameter Size (Billion)')
plt.ylabel('Error Rate (%)')
plt.title('Error Rate vs Parameter Size for Local LLMs')
plt.grid(True)
# y 軸を 70%～110% の範囲に設定
plt.ylim(70, 110)
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
