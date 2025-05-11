#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
plot_mean_response_time.py

各モデルの平均応答時間（mean_elapsed_sec）をバーグラフでプロットする
"""
import matplotlib.pyplot as plt

## モデル名と平均応答時間 (mean_elapsed_sec) 値
# 両プロットで同じ順序・カラーを使用
modules = [
    'gpt-4.1-mini',
    'gpt-4.1',
    'gpt-4o-mini',
    'gpt-4o',
    'o4-mini',
    'deepseek-v3',
]
# 各モデルの mean_elapsed_sec
mean_elapsed_secs = [
    0.908558,   # gpt-4.1-mini
    0.985278,   # gpt-4.1
    0.912621,   # gpt-4o-mini
    0.821817,   # gpt-4o
    21.780880,  # o4-mini
    6.444244,   # deepseek-v3
]

# カラー設定（モデルごと, plot_errorrate_vs_cost.py と同順序）
colors = ['skyblue', 'orange', 'gray', 'green', 'pink', 'red']

import matplotlib.pyplot as plt

# バーグラフの描画
plt.figure(figsize=(10, 6))
bars = plt.bar(modules, mean_elapsed_secs, color=colors)

# 各バーに値を表示 (平均応答時間)
for bar, val in zip(bars, mean_elapsed_secs):
    height = bar.get_height()
    plt.annotate(f'{val:.2f}',
                 xy=(bar.get_x() + bar.get_width() / 2, height),
                 xytext=(0, 5),
                 textcoords='offset points',
                 ha='center', va='bottom')

plt.xlabel('Model')
plt.ylabel('Mean Response Time (sec)')
plt.title('Mean Response Time for Each Model')
plt.xticks(rotation=45)
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.tight_layout()
plt.show()
