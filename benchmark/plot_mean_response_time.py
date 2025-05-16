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
    'gpt-3.5-turbe',
    'gpt-4.1-mini',
    'gpt-4.1',
    'gpt-4o-mini',
    'gpt-4o',
    'o4-mini',
    'deepseek-v3',
    'gemini-2.0-flash',
    'gemini-2.0-flash-lite',
    'gemini-2.5-flash-preview-04-17',
    'gemini-2.5-pro-preview-05-06',
]
# 各モデルの mean_elapsed_sec
mean_elapsed_secs = [
    0.836779,   # gpt-3.5-turbe
    0.908558,   # gpt-4.1-mini
    0.985278,   # gpt-4.1
    0.912621,   # gpt-4o-mini
    0.821817,   # gpt-4o
    21.780880,  # o4-mini
    6.444244,   # deepseek-v3
    0.651836,   # gemini-2.0-flash
    0.709727,   # gemini-2.0-flash-lite
    6.640661,   # gemini-2.5-flash-preview-04-17
    28.210514,  # gemini-2.5-pro-preview-05-06
]

# カラー設定（モデルごと, plot_errorrate_vs_cost.py と同順序）
colors = [
    'palegreen',
    'lightgreen',
    'mediumspringgreen',
    'springgreen',
    'mediumseagreen',
    'springgreen',
    'pink',
    'lightgray',
    'silver',
    'darkgray',
    'gray',
]

import matplotlib.pyplot as plt

## バーグラフの描画 (左軸: 平均応答時間, 注釈にエラーレートを表示)
plt.figure(figsize=(10, 6))
bars = plt.bar(modules, mean_elapsed_secs, color=colors)

## 各モデルのエラーレート (mean_cer)
error_rates = [
    0.721777,  # gpt-3.5-turbe
    0.430172,  # gpt-4.1-mini
    0.218878,  # gpt-4.1
    0.735027,  # gpt-4o-mini
    0.197426,  # gpt-4o
    0.332583,  # o4-mini
    0.592557,  # deepseek-v3
    0.266489,  # gemini-2.0-flash
    0.356546,  # gemini-2.0-flash-lite
    0.137815,  # gemini-2.5-flash-preview-04-17
    0.093759,  # gemini-2.5-pro-preview-05-06
]

# 各バーに値とエラーレートを表示
for bar, val, err in zip(bars, mean_elapsed_secs, error_rates):
    height = bar.get_height()
    plt.annotate(f'{val:.2f}s\n{err*100:.1f}%',
                 xy=(bar.get_x() + bar.get_width() / 2, height),
                 xytext=(0, 5),
                 textcoords='offset points',
                 ha='center', va='bottom')

plt.xlabel('Model')
plt.ylabel('Mean Response Time (sec)')
plt.title('Mean Response Time and Each Model')
plt.xticks(rotation=45)
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.tight_layout()
plt.show()
