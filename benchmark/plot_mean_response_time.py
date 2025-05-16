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
# 応答時間が長い順にソート
sorted_idx = sorted(range(len(mean_elapsed_secs)), key=lambda i: mean_elapsed_secs[i], reverse=True)
modules = [modules[i] for i in sorted_idx]
mean_elapsed_secs = [mean_elapsed_secs[i] for i in sorted_idx]
colors = [colors[i] for i in sorted_idx]
error_rates = [error_rates[i] for i in sorted_idx]

import matplotlib.pyplot as plt
# メイン軸：平均応答時間を横長バーでプロット
fig, ax1 = plt.subplots(figsize=(10, 6))
bars = ax1.barh(modules, mean_elapsed_secs, color=colors)

ax1.set_xlabel('Mean Response Time (sec)')
ax1.set_ylabel('Model')
ax1.grid(axis='x', linestyle='--', alpha=0.7)

# 補助軸：エラー率を折れ線でプロット
ax2 = ax1.twiny()
# エラー率をパーセントに変換
err_pct = [e * 100 for e in error_rates]
# バーの中心 y 座標を取得
y_centers = [bar.get_y() + bar.get_height() / 2 for bar in bars]
ax2.plot(err_pct, y_centers, 'o-', color='firebrick', label='Error Rate')
ax2.set_xlabel('Error Rate (%)')
ax2.set_xlim(0, 100)
ax2.set_xticks(range(0, 101, 10))
ax2.xaxis.set_label_position('top')
ax2.xaxis.set_ticks_position('top')
ax2.legend(loc='lower right')

# 平均応答時間の値をバーの右側にアノテーション
for bar, val in zip(bars, mean_elapsed_secs):
    x = bar.get_width()
    y = bar.get_y() + bar.get_height() / 2
    ax1.annotate(f'{val:.2f}s',
                 xy=(x, y),
                 xytext=(5, 0),
                 textcoords='offset points',
                 ha='left', va='center')

plt.tight_layout()
plt.show()
