# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
"""
変換エラー率とコストの比較グラフをプロットする
"""
models = [
    'gpt-4.1-mini',
    'gpt-4.1',
    'gpt-4o-mini',
    'gpt-4o',
    'o4-mini',
    'deepseek-v3',
    'gemini-2.0-flash',
    'gemini-2.0-flash-lite',
    'gemini-2.5-pro-preview-05-06',
    'gemini-2.5-flash-preview-04-17',
]
error_rate = [ # 単位: %
    0.430172,
    0.218878,
    0.735027,
    0.197426,
    0.332583,
    0.592557,
    0.266489,
    0.356546,
    0.093759,
    0.137815,
]
costs = [ # 単位: $/リクエスト ( sumibi_typical_convert_client.py でリクエスト500トークン、レスポンス200トークンを消費すると仮定した)
    0.00052,
    0.00260,
    0.000195,
    0.00550,
    0.00143,
    0.000355,
    0.00013,
    0.0000975,
    0.002625,
    0.000195,
]
colors = [
    'palegreen',
    'lightgreen',
    'mediumspringgreen',
    'springgreen',
    'mediumseagreen',
    'pink',
    'lightgray',
    'silver',
    'darkgray',
    'gray',
    'dimgray',
]

# 各モデルの平均応答時間 (mean_elapsed_sec)
mean_elapsed_secs = [
    0.908558,   # gpt-4.1-mini
    0.985278,   # gpt-4.1
    0.912621,   # gpt-4o-mini
    0.821817,   # gpt-4o
    21.780880,  # o4-mini
    6.444244,   # deepseek-v3
    0.651836,   # gemini-2.0-flash
    0.709727,   # gemini-2.0-flash-lite
    28.210514,  # gemini-2.5-pro-preview-05-06
    6.640661,   # gemini-2.5-flash-preview-04-17
]
# 円の大きさスケール (s = mean_elapsed_sec * SCALE)
SCALE = 50

# 散布図の描画とモデル名の表示
plt.figure(figsize=(8, 6))

# 各モデルをコスト vs エラーレートでプロット (円の大きさは平均応答時間に比例)
for name, cost, err, c, sec in zip(models, costs, error_rate, colors, mean_elapsed_secs):
    pct = err * 100
    size = sec * SCALE
    plt.scatter(cost, pct, s=size, color=c)
    plt.annotate(name,
                 xy=(cost, pct),
                 xytext=(5, 5),
                 textcoords='offset points',
                 ha='left',
                 va='bottom',
                 clip_on=False)

# 軸ラベルとタイトル
plt.xlabel("Cost Per Request ($)")
plt.ylabel("Error Rate (%)")
plt.title("Error Rate vs Cost of LLM Model")
plt.grid(True)
# y 軸を 0%～110% の範囲に設定
plt.ylim(0, 110)
# ラベルがプロット外に出ても描画するためマージンを追加（x 軸のみ）
plt.margins(x=0.05)
plt.tight_layout()
plt.show()
