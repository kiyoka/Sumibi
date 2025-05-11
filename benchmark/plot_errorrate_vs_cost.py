# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
"""
変換エラー率とコストの比較グラフをプロットする
"""
modules = ['gpt-4.1-mini', 'gpt-4.1', 'gpt-4o-mini', 'gpt-4o', 'o4-mini', 'deepseek-v3']
error_rate = [0.430172, 0.218878, 0.735027, 0.197426, 0.332583, 0.592557]      # 単位: %
costs = [0.00052, 0.00260, 0.000195, 0.00550, 0.00143, 0.000355]              # 単位: $/リクエスト ( sumibi_typical_convert_client.py でリクエスト500トークン、レスポンス200トークンを消費すると仮定した)

# 散布図の描画とモデル名の表示
plt.figure(figsize=(8, 6))
colors = ['skyblue', 'orange', 'gray', 'green', 'pink', 'red']
for name, cost, err, c in zip(modules, costs, error_rate, colors):
    # Error Rate をパーセント表示に変換してプロット
    pct = err * 100
    plt.scatter(cost, pct, s=150, color=c)
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
# ラベルがプロット外に出ても描画するためマージンを追加
plt.margins(x=0.05, y=0.1)
plt.tight_layout()
plt.show()
