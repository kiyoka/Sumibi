# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
"""
変換エラー率とコストの比較グラフをプロットする
"""
modules = ['gpt-4.1-mini', 'gpt-4.1', 'gpt-4o-mini', 'gpt-4o']
error_rate = [0, 0, 0, 0]      # 単位: %
costs = [0.00052, 0.00260, 0.000195, 0.00550]              # 単位: $/リクエスト ( sumibi_typical_convert_client.py でリクエスト500トークン、レスポンス200トークンを消費すると仮定した)

# 散布図の描画とモデル名の表示
plt.figure(figsize=(8, 6))
colors = ['skyblue', 'orange', 'red', 'green']
for name, cost, err, c in zip(modules, costs, error_rate, colors):
    plt.scatter(cost, err, s=150, color=c)
    plt.annotate(name,
                 xy=(cost, err),
                 xytext=(5, 5),
                 textcoords='offset points',
                 ha='left',
                 va='bottom')

# 軸ラベルとタイトル
plt.xlabel("Cost Per Request ($)")
plt.ylabel("Error Rate (%)")
plt.title("Error Rate vs Cost of LLM Model")
plt.grid(True)
plt.tight_layout()
plt.show()
