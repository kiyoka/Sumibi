# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
"""
変換エラー率とコストの比較グラフをプロットする
"""
modules = ['gpt-4.1-mini', 'gpt-4.1', 'gpt-4o-mini', 'gpt-4o']
error_rate = [0, 0, 0, 0]      # 単位: %
costs = [0.00052, 0.00260, 0.000195, 0.00550]              # 単位: $/リクエスト ( sumibi_typical_convert_client.py でリクエスト500トークン、レスポンス200トークンを消費すると仮定した)

# 散布図の描画
plt.figure(figsize=(8, 6))
plt.scatter(costs, error_rate, s=150, c=['skyblue', 'orange', 'red', 'green'])

# ラベル付け
for i, name in enumerate(modules):
    plt.text(costs[i] + 0.02, error_rate[i], name)

# 軸ラベルとタイトル
plt.xlabel("Cost Per Request ($)")
plt.ylabel("Error Rate (%)")
plt.title("Error Rate vs Cost of LLM Privider")
plt.grid(True)
plt.tight_layout()
plt.show()
