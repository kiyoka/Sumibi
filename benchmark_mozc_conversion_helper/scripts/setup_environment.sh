#!/bin/bash

# Mozc変換候補選択ベンチマーク環境のセットアップ

echo "=== Mozc Conversion Helper Benchmark Setup ==="

# Pythonの仮想環境を作成
echo "Creating Python virtual environment..."
python3 -m venv venv

# 仮想環境を有効化
echo "Activating virtual environment..."
source venv/bin/activate

# 必要なパッケージをインストール
echo "Installing required packages..."
pip install --upgrade pip
pip install -r requirements.txt

echo "Setup completed!"
echo ""
echo "To run the benchmark:"
echo "1. source venv/bin/activate"
echo "2. export OPENAI_API_KEY='your-api-key-here'"
echo "3. export OPENAI_MODEL='gpt-4o-mini'  # optional"
echo "4. python scripts/llm_selection_benchmark.py"