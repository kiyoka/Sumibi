# Mozc Conversion Helper Benchmark Makefile

.PHONY: setup download-data test-mozc run-benchmark clean help

# デフォルトターゲット
help:
	@echo "Available targets:"
	@echo "  setup          - 環境をセットアップ"
	@echo "  download-data  - 青空文庫データをダウンロード"
	@echo "  test-mozc      - Mozcクライアントをテスト"
	@echo "  run-benchmark  - ベンチマークを実行"
	@echo "  clean          - 生成ファイルをクリーンアップ"

# 環境セットアップ
setup:
	@echo "Setting up environment..."
	python3 -m venv venv
	./venv/bin/pip install --upgrade pip
	./venv/bin/pip install -r requirements.txt
	@echo "Setup completed!"

# 青空文庫データのダウンロード
download-data:
	@echo "Downloading Aozora Bunko data..."
	python3 scripts/download_aozora.py

# Mozcシミュレータのテスト
test-mozc:
	@echo "Testing Mozc simulator..."
	python3 mozc_helper/mozc_simulator.py

# ベンチマーク実行 (要: OPENAI_API_KEY環境変数)
run-benchmark:
	@if [ -z "$$OPENAI_API_KEY" ]; then \
		echo "Error: OPENAI_API_KEY environment variable is required"; \
		echo "Please set it like: export OPENAI_API_KEY='your-api-key'"; \
		exit 1; \
	fi
	@echo "Running LLM selection benchmark..."
	python3 scripts/llm_selection_benchmark.py

# クリーンアップ
clean:
	rm -rf venv/
	rm -rf results/
	rm -rf __pycache__/
	rm -rf */__pycache__/
	rm -rf */*/__pycache__/
	@echo "Cleanup completed!"

# 開発用: 必要なディレクトリを作成
init-dirs:
	mkdir -p data results

# 全体のセットアップ (初回実行用)
init: init-dirs setup download-data test-mozc
	@echo "Initial setup completed!"
	@echo "To run benchmark:"
	@echo "  export OPENAI_API_KEY='your-api-key'"
	@echo "  make run-benchmark"