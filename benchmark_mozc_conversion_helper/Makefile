# Mozc Conversion Helper Benchmark Makefile

.PHONY: setup download-data download-wikipedia test-mozc run-benchmark run-benchmark-hw1 run-benchmark-hw2 run-benchmark-hw3 plot-results clean help

# デフォルトターゲット
help:
	@echo "Available targets:"
	@echo "  setup                        - 環境をセットアップ"
	@echo "  download-data                - Wikipedia 日本語ダンプ取得手順 (自動取得を試行)"
	@echo "  download-wikipedia           - download-data と同義"
	@echo "  test-mozc                    - Mozcクライアントをテスト"
	@echo "  run-benchmark                - ベンチマークを実行 (HARDWARE=hardware1 or hardware2 or hardware3, PROMPT_MODE=rerank-all or optimize)"
	@echo "  run-benchmark-optimize       - 最適化モード（1つ選択+短縮文脈）でベンチマーク実行"
	@echo "  run-benchmark-rerank-all     - 全候補並び替えモードでベンチマーク実行"
	@echo "  run-benchmark-hw1            - hardware1でベンチマークを実行"
	@echo "  run-benchmark-hw1-optimize   - hardware1で最適化モードを実行"
	@echo "  run-benchmark-hw1-rerank-all - hardware1で全候補並び替えモードを実行"
	@echo "  run-benchmark-hw2            - hardware2でベンチマークを実行"
	@echo "  run-benchmark-hw2-optimize   - hardware2で最適化モードを実行"
	@echo "  run-benchmark-hw2-rerank-all - hardware2で全候補並び替えモードを実行"
	@echo "  run-benchmark-hw3            - hardware3 (MacBook Air)でベンチマークを実行"
	@echo "  run-benchmark-hw3-optimize   - hardware3で最適化モードを実行"
	@echo "  run-benchmark-hw3-rerank-all - hardware3で全候補並び替えモードを実行"
	@echo "  plot-results                 - ベンチマーク結果をグラフ化"
	@echo "  clean                        - 生成ファイルをクリーンアップ"

# 環境セットアップ
setup:
	@echo "Setting up environment..."
	python3 -m venv venv
	./venv/bin/pip install --upgrade pip
	./venv/bin/pip install -r requirements.txt
	@echo "Setup completed!"

# 青空文庫データのダウンロード
download-data:
	@echo "Preparing Wikipedia Japanese dump..."
	python3 scripts/download_wikipedia_dump.py || true

download-wikipedia: download-data

# Mozcシミュレータのテスト
test-mozc:
	@echo "Testing Mozc simulator..."
	python3 mozc_helper/mozc_simulator.py

# ベンチマーク実行 (要: OPENAI_API_KEY環境変数)
run-benchmark:
	@if [ -z "$$OPENAI_API_KEY" ]; then \
		echo "Error: OPENAI_API_KEY environment variable is required"; \
		echo "Please set it like: export OPENAI_API_KEY='your-api-key'"; \
		echo "For local LLM: export OPENAI_API_KEY='dummy' OPENAI_BASEURL='http://localhost:1234/'"; \
		exit 1; \
	fi
	@if [ -n "$(HARDWARE)" ]; then \
		mkdir -p results/$(HARDWARE); \
		echo "Running LLM selection benchmark on $(HARDWARE)..."; \
	else \
		mkdir -p results; \
		echo "Running LLM selection benchmark..."; \
	fi
	@if [ -n "$$OPENAI_BASEURL" ]; then \
		echo "Using local LLM endpoint: $$OPENAI_BASEURL"; \
	fi
	HARDWARE=$(HARDWARE) OPENAI_MODEL=$${OPENAI_MODEL:-gpt-5} PROMPT_MODE=$${PROMPT_MODE:-rerank-all} python3 scripts/llm_selection_benchmark.py
	@if [ -n "$$NOTIFY_SCRIPT" ]; then \
		echo "Running notification script: $$NOTIFY_SCRIPT"; \
		$$NOTIFY_SCRIPT; \
	fi

# ベンチマーク実行 (optimizeモード: 最適化モード)
run-benchmark-optimize:
	@$(MAKE) run-benchmark PROMPT_MODE=optimize

# ベンチマーク実行 (rerank-allモード: 全候補並び替え)
run-benchmark-rerank-all:
	@$(MAKE) run-benchmark PROMPT_MODE=rerank-all

# hardware1でベンチマーク実行
run-benchmark-hw1:
	$(MAKE) run-benchmark HARDWARE=hardware1

# hardware1でベンチマーク実行 (optimizeモード)
run-benchmark-hw1-optimize:
	$(MAKE) run-benchmark HARDWARE=hardware1 PROMPT_MODE=optimize

# hardware1でベンチマーク実行 (rerank-allモード)
run-benchmark-hw1-rerank-all:
	$(MAKE) run-benchmark HARDWARE=hardware1 PROMPT_MODE=rerank-all

# hardware2でベンチマーク実行
run-benchmark-hw2:
	$(MAKE) run-benchmark HARDWARE=hardware2

# hardware2でベンチマーク実行 (optimizeモード)
run-benchmark-hw2-optimize:
	$(MAKE) run-benchmark HARDWARE=hardware2 PROMPT_MODE=optimize

# hardware2でベンチマーク実行 (rerank-allモード)
run-benchmark-hw2-rerank-all:
	$(MAKE) run-benchmark HARDWARE=hardware2 PROMPT_MODE=rerank-all

# hardware3でベンチマーク実行 (MacBook Air)
run-benchmark-hw3:
	$(MAKE) run-benchmark HARDWARE=hardware3

# hardware3でベンチマーク実行 (optimizeモード)
run-benchmark-hw3-optimize:
	$(MAKE) run-benchmark HARDWARE=hardware3 PROMPT_MODE=optimize

# hardware3でベンチマーク実行 (rerank-allモード)
run-benchmark-hw3-rerank-all:
	$(MAKE) run-benchmark HARDWARE=hardware3 PROMPT_MODE=rerank-all

# ベンチマーク結果をグラフ化
plot-results:
	@echo "Plotting benchmark results..."
	python3 scripts/plot_benchmark_results.py

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
