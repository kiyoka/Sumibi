EVAL    := ./AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json
SCRIPT  := sumibi_bench.py
GOOGLE_IME_NATIVE_SCRIPT := google_ime_native_bench.py
GOOGLE_IME_CGI_SCRIPT := google_ime_cgi_bench.py
RESULTS := gpt-3.5-turbe.json gpt-4.1-mini.json gpt-4.1.json gpt-4o-mini.json gpt-4o.json deepseek-v3.json o4-mini.json o3.json \
	   gemini-2.0-flash.json \
	   gemini-2.0-flash-lite.json \
	   gemini-2.5-pro.json \
	   gemini-2.5-flash.json \
	   gemini-3-pro-preview.json \
	   gemini-3-flash-preview.json \
	   gemma-3-12b-it-qat.json gemma-3-27b-it-qat.json gemma-3-27b-it-Q8_0.json japanese-stablelm-instruct-gamma-7b.json \
	   hermes-3-llama-3.2-3b.json llama-4-scout-17b-16e-instruct.json \
	   llama-3.3-70b-instruct.json \
	   stockmark-2-100b-instruct-beta@q3_k_m.json \
	   llm-jp-3.1-8x13b-instruct4.json llm-jp-3.1-13b-instruct4.json \
	   claude-3-7-sonnet-20250219.json \
	   claude-sonnet-4-20250514.json \
	   claude-sonnet-4-5-20250929.json \
	   claude-opus-4-20250514.json \
	   claude-opus-4-1-20250805.json \
	   gpt-5-mini.json \
	   gpt-5-nano.json \
	   gpt-5.json \
	   gpt-5.1.json \
           gpt-oss-120b\(low\).json \
           qwen3-vl-8b-instruct-mlx.json \
           gemini-3.1-flash-lite-preview.json \
           gpt-5.4.json

# Google IME benchmark results (hiragana input only)
GOOGLE_IME_NATIVE_RESULTS := google_ime_native.json
GOOGLE_IME_CGI_RESULTS := google_ime_cgi.json

# Notification script to run after benchmark completion
define run_notify
	@if [ -n "$$NOTIFY_SCRIPT" ]; then \
		echo "Running notification script: $$NOTIFY_SCRIPT"; \
		$$NOTIFY_SCRIPT; \
	fi
endef

.DEFAULT_GOAL := help
.PHONY: help all aggregate clean plots

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  all       generate all result JSONs"
	@echo "  google_ime_native  generate Google IME (native) benchmark results"
	@echo "  google_ime_cgi    generate Google IME (CGI API) benchmark results"
	@echo "  aggregate perform aggregation on all result JSONs"
	@echo "  clean     remove result JSONs"
	@echo "  plots     generate plot PNG images"

all: $(addprefix result_ver2.3.0/,$(RESULTS) result_ver2.4.0/,$(RESULTS))

aggregate:
	python3 aggregate_results.py result_ver2.3.0/*.json
	python3 aggregate_results.py result_ver2.4.0/*.json

result_ver2.3.0/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@" romaji_direct_input
	$(run_notify)

result_ver2.4.0/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@" romaji_direct_input
	$(run_notify)

# Hiragana input mode targets
result_ver2.3.0/%_hiragana.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

result_ver2.4.0/%_hiragana.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

# Katakana input mode targets
result_ver2.3.0/%_katakana.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@" katakana_input
	$(run_notify)

result_ver2.4.0/%_katakana.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@" katakana_input
	$(run_notify)

# Google IME (native / mozc_emacs_helper) benchmark targets
.PHONY: google_ime_native google_ime_cgi

google_ime_native: result_ver2.4.0/google_ime_native.json

result_ver2.4.0/google_ime_native.json: $(EVAL) $(GOOGLE_IME_NATIVE_SCRIPT)
	python3 $(GOOGLE_IME_NATIVE_SCRIPT) $(EVAL) "$@"
	$(run_notify)

result_ver2.4.0/google_ime_native_hiragana.json: $(EVAL) $(GOOGLE_IME_NATIVE_SCRIPT)
	python3 $(GOOGLE_IME_NATIVE_SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

# Google IME (CGI API) benchmark targets
google_ime_cgi: result_ver2.4.0/google_ime_cgi.json

result_ver2.4.0/google_ime_cgi.json: $(EVAL) $(GOOGLE_IME_CGI_SCRIPT)
	python3 $(GOOGLE_IME_CGI_SCRIPT) $(EVAL) "$@"
	$(run_notify)

result_ver2.4.0/google_ime_cgi_hiragana.json: $(EVAL) $(GOOGLE_IME_CGI_SCRIPT)
	python3 $(GOOGLE_IME_CGI_SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

# ---------------------------------------------------------------------------
# Issue #143: Context Length benchmark (mlx-community/gemma-4-26b-a4b-it, hiragana only)
# LM Studio で Context Length を変えて計測する
# 使い方: LM Studio の Context Length を各値に設定してからそれぞれ実行
# ---------------------------------------------------------------------------
CTX_MODEL   := mlx-community/gemma-4-26b-a4b-it
CTX_BASEURL := http://127.0.0.1:1234/v1
CTX_APIKEY  := lm-studio

.PHONY: ctx_bench ctx512 ctx1024 ctx2048 ctx4096 ctx8192 ctx_plot

ctx_bench: ctx512 ctx1024 ctx2048 ctx4096 ctx8192

ctx512: result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx512_hiragana.json
ctx1024: result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx1024_hiragana.json
ctx2048: result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx2048_hiragana.json
ctx4096: result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx4096_hiragana.json
ctx8192: result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx8192_hiragana.json

result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx512_hiragana.json: $(EVAL) $(SCRIPT)
	SUMIBI_AI_API_KEY=$(CTX_APIKEY) SUMIBI_AI_BASEURL=$(CTX_BASEURL) SUMIBI_AI_MODEL="$(CTX_MODEL)" SUMIBI_AI_TIMEOUT=120 \
	  python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx1024_hiragana.json: $(EVAL) $(SCRIPT)
	SUMIBI_AI_API_KEY=$(CTX_APIKEY) SUMIBI_AI_BASEURL=$(CTX_BASEURL) SUMIBI_AI_MODEL="$(CTX_MODEL)" SUMIBI_AI_TIMEOUT=120 \
	  python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx2048_hiragana.json: $(EVAL) $(SCRIPT)
	SUMIBI_AI_API_KEY=$(CTX_APIKEY) SUMIBI_AI_BASEURL=$(CTX_BASEURL) SUMIBI_AI_MODEL="$(CTX_MODEL)" SUMIBI_AI_TIMEOUT=120 \
	  python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx4096_hiragana.json: $(EVAL) $(SCRIPT)
	SUMIBI_AI_API_KEY=$(CTX_APIKEY) SUMIBI_AI_BASEURL=$(CTX_BASEURL) SUMIBI_AI_MODEL="$(CTX_MODEL)" SUMIBI_AI_TIMEOUT=120 \
	  python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx8192_hiragana.json: $(EVAL) $(SCRIPT)
	SUMIBI_AI_API_KEY=$(CTX_APIKEY) SUMIBI_AI_BASEURL=$(CTX_BASEURL) SUMIBI_AI_MODEL="$(CTX_MODEL)" SUMIBI_AI_TIMEOUT=120 \
	  python3 $(SCRIPT) $(EVAL) "$@" hiragana_input
	$(run_notify)

ctx_plot: ../images/plot_context_length_vs_response_time.png

../images/plot_context_length_vs_response_time.png: plot_context_length_vs_response_time.py \
    result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx512_hiragana.json \
    result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx1024_hiragana.json \
    result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx2048_hiragana.json \
    result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx4096_hiragana.json \
    result_ver2.4.0/gemma-4-26b-a4b-it-mlx_ctx8192_hiragana.json
	python3 plot_context_length_vs_response_time.py -o $@

# ---------------------------------------------------------------------------
# Generate plot images in the project's images directory
plots: ../images/plot_errorrate_vs_cost.png ../images/plot_errorrate_vs_cost_zoomed.png ../images/plot_errorrate_vs_paramsize.png ../images/plot_errorrate_vs_paramsize_zoomed.png ../images/plot_mean_response_time.png ../images/plot_errorrate_vs_inputtype.png ../images/plot_errorrate_vs_inputtype_hiragana.png ../images/plot_errorrate_vs_cost_1000x600.png ../images/plot_errorrate_vs_cost_zoomed_1000x600.png ../images/plot_errorrate_vs_paramsize_1000x600.png ../images/plot_errorrate_vs_paramsize_zoomed_1000x600.png ../images/plot_mean_response_time_1000x1800.png ../images/plot_errorrate_vs_inputtype_1000x600.png ../images/plot_errorrate_vs_inputtype_hiragana_1000x600.png

../images/plot_errorrate_vs_cost.png: plot_errorrate_vs_cost.py
	python3 $< --range 0 -o $@

../images/plot_errorrate_vs_cost_zoomed.png: plot_errorrate_vs_cost.py
	python3 $< --range 1 -o $@

../images/plot_errorrate_vs_paramsize.png: plot_errorrate_vs_paramsize.py
	python3 $< -o $@

../images/plot_errorrate_vs_paramsize_zoomed.png: ../images/plot_errorrate_vs_paramsize.png

../images/plot_mean_response_time.png: plot_mean_response_time.py
	python3 $< -o $@

../images/plot_errorrate_vs_inputtype.png: plot_errorrate_vs_inputtype.py
	python3 $< -o $@

../images/plot_errorrate_vs_inputtype_hiragana.png: plot_errorrate_vs_inputtype_hiragana.py
	python3 $< -o $@

../images/plot_errorrate_vs_cost_1000x600.png: ../images/plot_errorrate_vs_cost.png
	convert $< -resize 1000x600 $@

../images/plot_errorrate_vs_cost_zoomed_1000x600.png: ../images/plot_errorrate_vs_cost_zoomed.png
	convert $< -resize 1000x600 $@

../images/plot_errorrate_vs_paramsize_1000x600.png: ../images/plot_errorrate_vs_paramsize.png
	convert $< -resize 1000x600 $@

../images/plot_errorrate_vs_paramsize_zoomed_1000x600.png: ../images/plot_errorrate_vs_paramsize_zoomed.png
	convert $< -resize 1000x600 $@

../images/plot_mean_response_time_1000x1800.png: ../images/plot_mean_response_time.png
	convert $< -resize 1000x1800! $@

../images/plot_errorrate_vs_inputtype_1000x600.png: ../images/plot_errorrate_vs_inputtype.png
	convert $< -resize 1000x600 $@

../images/plot_errorrate_vs_inputtype_hiragana_1000x600.png: ../images/plot_errorrate_vs_inputtype_hiragana.png
	convert $< -resize 1000x600 $@
