EVAL    := ./AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json
SCRIPT  := sumibi_bench.py
RESULTS := gpt-3.5-turbe.json gpt-4.1-mini.json gpt-4.1.json gpt-4o-mini.json gpt-4o.json deepseek-v3.json o4-mini.json o3.json \
	   gemini-2.0-flash.json \
	   gemini-2.0-flash-lite.json \
	   gemini-2.5-pro.json \
	   gemini-2.5-flash.json \
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
	   gpt-5-mini\(medium\).json \
	   gpt-5-mini\(minimal\).json \
	   gpt-5-mini\(minimal\+low\).json \
	   gpt-5-nano\(minimal\).json \
	   gpt-5\(minimal\).json \
	   gpt-5\(minimal\+low\).json \
	   gpt-5\(medium\).json \
           gpt-oss-120b\(low\).json

.DEFAULT_GOAL := help
.PHONY: help all aggregate clean plots

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  all       generate all result JSONs"
	@echo "  aggregate perform aggregation on all result JSONs"
	@echo "  clean     remove result JSONs"
	@echo "  plots     generate plot PNG images"

all: $(addprefix result_ver2.3.0/,$(RESULTS) result_ver2.4.0/,$(RESULTS))

aggregate:
	python3 aggregate_results.py result_ver2.3.0/*.json
	python3 aggregate_results.py result_ver2.4.0/*.json

result_ver2.3.0/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@"

result_ver2.4.0/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) "$@"

# Generate plot images in the project's images directory
plots: ../images/plot_errorrate_vs_cost.png ../images/plot_errorrate_vs_cost_zoomed.png ../images/plot_errorrate_vs_paramsize.png ../images/plot_mean_response_time.png ../images/plot_errorrate_vs_cost_1000x600.png ../images/plot_errorrate_vs_cost_zoomed_1000x600.png ../images/plot_errorrate_vs_paramsize_1000x600.png ../images/plot_mean_response_time_1000x600.png

../images/plot_errorrate_vs_cost.png: plot_errorrate_vs_cost.py
	python3 $< --range 0 -o $@

../images/plot_errorrate_vs_cost_zoomed.png: plot_errorrate_vs_cost.py
	python3 $< --range 1 -o $@

../images/plot_errorrate_vs_paramsize.png: plot_errorrate_vs_paramsize.py
	python3 $< -o $@

../images/plot_mean_response_time.png: plot_mean_response_time.py
	python3 $< -o $@

../images/plot_errorrate_vs_cost_1000x600.png: ../images/plot_errorrate_vs_cost.png
	convert $< -resize 1000x600 $@

../images/plot_errorrate_vs_cost_zoomed_1000x600.png: ../images/plot_errorrate_vs_cost_zoomed.png
	convert $< -resize 1000x600 $@

../images/plot_errorrate_vs_paramsize_1000x600.png: ../images/plot_errorrate_vs_paramsize.png
	convert $< -resize 1000x600 $@

../images/plot_mean_response_time_1000x600.png: ../images/plot_mean_response_time.png
	convert $< -resize 1000x600 $@
