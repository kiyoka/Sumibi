EVAL    := ./AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json
SCRIPT  := sumibi_bench.py
RESULTS := gpt-3.5-turbe.json gpt-4.1-mini.json gpt-4.1.json gpt-4o-mini.json gpt-4o.json deepseek-v3.json o4-mini.json \
	   gemini-2.0-flash.json gemini-2.0-flash-lite.json gemini-2.5-pro-preview-05-06.json gemini-2.5-flash-preview-04-17.json \
	   gemma-3-12b-it-qat.json japanese-stablelm-instruct-gamma-7b.json

.DEFAULT_GOAL := help
.PHONY: help all aggregate clean plots

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  all       generate all result JSONs"
	@echo "  aggregate perform aggregation on all result JSONs"
	@echo "  clean     remove result JSONs"
	@echo "  plots     generate plot PNG images"

all: $(addprefix result/,$(RESULTS))

aggregate:
	python3 aggregate_results.py result/*.json

result/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) $@

clean:
	rm -f result/*.json

# Generate plot images in the project's images directory
plots: ../images/plot_errorrate_vs_cost.png ../images/plot_errorrate_vs_paramsize.png ../images/plot_mean_response_time.png

../images/plot_errorrate_vs_cost.png: plot_errorrate_vs_cost.py
	python3 $< -o $@

../images/plot_errorrate_vs_paramsize.png: plot_errorrate_vs_paramsize.py
	python3 $< -o $@

../images/plot_mean_response_time.png: plot_mean_response_time.py
	python3 $< -o $@
