EVAL    := ./AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json
SCRIPT  := sumibi_bench.py
RESULTS := gpt-4.1-mini.json gpt-4.1.json gpt-4o-mini.json gpt-4o.json deepseek-v3.json o4-mini.json gemma-3-12b-it-qat.json

.DEFAULT_GOAL := help
.PHONY: help all aggregate clean

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  all       generate all result JSONs"
	@echo "  aggregate perform aggregation on all result JSONs"
	@echo "  clean     remove result JSONs"

all: $(addprefix result/,$(RESULTS))

aggregate: all
	python3 aggregate_results.py result/*.json

result/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) $@

clean:
	rm -f result/*.json
