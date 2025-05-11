EVAL    := ./AJIMEE-Bench/JWTD_v2/v1/evaluation_items.json
SCRIPT  := sumibi_bench.py
RESULTS := gpt-4.1-mini.json gpt-4.1.json gpt-4o-mini.json gpt-4o.json

.DEFAULT_GOAL := help
.PHONY: help all clean

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  all     generate all result JSONs"
	@echo "  clean   remove result JSONs"

all: $(addprefix result/,$(RESULTS))

result/%.json: $(EVAL) $(SCRIPT)
	python3 $(SCRIPT) $(EVAL) $@

clean:
	rm -f result/*.json
