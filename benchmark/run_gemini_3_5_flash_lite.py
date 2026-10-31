#!/usr/bin/env python3
"""Issue #201: Gemini 3.5 Flash-Lite, minimal thinking, user-supplied key only."""
from datetime import date
import run_gemini_3_8_flash as shared

MODEL = "gemini-3.5-flash-lite"
PRICES = {"input": 0.30, "cached_input": 0.03, "output": 2.50}


def main():
    shared.main(model=MODEL, prices=PRICES, reasoning_effort="minimal", temperature=None,
                pricing_valid_through=date(2026, 12, 31), description=__doc__)


if __name__ == "__main__":
    main()
