#!/usr/bin/env python3
"""Issue #202: Gemini 4 Argon, low thinking, user-supplied Gemini key only."""
from datetime import date
import run_gemini_3_8_flash as shared

MODEL = "gemini-4-argon"
PRICES = {"input": 2.00, "cached_input": 0.10, "output": 10.00}


def main():
    shared.main(model=MODEL, prices=PRICES, reasoning_effort="low", temperature=None,
                pricing_valid_through=date(2026, 11, 30), description=__doc__)


if __name__ == "__main__":
    main()
