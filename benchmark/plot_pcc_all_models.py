#!/usr/bin/env python3
"""Plot iOS PCC among every saved AJIMEE v1 measurement configuration."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from generate_pcc_comparison import INPUT_TYPES, PCC_NAME, load_comparison


def plot(output: Path) -> None:
    tables = load_comparison()
    english_labels = {"romaji": "Romaji", "hiragana": "Hiragana",
                      "katakana": "Katakana"}
    figure, axes = plt.subplots(1, 3, figsize=(15, 5.5), sharey=True)
    for axis, kind in zip(axes, INPUT_TYPES):
        entries = tables[kind]
        current = [e for e in entries if e["version"] == "v2.4.0"]
        older = [e for e in entries if e["version"] == "v2.3.0"]
        pcc = next(e for e in entries if e["model"] == PCC_NAME)
        axis.scatter([e["cer"] * 100 for e in current],
                     [e["p95_mean"] for e in current],
                     s=24, alpha=0.7, color="#4676b7", label="v2.4.0")
        axis.scatter([e["cer"] * 100 for e in older],
                     [e["p95_mean"] for e in older],
                     s=32, alpha=0.7, marker="x", color="#888888",
                     label="v2.3.0 only")
        axis.scatter([pcc["cer"] * 100], [pcc["p95_mean"]],
                     s=240, marker="*", color="#f18a00", edgecolor="black",
                     linewidth=0.6, zorder=10, label="iOS PCC")
        axis.annotate("PCC", (pcc["cer"] * 100, pcc["p95_mean"]),
                      xytext=(6, 7), textcoords="offset points", fontsize=9,
                      fontweight="bold")
        axis.axhline(2, color="#aa5555", linestyle="--", linewidth=1,
                     label="2-second guideline")
        axis.set_title(f"{english_labels[kind]} input ({len(entries)} runs)")
        axis.set_xlabel("Mean CER (%) - lower is better")
        axis.set_yscale("log")
        axis.set_xlim(-2, 102)
        axis.grid(alpha=0.25)
    axes[0].set_ylabel("Mean latency, below p95 (seconds, log scale)")
    axes[-1].legend(loc="upper right", fontsize=8)
    figure.suptitle("AJIMEE v1 / 200 cases: iOS PCC vs all measured models")
    figure.text(0.5, 0.01,
                "Reference comparison only: API, hardware and prompt structure differ; v2 (628 cases) excluded.",
                ha="center", fontsize=9)
    figure.tight_layout(rect=(0, 0.05, 1, 0.94))
    figure.savefig(output, dpi=150)
    print(f"Saved {output}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-o", "--output", type=Path,
                        default=Path(__file__).parent.parent / "images/plot_pcc_all_models_v1.png")
    args = parser.parse_args()
    plot(args.output)


if __name__ == "__main__":
    main()
