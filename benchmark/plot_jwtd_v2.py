#!/usr/bin/env python3
"""Plot only validated Issue #205 summaries; never scan legacy result folders."""
import argparse
import json
from pathlib import Path

from jwtd_v2 import IDENTITY, MODES, SCORING


def plot(summary, directory):
    if (summary.get("schema") != "sumibi-jwtd-v2-summary-1"
            or summary.get("dataset") != IDENTITY or summary.get("scoring") != SCORING):
        raise ValueError("Not the pinned JWTD_v2/v2 summary")
    models = summary["models"]
    if not models or any(not m.startswith("gemini-") or m == "gemini-3.7-flash" for m in models):
        raise ValueError("Only independently measured Gemini models may be plotted")
    if directory.exists():
        raise ValueError("Choose a fresh plot directory; no overwrites")
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    directory.mkdir(parents=True)
    for mode in MODES:
        names = sorted(models, key=lambda m: models[m]["modes"][mode]["quality"]["overall"]["MinCER"])
        fig, axes = plt.subplots(1, 2, figsize=(13, 5))
        for axis, metric in zip(axes, ("cer", "latency")):
            values = [(models[m]["modes"][mode]["quality"]["overall"]["MinCER"] * 100
                       if metric == "cer" else models[m]["modes"][mode]["latency"]["mean"]) for m in names]
            axis.barh(names, values, color="steelblue")
            axis.invert_yaxis()
            axis.set_xlabel("Raw MinCER (%)" if metric == "cer" else "Mean response time (all cases, sec)")
        fig.suptitle(f"JWTD_v2/v2 | 628 cases | Gemini only | {mode}\n"
                     f"{IDENTITY['revision'][:12]} / dataset {IDENTITY['sha256'][:12]}")
        fig.tight_layout()
        fig.savefig(directory / (mode + ".png"), dpi=150)
        plt.close(fig)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("summary", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    plot(json.loads(args.summary.read_text()), args.output_dir)
