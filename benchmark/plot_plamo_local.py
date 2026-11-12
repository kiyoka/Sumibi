#!/usr/bin/env python3
"""Rebuild Issue #209 charts from saved local benchmark results (no API calls)."""
import argparse
import json
from pathlib import Path
from statistics import mean, median

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from run_plamo_local import DATASET_HASH, summarize

ROOT = Path(__file__).resolve().parent
MODELS = ("0.8B", "2B", "8B")
MODES = ("Hiragana", "Katakana")
COLORS = ("#226fa8", "#bf6316")
BASELINES = (
    ("gemma-3n-e2b-it-mlx", "Gemma 3n E2B MLX"),
    ("gemma-4-e4b", "Gemma 4 E4B"),
    ("gemma-4-26b-a4b-it-mlx", "Gemma 4 26B A4B MLX"),
    ("mozc", "Mozc (non-LLM)"),
)


def load_runs(models):
    runs = {}
    reference_cases = None
    for model in models:
        for mode in MODES:
            suffix = "-katakana" if mode == "Katakana" else ""
            path = ROOT / "issue209" / f"plamo-{model.lower()}{suffix}-full.jsonl"
            records = [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines()]
            meta, last = records[0], records[-1]
            rows = [r for r in records if r["kind"] == "result"]
            if (meta["dataset"] != "JWTD_v2/v1" or meta["sha256"] != DATASET_HASH
                    or meta["model"] != f"plamo-3-translate-{model.lower()}"
                    or meta["mode"] != f"{mode.lower()}_input"
                    or last["kind"] != "summary" or len(rows) != 200
                    or [r["position"] for r in rows] != list(range(1, 201))):
                raise ValueError(f"Unexpected dataset, model, mode or incomplete run: {path}")
            cases = [(r["index"], r["expected"]) for r in rows]
            if reference_cases is None:
                reference_cases = cases
            elif cases != reference_cases:
                raise ValueError(f"Different evaluation cases: {path}")
            summary = summarize(rows)
            if summary != {k: v for k, v in last.items() if k != "kind"}:
                raise ValueError(f"Summary mismatch: {path}")
            summary["copy_rate"] = mean(r["output"].strip() == r["input"] for r in rows)
            runs[model, mode] = summary
    return runs, [expected for _, expected in reference_cases]


def plot_input_modes(runs, models, output):
    fig, axes = plt.subplots(2, 2, figsize=(9, 6.5), layout="constrained")
    metrics = (
        ("Mean MinCER (%)", "mean_min_cer", 100, 145, 1),
        ("Exact match (%)", "exact_matches", 0.5, 10, 1),
        ("Unchanged input (%)", "copy_rate", 100, 100, 1),
        ("Mean response time (s)", "mean_seconds", 1, 1.8, 2),
    )
    for ax, (label, key, factor, top, digits) in zip(axes.flat, metrics):
        peak = 0
        for i, (mode, color) in enumerate(zip(MODES, COLORS)):
            values = [runs[model, mode][key] * factor for model in models]
            peak = max(peak, *values)
            bars = ax.bar([x + (i - 0.5) * 0.34 for x in range(len(models))], values,
                          width=0.34, color=color, label=mode)
            ax.bar_label(bars, fmt=f"%.{digits}f", padding=3, fontsize=9)
        ax.set_title(label, loc="left", fontweight="bold")
        ax.set_xticks(range(len(models)), models)
        ax.set_ylim(0, max(top, peak * 1.17))
        ax.set_axisbelow(True)
        ax.grid(axis="y", alpha=0.2)
        if key == "mean_min_cer":
            ax.legend(loc="upper right", frameon=False, fontsize=9)
    fig.suptitle("PLaMo 3 Translate — GGUF Q4_K_M, Japanese → Japanese\n"
                 "AJIMEE-Bench JWTD_v2/v1 · 200 items per condition", fontsize=13)
    fig.savefig(output, dpi=150)
    plt.close(fig)


def plot_local_comparison(runs, models, expected, output):
    fig, ax = plt.subplots(figsize=(8.3, 4.8), layout="constrained")
    points = []
    for model in models:
        row = runs[model, "Hiragana"]
        xy = (row["median_seconds"], 100 * row["mean_min_cer"])
        points.append(xy)
        ax.scatter(*xy, color=COLORS[0], marker="D", s=58)
        ax.annotate(f"PLaMo {model}", xy, xytext=(9, 2), textcoords="offset points")
    for key, label in BASELINES:
        path = ROOT / "result_ver2.4.0" / f"{key}_hiragana.json"
        rows = json.loads(path.read_text(encoding="utf-8"))
        if [r["expect"] for r in rows] != expected:
            raise ValueError(f"Different reference order: {path}")
        xy = (median(r["elapsed_sec"] for r in rows), 100 * mean(r["cer"] for r in rows))
        points.append(xy)
        ax.scatter(*xy, color="#616a72", s=45)
        ax.annotate(label, xy, xytext=(9, 2), textcoords="offset points")
    ax.set(xlim=(-0.04, max(2.85, max(x for x, _ in points) + 0.75)),
           ylim=(0, max(85, max(y for _, y in points) * 1.12)), xlabel="Median response time (s)",
           ylabel="Mean MinCER (%)",
           title="Local models · Hiragana · JWTD_v2/v1 (200 items)\n"
                 "Historical runs use different prompts and runtime settings")
    ax.grid(alpha=0.2)
    ax.set_axisbelow(True)
    fig.savefig(output, dpi=150)
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=ROOT / "issue209/figures")
    parser.add_argument("--models", choices=MODELS, nargs="+", default=MODELS)
    args = parser.parse_args()
    models = tuple(dict.fromkeys(args.models))
    runs, expected = load_runs(models)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    plot_input_modes(runs, models, args.output_dir / "input-modes.png")
    plot_local_comparison(runs, models, expected, args.output_dir / "local-hiragana.png")


if __name__ == "__main__":
    main()
