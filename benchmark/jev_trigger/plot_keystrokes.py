#!/usr/bin/env python3
"""Plot measured per-key Jev scores; never interpolates missing observations."""
import argparse
import json
import math
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("result", type=Path)
    parser.add_argument("--case-id", default="greeting_thanks")
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args(argv)
    data = json.loads(args.result.read_text(encoding="utf-8"))
    from bench import CASES, ADDITIONAL_CASES, ENGLISH_CASES, PUNCTUATION_CASES, EXCLUDED_CONTEXTS
    cases = [case for path in (CASES, ADDITIONAL_CASES, ENGLISH_CASES, PUNCTUATION_CASES)
             for case in json.loads(path.read_text(encoding="utf-8"))]
    case = next((c for c in cases if c["id"] == args.case_id), None)
    if case is None:
        parser.error("未知のcase-idです。")
    text = case["keys"]
    rows = [r for r in data.get("rows", []) if r["id"] == args.case_id]
    if data["backend"] != "jev" or [r["index"] for r in rows] != list(range(1, len(text) + 1)):
        parser.error(f"Jevの全{len(text)}打鍵の実測値が必要です（--include-rows）。")
    if any(r["buffer"] != text[:r["index"]] for r in rows):
        parser.error("対象例文のバッファが一致しません。")
    excluded = case["context"] in EXCLUDED_CONTEXTS
    available = {f.name for f in font_manager.fontManager.ttflist}
    plt.rcParams["font.family"] = next((f for f in
        ("Hiragino Sans", "Noto Sans CJK JP", "IPAexGothic") if f in available), "DejaVu Sans")
    plt.rcParams["svg.fonttype"] = "path"
    panels = math.ceil(len(rows) / 32)
    fig, axes = plt.subplots(panels, 1, figsize=(16, 4.2 * panels + 1), squeeze=False)
    for panel, ax in enumerate(axes[:, 0]):
        subset = rows[panel * 32:(panel + 1) * 32]
        x = [r["index"] for r in subset]
        y = [r["score"] for r in subset]
        ax.plot(x, y, "o-", color="#2878b5", linewidth=2,
                label="ローカル除外（API未送信）" if excluded else "各打鍵の実測評価値")
        for threshold, color in ((0.8, "#c86658"), (0.9, "#777777")):
            ax.axhline(threshold, linestyle="--", color=color, label=f"閾値 {threshold}")
        for row in subset:
            index, score = row["index"], row["score"]
            if row["gold"] or row["acceptable"]:
                color = "#2878b5" if row["gold"] else "#e9b44c"
                label = "好ましい" if row["gold"] else "許容"
                ax.scatter([index], [score], s=100, color=color, zorder=4)
                ax.annotate(label, (index, score), xytext=(0, -30),
                            textcoords="offset points", ha="center", fontsize=9)
            ax.text(index, score + 0.025, f"{score:.2f}", ha="center", fontsize=8)
            if row["pause_ms"]:
                ax.axvline(index, alpha=0.2, color="#777777")
        ax.set_xticks(x, [f"{r['index']}\n{r['key'] if r['key'] != ' ' else '␣'}" for r in subset])
        ax.set(xlim=(x[0] - 0.7, x[-1] + 0.7), ylim=(0, 1.15),
               ylabel="変換評価値（0〜1）", xlabel="打鍵番号と入力文字（␣は空白）")
        ax.grid(alpha=0.2)
        if panel == 0:
            ax.legend(loc="upper left", fontsize=9, ncol=3)
    title = "API未測定・ローカル除外" if excluded else "1文字ごとの Jev 評価値"
    fig.suptitle(f"{args.case_id}：{title}\n{text.replace(' ', '␣')}", fontsize=13)
    fig.text(0.08, 0.025,
             f"質問文 {data['prompt_version']} ｜各時点を独立評価（途中で変換せず入力継続）。縦線は待機後の判定。英文の期待結果は全打鍵で変換なし。",
             fontsize=10)
    fig.tight_layout(rect=(0, 0.065, 1, 0.94))
    output_dir = args.output_dir or args.result.parent
    output_dir.mkdir(parents=True, exist_ok=True)
    for suffix in ("png", "svg"):
        output = output_dir / f"{args.case_id}_keystrokes.{suffix}"
        fig.savefig(output, dpi=180)
        print(output)
    plt.close(fig)


if __name__ == "__main__":
    main()
