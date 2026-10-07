#!/usr/bin/env python3
"""Generate matched Decisions/Jev reports and one chart per case; no API calls."""
import argparse
import json
import math
from pathlib import Path

from run import DATASETS, THRESHOLDS, bench, load_rows, summarize

GRAPH_GUIDE = [
    "### 折れ線グラフの読み方", "",
    "折れ線は各APIの変換評価値です。黄色・緑の帯は、テストデータにあらかじめ付けた期待する変換タイミングを示し、APIの評価値や実際に変換が起きた位置ではありません。", "",
    "- 緑の帯：変換するのが好ましい打鍵位置（`trigger_after`）。",
    "- 黄色の帯：変換してもよいが、もう少し待つ方が好ましい打鍵位置（`acceptable_after`）。",
    "- 帯なし：今回のラベルでは、まだ変換せず待ってほしい打鍵位置。", "",
    "例えば `arigatou␣gozaimasu␣` では、最初の空白が黄色、最後の空白が緑です。`␣` は空白を表します。帯は評価の基準であり、普遍的な唯一の正解を意味しません。", "",
]


def validated_rows(data, expected, backend):
    if data.get("backend") != backend or not data.get("complete_dataset") or data.get("prompt_version") != "v2":
        raise ValueError("complete v2 measurements of the requested backend are required")
    rows = data.get("rows", [])
    if len(rows) != len(expected):
        raise ValueError("missing keystroke rows")
    normalized = []
    for actual, reference in zip(rows, expected):
        if any(actual.get(k) != value for k, value in reference.items()):
            raise ValueError("dataset mismatch")
        excluded = actual["context"] in bench.EXCLUDED_CONTEXTS
        status = actual.get("status", "excluded" if excluded else "ok")
        if status != ("excluded" if excluded else "ok"):
            raise ValueError("failed/incomplete rows cannot be graphed as scores")
        score = actual.get("score")
        if not excluded and (type(score) not in (int, float) or not math.isfinite(score) or not 0 <= score <= 1):
            raise ValueError("invalid measured score")
        normalized.append({**actual, "status": status, "score": None if excluded else score})
    return normalized


def select_threshold(rows):
    """Conservative tuning on dev only: fewer unwanted, then more preferred hits."""
    choices = [(t, bench.preference_summary(rows, t)) for t in THRESHOLDS]
    return min(choices, key=lambda x: (x[1]["unwanted_triggers"],
                                      -x[1]["preferred_detected"],
                                      -x[1]["acceptable_precision"], x[0]))[0]


def build_comparison(decisions, jev):
    expected = load_rows("all")
    rows = {"decisions": validated_rows(decisions, expected, "decisions"),
            "jev": validated_rows(jev, expected, "jev")}
    dev_ids = {r["id"] for r in load_rows("dev")}
    result = {"total_keystrokes": len(expected), "tuning_case_ids": sorted(dev_ids), "backends": {}}
    for name, data in (("decisions", decisions), ("jev", jev)):
        measured = [r for r in rows[name] if r["status"] == "ok"]
        tuning = [r for r in measured if r["id"] in dev_ids]
        holdout = [r for r in measured if r["id"] not in dev_ids]
        threshold = select_threshold(tuning)
        result["backends"][name] = {
            "at_common_threshold_0.8": summarize(rows[name], .8),
            "dev_selected_threshold": threshold,
            "heldout_at_selected_threshold": summarize(holdout, threshold),
            "model_versions": data.get("model_versions"),
            "started_at": data.get("started_at", "historical: see Jev VISUAL_REPORT"),
            "latency_p50_ms": data.get("summary", {}).get("latency_ms", {}).get("p50", data.get("latency_p50_ms")),
            "latency_p95_ms": data.get("summary", {}).get("latency_ms", {}).get("p95", data.get("latency_p95_ms")),
            "latency_p99_ms": data.get("summary", {}).get("latency_ms", {}).get("p99"),
            "api_calls": data["api_calls"], "input_tokens": data["input_tokens"],
            "estimated_usd": data["estimated_usd"],
            "estimated_usd_per_1000_keystrokes": data["estimated_usd_per_1000_keystrokes"],
            "unknown_usage_attempts": data.get("unknown_usage_attempts", 0),
            "attempt_errors": data.get("summary", {}).get("attempt_errors", {}),
        }
    return result, rows


def generate(args):
    decisions = json.loads(args.decisions.read_text(encoding="utf-8"))
    jev = json.loads(args.jev.read_text(encoding="utf-8"))
    comparison, rows = build_comparison(decisions, jev)
    # Validation precedes imports and file generation.
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib import font_manager
    fonts = {f.name for f in font_manager.fontManager.ttflist}
    plt.rcParams["font.family"] = next((f for f in ("Hiragino Sans", "Noto Sans CJK JP", "IPAexGothic")
                                      if f in fonts), "DejaVu Sans")
    plt.rcParams["svg.fonttype"] = "path"
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "comparison.json").write_text(json.dumps(comparison, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    names = ("decisions", "jev")
    colors = {"decisions": "#c35d36", "jev": "#267bb5"}
    fig, axes = plt.subplots(1, 3, figsize=(13, 4))
    for ax, field, title in zip(axes, ("preferred_detected", "acceptable_but_nonpreferred", "unwanted_triggers"),
                                ("好ましい位置の検出", "許容できる早期発動", "待つべき位置での発動")):
        ax.bar(names, [comparison["backends"][n]["at_common_threshold_0.8"]["preference_summary"][field] for n in names],
               color=[colors[n] for n in names])
        ax.set_title(title)
        ax.set_ylabel("件数（共通閾値0.8）")
        for bar in ax.patches:
            ax.annotate(f"{bar.get_height():.0f}", (bar.get_x() + bar.get_width() / 2, bar.get_height()),
                        xytext=(0, 3), textcoords="offset points", ha="center")
        ax.set_ylim(0, max(1, max(bar.get_height() for bar in ax.patches) * 1.15))
    fig.tight_layout()
    fig.savefig(args.output_dir / "quality.png", dpi=150)
    plt.close(fig)
    fig, axes = plt.subplots(1, 2, figsize=(10, 4))
    for i, name in enumerate(names):
        record = comparison["backends"][name]
        axes[0].bar(i - .18, record["latency_p50_ms"], width=.35, color=colors[name])
        axes[0].bar(i + .18, record["latency_p95_ms"], width=.35, color=colors[name], alpha=.45)
    axes[0].set_xticks(range(2), names)
    axes[0].set_title("応答時間：左p50 / 右p95（ms）")
    axes[1].bar(names, [comparison["backends"][n]["estimated_usd_per_1000_keystrokes"] for n in names],
                color=[colors[n] for n in names])
    axes[1].set_title("1000打鍵当たり概算（USD）")
    fig.tight_layout()
    fig.savefig(args.output_dir / "latency_cost.png", dpi=150)
    plt.close(fig)
    fig, axes = plt.subplots(1, 2, figsize=(11, 4))
    for name in names:
        sweep = comparison["backends"][name]["at_common_threshold_0.8"]["threshold_sweep"]
        axes[0].plot(THRESHOLDS, [sweep[str(t)]["preferred_recall"] for t in THRESHOLDS], "o-", color=colors[name], label=name)
        axes[1].plot(THRESHOLDS, [sweep[str(t)]["unwanted_triggers"] for t in THRESHOLDS], "o-", color=colors[name], label=name)
    for ax, title in zip(axes, ("好ましい位置の検出率", "待つべき位置での発動件数")):
        ax.set(xlabel="閾値", title=title)
        ax.grid(alpha=.2)
        ax.legend()
    axes[0].set_ylim(0, 1.05)
    fig.tight_layout()
    fig.savefig(args.output_dir / "thresholds.png", dpi=150)
    plt.close(fig)
    report = ["# Decisions API / Jev 比較結果", "",
              "各打鍵は独立に評価。途中で実際の変換は行いません。評価値の校正はAPI間で同一とは仮定しません。", "",
              "過去Jevとの比較は計測日時・環境差を含みます。遅延のp99は古い結果には保存されていないため補間しません。", "",
              "![品質](quality.png)", "", "![速度と費用](latency_cost.png)", "", "![閾値比較](thresholds.png)", "",
              "| API | 好ましい検出/総数 | 許容早期 | 待つべき発動 | 英語発動 | dev選択閾値 |", "|---|---:|---:|---:|---:|---:|"]
    for name in names:
        item = comparison["backends"][name]
        summary = item["at_common_threshold_0.8"]
        pref = summary["preference_summary"]
        report.append(f"| {name} | {pref['preferred_detected']}/{pref['preferred_total']} | {pref['acceptable_but_nonpreferred']} | {pref['unwanted_triggers']} | {summary['english_safety']['false_triggers']} | {item['dev_selected_threshold']} |")
    report.extend(["", "閾値選択はdevのみで、待つべき位置の発動を最小化し、同数なら好ましい位置の検出数を最大化します。追加・英語・句読点セットを評価側に使用します。既存データはJevプロンプト調整にも使われたため、完全な未見評価ではありません。", "",
                   "| API | 評価側の好ましい検出/総数 | 評価側の待つべき発動 |", "|---|---:|---:|"])
    for name in names:
        pref = comparison["backends"][name]["heldout_at_selected_threshold"]["preference_summary"]
        report.append(f"| {name} | {pref['preferred_detected']}/{pref['preferred_total']} | {pref['unwanted_triggers']} |")
    report.extend(["", "| API | p50 (ms) | p95 (ms) | p99 (ms) | 1000打鍵概算 (USD) |",
                   "|---|---:|---:|---:|---:|"])
    for name in names:
        item = comparison["backends"][name]
        latency = ["未保存" if item[field] is None else f"{item[field]:.1f}"
                   for field in ("latency_p50_ms", "latency_p95_ms", "latency_p99_ms")]
        report.append(f"| {name} | {' | '.join(latency)} | {item['estimated_usd_per_1000_keystrokes']:.6f} |")
    report.extend(["", "## 費用と計測情報", ""])
    for name in names:
        item = comparison["backends"][name]
        cost = item["estimated_usd_per_1000_keystrokes"]
        report.extend([f"- {name}: モデル {item['model_versions']}、開始 {item['started_at']}、{item['api_calls']}回、{item['input_tokens']}入力トークン、概算${item['estimated_usd']:.6f}。",
                       f"  1日10000打鍵・月30日という仮定：日${cost * 10:.4f} / 月${cost * 300:.4f}（漢字変換API料金は別）。", ""])
        if item["attempt_errors"]:
            report.extend([f"  途中の失敗試行：{item['attempt_errors']}。再試行後の評価値で品質を集計し、遅延は失敗試行も含めます。", ""])
        if item["unknown_usage_attempts"]:
            report.extend([f"  使用量不明の試行が{item['unknown_usage_attempts']}件あります。料金は判明分だけで、請求総額ではありません。", ""])
    report.extend(["## 判断", "",
                   "数値でいうと、今回の共通閾値0.8ではJevが優位です。一方、SumibiですでにOpenAIのアクセスキーを利用する運用なら、別サービスの契約・キー管理を増やさずに使えるDecisions APIも総合評価では有力な選択肢です。グラフの共通した挙動を踏まえ、閾値・発動条件の調整と実操作での検証によって代替可能性を評価します。", "",
                   "この集計だけでは採用を確定しません。新しい未見例文、繰り返し測定、コメント・文字列の追加データ、連続入力での間引き・古い応答破棄を含む実Emacs相当の評価が必要です。", "", "## 全例文：1例文1グラフ", ""])
    report.extend(GRAPH_GUIDE)
    for case_id in dict.fromkeys(r["id"] for r in rows["decisions"]):
        series = {n: [r for r in rows[n] if r["id"] == case_id] for n in names}
        reference = series["decisions"]
        report.extend([f"### {case_id}", ""])
        if reference[0]["status"] == "excluded":
            report.extend(["ローカル除外・API未送信。モデル評価値のグラフは作りません。", ""])
            continue
        panels = math.ceil(len(reference) / 32)
        fig, axes = plt.subplots(panels, 1, figsize=(15, panels * 3.4 + 1), squeeze=False)
        for panel, ax in enumerate(axes[:, 0]):
            subset = reference[panel * 32:(panel + 1) * 32]
            for n in names:
                other = series[n][panel * 32:(panel + 1) * 32]
                ax.plot([r["index"] for r in other], [r["score"] for r in other], ".-", color=colors[n], label=n)
            for r in subset:
                if r["acceptable"]:
                    ax.axvspan(r["index"] - .3, r["index"] + .3,
                               color="#67a77d" if r["gold"] else "#e6b94d", alpha=.25)
            ax.axhline(.8, color="gray", linestyle="--", label="共通閾値0.8")
            ax.set_xticks([r["index"] for r in subset], [f"{r['index']}\n{r['key'].replace(' ', '␣')}" for r in subset])
            ax.set(ylim=(0, 1.05), ylabel="変換評価値")
            ax.grid(alpha=.15)
            if panel == 0:
                ax.legend(ncol=3)
        fig.suptitle(f"{case_id}：緑=好ましい / 黄=許容（各時点を独立評価）")
        fig.tight_layout(rect=(0, 0, 1, .96))
        for suffix in ("png", "svg"):
            fig.savefig(args.output_dir / f"{case_id}.{suffix}", dpi=150)
        plt.close(fig)
        report.extend([f"![{case_id}]({case_id}.png)", "", f"[SVG]({case_id}.svg)", ""])
    (args.output_dir / "REPORT.md").write_text("\n".join(report), encoding="utf-8")
    return args.output_dir / "REPORT.md"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("decisions", type=Path)
    parser.add_argument("jev", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    try:
        print(generate(args))
    except (ValueError, KeyError, OSError, TypeError) as exc:
        parser.exit(1, f"comparison error: {exc}\n")


if __name__ == "__main__":
    main()
