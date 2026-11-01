import Levenshtein


# 全角英数字・記号を半角へ写像するテーブル（幅正規化用）
_FULLWIDTH = "".join(chr(c) for c in range(0xFF01, 0xFF5F))
_HALFWIDTH = "".join(chr(c - 0xFEE0) for c in range(0xFF01, 0xFF5F))
_WIDTH_TABLE = str.maketrans(_FULLWIDTH, _HALFWIDTH)


def normalize_width(s):
    """全角英数字・記号を半角に統一する。

    半角/全角の表記ゆれ（例: ３００ と 300、ＮＨＫ と NHK）はかな漢字変換の
    本質的な難所ではなく IME の表記正規化・後処理の問題である。この関数は
    その幅ゆれを吸収するためのもので、漢数字（三百）と算用数字（300）のような
    「変換の選択」の違いは正規化しない（別文字列のまま保持される）。
    """
    return s.translate(_WIDTH_TABLE)


def calculate_CER(reference, hypothesis):
    """
    Calculate Character Error Rate (CER) between two strings.
    CER = (substitutions + deletions + insertions) / length of reference
    """
    if len(reference) == 0:
        return float('inf')  # avoid division by zero
    distance = Levenshtein.distance(reference, hypothesis)
    return distance / len(reference)


def calculate_MinCER(references, hypothesis):
    """
    Calculate Character Error Rate (CER) between two strings.
    CER = (substitutions + deletions + insertions) / length of reference
    """
    cers = [calculate_CER(reference, hypothesis) for reference in references]
    return min(cers)


def calculate_accuracy_at1(references, attempt, normalize=False):
    """
    Calculate accuracy of attempts against references.
    Accuracy = correct / total

    references: list[str], reference string (multiple reference strings can be provided)
    attempt: str, best attempt string
    normalize: bool, if True, normalize fullwidth/halfwidth before matching.
        Default is False, which preserves the exact-match behavior of the
        original benchmark. Turn it on to ignore mere width variation
        (e.g. treat ３００ and 300 as equal) without listing every variant.
    """
    if normalize:
        return 1 if normalize_width(attempt) in {normalize_width(r) for r in references} else 0
    return 1 if attempt in references else 0


def bootstrap_ci(values, n_resamples=10000, confidence=0.95, seed=42):
    """
    Calculate a bootstrap confidence interval for the mean of values.

    values: list[float], per-item scores (e.g. accuracy 0/1 or CER)
    Returns (mean, lower, upper).
    """
    import random
    if len(values) == 0:
        raise ValueError("values must not be empty")
    rng = random.Random(seed)
    n = len(values)
    means = sorted(sum(rng.choices(values, k=n)) / n for _ in range(n_resamples))
    alpha = (1 - confidence) / 2
    lower = means[int(alpha * n_resamples)]
    upper = means[min(int((1 - alpha) * n_resamples), n_resamples - 1)]
    return sum(values) / n, lower, upper


def evaluate_items(items, attempts, n_resamples=10000, normalize=False):
    """
    Evaluate attempts against evaluation items, with per-tag breakdown.

    items: list[dict], evaluation items (each may have an optional "tags" field)
    attempts: list[str], model outputs aligned with items
    normalize: bool, forwarded to calculate_accuracy_at1 to optionally ignore
        fullwidth/halfwidth variation (default False keeps exact match).
    Returns a dict:
        {
          "overall": {"n", "accuracy@1", "accuracy@1_ci", "MinCER"},
          "by_tag": {tag: {"n", "accuracy@1", "MinCER"}, ...}
        }
    """
    if len(items) != len(attempts):
        raise ValueError("items and attempts must have the same length")
    accs = []
    cers = []
    for item, attempt in zip(items, attempts):
        accs.append(calculate_accuracy_at1(item["expected_output"], attempt, normalize=normalize))
        cers.append(calculate_MinCER(item["expected_output"], attempt))
    mean_acc, acc_lo, acc_hi = bootstrap_ci(accs, n_resamples=n_resamples)
    mean_cer, cer_lo, cer_hi = bootstrap_ci(cers, n_resamples=n_resamples)
    result = {
        # CER は連続値で部分的な改善も検出できるため、僅差のモデル比較では
        # accuracy@1（二値）よりこちらを優先して見ることを推奨する。
        "primary_metric": "MinCER",
        "overall": {
            "n": len(items),
            "accuracy@1": mean_acc,
            "accuracy@1_ci": (acc_lo, acc_hi),
            "MinCER": mean_cer,
            "MinCER_ci": (cer_lo, cer_hi),
        },
        "by_tag": {},
    }
    tag_scores = {}
    for item, acc, cer in zip(items, accs, cers):
        for tag in item.get("tags", []):
            tag_scores.setdefault(tag, []).append((acc, cer))
    for tag, scores in sorted(tag_scores.items()):
        result["by_tag"][tag] = {
            "n": len(scores),
            "accuracy@1": sum(a for a, _ in scores) / len(scores),
            "MinCER": sum(c for _, c in scores) / len(scores),
        }
    return result
