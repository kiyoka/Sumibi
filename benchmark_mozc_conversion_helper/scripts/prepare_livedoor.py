#!/usr/bin/env python3
"""
Livedoor ニュースコーパス（LDCC）から記事本文を抽出して、
benchmark_mozc_conversion_helper/data/corpus/livedoor.txt を生成します。

使い方:
  1) Livedoor ニュースコーパスを展開（例: ./external/LDCC/text/...）
  2) python3 scripts/prepare_livedoor.py ./external/LDCC/text
     ※ 引数未指定時は ./data/livedoor を探索します
  3) data/corpus/livedoor.txt が生成されます
"""

from pathlib import Path
import sys
import re


def iter_ldcc_files(root: Path):
    for p in root.rglob("*.txt"):
        # READMEやLICENSEなどは除外
        name = p.name.lower()
        if any(k in name for k in ["readme", "license", "changelog"]):
            continue
        yield p


def extract_article_text(path: Path) -> str:
    """LDCCの1ファイルからタイトル+本文を推定抽出して1行テキストを返す。"""
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except UnicodeDecodeError:
        lines = path.read_text(encoding="cp932", errors="ignore").splitlines()

    if len(lines) < 3:
        return ""

    # 典型構造: 0:url, 1:date, 2:title, 3-:body
    # バリアント: 空行が入る場合も考慮
    title_idx = 2
    if len(lines) > 3 and lines[2].strip() == "":
        title_idx = 3

    title = lines[title_idx].strip()
    body_lines = [ln.strip() for ln in lines[title_idx + 1:] if ln.strip()]
    body = " ".join(body_lines)

    # 余計な空白の正規化
    text = (title + "。 " + body).strip()
    text = re.sub(r"\s+", " ", text)
    return text


def main():
    # 入力ルート
    src = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("data/livedoor")
    if not src.exists():
        print(f"Input directory not found: {src}")
        print("Usage: python3 scripts/prepare_livedoor.py /path/to/LDCC/text")
        sys.exit(1)

    # 出力先
    out_dir = Path("data/corpus")
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "livedoor.txt"

    n_files = 0
    n_written = 0

    with out_path.open("w", encoding="utf-8") as out:
        for fp in iter_ldcc_files(src):
            n_files += 1
            text = extract_article_text(fp)
            if text:
                out.write(text + "\n")
                n_written += 1

    print(f"Processed files: {n_files}")
    print(f"Written articles: {n_written}")
    print(f"Output: {out_path}")


if __name__ == "__main__":
    main()
