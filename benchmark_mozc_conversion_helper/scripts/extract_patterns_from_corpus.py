#!/usr/bin/env python3
"""
コーパス（Livedoorニュース等のプレーンテキスト、またはWikipediaダンプ）から
mozc_simulator のパターンにマッチする文を抽出し、
extracted_pattern_code.txt（TestCase の埋め込みコード片）を生成する。

使い方:
  1) data/corpus/*.txt にプレーンテキストを配置（推奨: Livedoorニュースの整形結果）
     もしくは data/ に jawiki ダンプ（.xml.bz2）を配置
  2) python3 scripts/extract_patterns_from_corpus.py
  3) extracted_pattern_code.txt が生成される
"""

import bz2
import re
import sys
from pathlib import Path
from typing import Dict, List, Tuple

try:
    import mwparserfromhell  # type: ignore
except Exception:
    mwparserfromhell = None

# 親ディレクトリの mozc_helper をインポート
sys.path.append(str(Path(__file__).parent.parent))
from mozc_helper import MozcClient


def get_conversion_patterns() -> Dict[str, List[str]]:
    """mozc_simulator.py の変換パターンを取得（extract_patterns_from_aozora.py と同等）。"""
    mozc_client = MozcClient()
    # 直接参照が難しいため、mozc_simulator と同等の候補をここでも構築する
    #（extract_patterns_from_aozora.py の conversion_map と同一の集合を使う）
    from scripts.extract_patterns_from_aozora import get_conversion_patterns as _orig
    return _orig()


def _strip_wikitext(text: str) -> str:
    """wikitext からプレーンテキストへ。mwparserfromhell が無ければ粗い正規化。"""
    if mwparserfromhell is not None:
        try:
            return mwparserfromhell.parse(text).strip_code()
        except Exception:
            pass
    # フォールバック: 粗いタグ除去（品質は下がるが実行可能）
    text = re.sub(r"\{\{[^}]*\}\}", " ", text)   # {{...}}
    text = re.sub(r"\[\[[^\]]*\|([^\]]+)\]\]", r"\1", text)  # [[A|B]] -> B
    text = re.sub(r"\[\[([^\]]+)\]\]", r"\1", text)          # [[A]] -> A
    text = re.sub(r"<[^>]+>", " ", text)            # HTMLタグ
    text = re.sub(r"'{2,}", "", text)               # 強調記号 '' '''
    return text


def _iter_wikipedia_texts(dump_path: Path):
    """jawiki ダンプから本文を順に取り出すジェネレータ。"""
    # <text ...> ... </text> の間の本文を抽出
    open_fn = bz2.open if dump_path.suffix == ".bz2" else open
    with open_fn(dump_path, "rt", encoding="utf-8", errors="ignore") as f:
        in_text = False
        buf = []
        for line in f:
            if "<text" in line:
                in_text = True
                # 開始タグ内の末尾以降を取り込む
                start = line.find(">")
                if start >= 0:
                    buf.append(line[start+1:])
                continue
            if in_text and "</text>" in line:
                # 終了タグ手前まで
                end = line.find("</text>")
                if end >= 0:
                    buf.append(line[:end])
                raw = "".join(buf)
                buf = []
                in_text = False
                plain = _strip_wikitext(raw)
                yield plain
                continue
            if in_text:
                buf.append(line)


def _split_sentences(text: str) -> List[str]:
    # 句点・感嘆符・改行で分割して、長さフィルタを適用
    sentences = re.split(r"[。！？\n]", text)
    return [s.strip() for s in sentences if 10 <= len(s.strip()) <= 150]


def load_wikipedia_sentences() -> List[str]:
    """ダンプ（Wikipedia）またはプレーンテキスト（Livedoor 等）から文を読み込む。"""
    data_dir = Path("data")
    candidates = list(data_dir.glob("*.xml.bz2")) or list(data_dir.glob("*.bz2"))

    texts: List[str] = []

    # ダンプ優先
    if candidates:
        dump_path = candidates[0]
        print(f"Loading Wikipedia dump: {dump_path}")
        count_pages = 0
        for page_text in _iter_wikipedia_texts(dump_path):
            count_pages += 1
            if count_pages % 1000 == 0:
                print(f"  parsed pages: {count_pages}")
            texts.extend(_split_sentences(page_text))
        print(f"Loaded {len(texts)} sentences from dump (pages: {count_pages})")
        return texts

    # プレーンテキストがあればそれを使う（corpus/ のみをサポート）
    txt_files = list(data_dir.glob("corpus/*.txt"))
    if txt_files:
        print(f"Loading plain texts from {data_dir}")
        for fp in txt_files:
            try:
                content = fp.read_text(encoding="utf-8")
                texts.extend(_split_sentences(content))
            except Exception as e:
                print(f"Error loading {fp}: {e}")
        print(f"Loaded {len(texts)} sentences from {len(txt_files)} files")
        return texts

    print("No corpus found in ./data. Place text files under ./data/corpus/ or a jawiki dump under ./data/")
    return []


def find_pattern_matches(texts: List[str], conversion_map: Dict[str, List[str]]) -> Dict[str, Tuple[str, str, str]]:
    """各パターンにマッチする文章を検索（Aozora 版と同等の戦略）。"""
    pattern_matches = {}

    for reading, candidates in conversion_map.items():
        print(f"Searching for patterns: {reading}")

        # 英数字・カタカナ・ひらがな等を除外して漢字候補のみに絞る
        kanji_candidates = [c for c in candidates if c not in [reading, reading.upper(), reading.lower()]
                            and not re.match(r'^[a-zA-Z\[\]]+$', c)
                            and not re.match(r'^[ァ-ヶー]+$', c)
                            and not re.match(r'^[ぁ-ん]+$', c)]
        if not kanji_candidates:
            continue

        primary_candidates = kanji_candidates[:3]
        pattern = '(' + '|'.join(re.escape(c) for c in primary_candidates) + ')'

        for text in texts:
            matches = list(re.finditer(pattern, text))
            if not matches:
                continue
            match = matches[0]

            start = max(0, match.start() - 20)
            end = min(len(text), match.end() + 20)
            context = text[start:end]

            context_with_reading = text[:match.start()] + f"[{reading}]" + text[match.end():]

            pattern_matches[reading] = (
                context_with_reading,
                match.group(),
                text,
            )
            print(f"  Found: {match.group()} in context")
            break

        if reading not in pattern_matches:
            print(f"  No match found for {reading}")

    return pattern_matches


def clean_ruby_tags(text: str) -> str:
    # 一般に不要だが、保険として HTML 断片を除去
    text = re.sub(r'<[^>]+>', '', text)
    return text


def generate_embedded_code(pattern_matches: Dict[str, Tuple[str, str, str]]) -> str:
    cases_code = ""
    for reading, (context, correct_answer, source_text) in pattern_matches.items():
        # 安全のため軽いクリーニング
        context_clean = clean_ruby_tags(context)
        source_clean = clean_ruby_tags(source_text)

        context_escaped = context_clean.replace('"', '\\"').replace('\n', '\\n')
        source_escaped = source_clean.replace('"', '\\"').replace('\n', '\\n')

        cases_code += f'''
                test_case = TestCase(
                    context="{context_escaped}",
                    reading="{reading}",
                    correct_answer="{correct_answer}",
                    source_text="{source_escaped}"
                )
                cases.append(test_case)
'''

    return f"""
        # コーパスから抽出したテストケース
        cases = []
        {cases_code.strip()}

        return cases"""


def main():
    print("=== コーパスから変換候補マッチング抽出 ===")
    conversion_map = get_conversion_patterns()
    print(f"Total patterns: {len(conversion_map)}")

    texts = load_wikipedia_sentences()
    if not texts:
        print("No texts to process. Please check data/ directory.")
        return

    pattern_matches = find_pattern_matches(texts, conversion_map)
    print(f"\nMatched patterns: {len(pattern_matches)}")

    embedded_code = generate_embedded_code(pattern_matches)
    out_path = Path("extracted_pattern_code.txt")
    out_path.write_text(embedded_code, encoding="utf-8")
    print(f"Generated embedded code for {len(pattern_matches)} patterns -> {out_path}")


if __name__ == "__main__":
    main()
