"""Regression tests for the iOS PCC AJIMEE v1 import."""

import json
import unittest
from pathlib import Path

from aggregate_results import summarize
from import_pcc_ios import INPUT_TYPES, prepare_cases
from generate_pcc_comparison import load_comparison


HERE = Path(__file__).parent


class PCCImportTests(unittest.TestCase):
    def test_all_ids_and_scores(self):
        cases = prepare_cases()
        expected = {
            "romaji": (0.500666, 11, 26),
            "hiragana": (0.289493, 46, 29),
            "katakana": (0.342573, 28, 28),
        }
        for input_type in INPUT_TYPES:
            suffix = "" if input_type == "romaji" else f"_{input_type}"
            path = HERE / f"result_pcc_ios/apple-pcc-ios17e{suffix}.json"
            rows = json.loads(path.read_text(encoding="utf-8"))
            with self.subTest(input_type=input_type):
                self.assertEqual(len(rows), 200)
                self.assertEqual({row["id"] for row in rows}, set(cases))
                self.assertEqual(sum(row["at1"] for row in rows), expected[input_type][1])
                self.assertEqual(sum(row["error"] is not None for row in rows), expected[input_type][2])
                self.assertAlmostEqual(summarize(rows)[0], expected[input_type][0], places=6)
                for row in rows:
                    self.assertEqual(row["expect"], cases[row["id"]]["expected"])
                    self.assertEqual(row["henkan_text"], cases[row["id"]][input_type])

    def test_full_comparison_covers_all_saved_v1_results(self):
        tables = load_comparison()
        pcc = {"romaji": 1, "hiragana": 1, "katakana": 1}
        for input_type, entries in tables.items():
            saved = sum(
                1 for directory in ("result_ver2.3.0", "result_ver2.4.0")
                for path in (HERE / directory).glob("*.json")
                if (path.stem.endswith("_hiragana") if input_type == "hiragana"
                    else path.stem.endswith("_katakana") if input_type == "katakana"
                    else not path.stem.endswith(("_hiragana", "_katakana")))
            )
            # v2.3.0 duplicate configurations are represented by their v2.4.0 run.
            duplicates = sum(1 for path in (HERE / "result_ver2.3.0").glob("*.json")
                             if (HERE / "result_ver2.4.0" / path.name).exists()
                             and (path.stem.endswith("_hiragana") if input_type == "hiragana"
                                  else path.stem.endswith("_katakana") if input_type == "katakana"
                                  else not path.stem.endswith(("_hiragana", "_katakana"))))
            self.assertEqual(len(entries), saved - duplicates + pcc[input_type])


if __name__ == "__main__":
    unittest.main()
