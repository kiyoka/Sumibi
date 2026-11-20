"""Regression tests for the iOS PCC AJIMEE v1 import."""

import json
import unittest
import tempfile
from pathlib import Path
from unittest.mock import patch
import import_pcc_ios

from aggregate_results import summarize
from import_pcc_ios import INPUT_TYPES, prepare_cases, normalize, measurement_metadata, result_name, pcc_runs
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
            path = HERE / f"result_pcc_ios/apple-pcc-ios17e-20261116{suffix}.json"
            rows = json.loads(path.read_text(encoding="utf-8"))
            with self.subTest(input_type=input_type):
                self.assertEqual(len(rows), 200)
                self.assertEqual({row["id"] for row in rows}, set(cases))
                self.assertEqual(sum(row["at1"] for row in rows), expected[input_type][1])
                self.assertEqual(sum(row["error"] is not None for row in rows), expected[input_type][2])
                self.assertAlmostEqual(summarize(rows)[0], expected[input_type][0], places=6)
                for row in rows:
                    self.assertEqual(row["measurement_date"], "2026-11-16")
                    self.assertIn("PR #216 creation date (Asia/Tokyo)", row["measurement_date_source"])
                    self.assertEqual(row["expect"], cases[row["id"]]["expected"])
                    self.assertEqual(row["henkan_text"], cases[row["id"]][input_type])

    def test_import_retains_explicit_date_without_changing_scores(self):
        cases = prepare_cases()
        for kind in INPUT_TYPES:
            suffix = "" if kind == "romaji" else f"_{kind}"
            saved = json.loads((HERE / f"result_pcc_ios/apple-pcc-ios17e-20261116{suffix}.json").read_text())
            exports = [{"id": r["id"], "context": cases[r["id"]]["context"], "expected": r["expect"],
                        "romaji": cases[r["id"]]["romaji"], "inputText": r["henkan_text"],
                        "result": r["result"], "elapsedSec": r["elapsed_sec"], "error": r["error"]}
                       for r in saved]
            source = saved[0]["measurement_date_source"]
            self.assertEqual(normalize(exports, kind, cases, measured_on="2026-11-16",
                                       date_source=source), saved)
            updated = normalize(exports, kind, cases, measured_on="2026-11-20", date_source="Actual measurement date (Asia/Tokyo)")
            self.assertTrue(all(r["measurement_date"] == "2026-11-20" for r in updated))
        for value in ("2026-02-30", "2026-1-2", "not-a-date"):
            with self.assertRaises(ValueError):
                measurement_metadata(value, "test")
        with self.assertRaises(ValueError):
            measurement_metadata("2026-11-16", " ")

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

    def test_two_dated_runs_remain_distinct_in_all_graph_loaders(self):
        import plot_errorrate_vs_inputtype as all_inputs
        import plot_errorrate_vs_inputtype_hiragana as hiragana
        import plot_mean_response_time as latency
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "result_ver2.4.0").mkdir()
            target = root / "result_pcc_ios"
            target.mkdir()
            for day in ("2026-11-16", "2027-01-16"):
                for kind in INPUT_TYPES:
                    original = HERE / "result_pcc_ios" / result_name("2026-11-16", kind)
                    (target / result_name(day, kind)).write_bytes(original.read_bytes())
            names = {"apple-pcc-ios17e-20261116", "apple-pcc-ios17e-20270116"}
            self.assertEqual(set(pcc_runs(target)), names)
            # Synthetic copies only exercise loading; they are not new measurements.
            results = str(root / "result_ver2.4.0")
            self.assertEqual({r["model"] for r in all_inputs.load_model_data(results)},
                             {n + " (reference)" for n in names})
            self.assertEqual({r["model"] for r in hiragana.load_model_data(results)},
                             {n + " (reference)" for n in names})
            self.assertEqual(set(latency.load_all_results(results)),
                             {n + " (reference)" for n in names})
        from generate_pcc_comparison import render
        tables = load_comparison()
        for entries in tables.values():
            earlier = next(e for e in entries if e["version"] == "iOS PCC")
            entries.append({**earlier, "model": "apple-pcc-ios17e-20270116"})
        report = render(tables)
        self.assertIn("apple-pcc-ios17e-20261116", report)
        self.assertIn("apple-pcc-ios17e-20270116", report)

    def test_import_refuses_to_overwrite_dated_results(self):
        with tempfile.TemporaryDirectory() as directory:
            existing = Path(directory) / result_name("2026-11-16", "romaji")
            existing.write_text("preserve original")
            with patch("sys.argv", ["import_pcc_ios.py", "unused1", "unused2", "unused3",
                                   "--measurement-date", "2026-11-16",
                                   "--measurement-date-source", "test",
                                   "--output-dir", directory]):
                with self.assertRaises(SystemExit):
                    import_pcc_ios.main()
            self.assertEqual(existing.read_text(), "preserve original")
            self.assertEqual(list(Path(directory).iterdir()), [existing])


if __name__ == "__main__":
    unittest.main()
