#!/usr/bin/env python3

import argparse
import csv
import hashlib
import json
import sys
from pathlib import Path

csv.field_size_limit(sys.maxsize)

ROOT = Path(__file__).resolve().parents[1]

EXPECTED_ROWS = {
    "data/derived/book_summaries.csv": 250,
    "data/derived/story_morals.csv": 8685,
    "data/derived/section_4_1_cross_input_pairs_all.csv": 37665,
    "data/derived/section_4_1_cross_model_pairs_all.csv": 25110,
    "data/derived/section_4_1_within_full_text_pairs.csv": 2250,
    "data/derived/value_labels_full_text.csv": 34915,
    "data/derived/value_labels_chunk_summary.csv": 35346,
    "data/derived/value_labels_plot_arc_summary.csv": 31814,
    "data/derived/cross_cultural_book_summaries.csv": 192,
    "data/derived/cross_cultural_value_labels.csv": 6747,
    "data/reference/value_taxonomy.csv": 62,
}

FORBIDDEN_DIR_NAMES = {
    "DATA_BookToMoral_FullText",
    "SUPERCANON",
    "WORLDLIT_5LangSample",
    "CONLIT_BestsellersANDLowRated",
    "CHICAGO_LowLibraries",
}

REQUIRED_FILES = {
    "scripts/score_section_4_1_all_pairs.py",
    "scripts/analyze_story_morals.R",
    "scripts/analyze_value_label_stability.R",
    "scripts/analyze_cross_input_values.R",
    "scripts/analyze_cross_cultural_case_study.R",
    "scripts/validate_release.py",
    "data/derived/section_4_1_scoring_metadata.json",
    "data/README.md",
    "requirements-r.txt",
    "requirements-python.txt",
    "figures/figure_2_input_similarity.pdf",
    "figures/figure_3_label_consistency.pdf",
    "results/section_4_1_change_type_contrast.csv",
    "results/section_4_1_input_pair_by_class_lrt.csv",
    "results/section_4_1_gpt_gemini_sensitivity_emmeans.csv",
    "results/section_4_2_statement_stability.csv",
    "results/section_4_2_label_discovery.csv",
    "results/section_4_3_global_fidelity.csv",
    "results/section_4_3_fidelity_by_class.csv",
    "results/section_4_4_fidelity.csv",
    "results/section_4_4_rank_agreement.csv",
}

SECTION_4_1_MODEL_REVISION = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41"


def count_csv_rows(path: Path) -> tuple[int, list[str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.reader(handle)
        header = next(reader)
        return sum(1 for _ in reader), header


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def validate() -> list[str]:
    errors = []
    for rel in sorted(REQUIRED_FILES):
        if not (ROOT / rel).is_file():
            errors.append(f"missing required replication file: {rel}")

    for rel, expected in EXPECTED_ROWS.items():
        path = ROOT / rel
        if not path.exists():
            errors.append(f"missing required file: {rel}")
            continue
        observed, header = count_csv_rows(path)
        if observed != expected:
            errors.append(f"{rel}: expected {expected} rows, found {observed}")
        if any(name.lower() in {"text", "full_text", "fulltext"} for name in header):
            errors.append(f"{rel}: possible full-text column in header: {header}")

    for path in ROOT.rglob("*"):
        if path.is_dir() and path.name in FORBIDDEN_DIR_NAMES:
            errors.append(f"forbidden corpus directory present: {path.relative_to(ROOT)}")

    summaries = ROOT / "data/derived/book_summaries.csv"
    if summaries.exists():
        _, header = count_csv_rows(summaries)
        expected = {"book_id", "chunk_summary", "plot_arc_summary", "book_class"}
        if not expected.issubset(header):
            errors.append("book_summaries.csv is missing required summary/metadata columns")

    cultural_summaries = ROOT / "data/derived/cross_cultural_book_summaries.csv"
    if cultural_summaries.exists():
        _, header = count_csv_rows(cultural_summaries)
        expected = {"book_id", "culture", "chunk_summary", "short_summary"}
        if set(header) != expected:
            errors.append("cross_cultural_book_summaries.csv has an unexpected schema")

    cultural_labels = ROOT / "data/derived/cross_cultural_value_labels.csv"
    if cultural_labels.exists():
        with cultural_labels.open("r", encoding="utf-8-sig", newline="") as handle:
            rows = list(csv.DictReader(handle))
        books_by_culture = {}
        conditions_by_book = {}
        for row in rows:
            books_by_culture.setdefault(row["culture"], set()).add(row["book_id"])
            conditions_by_book.setdefault(row["book_id"], set()).add(row["input_condition"])
        observed_counts = {key: len(value) for key, value in books_by_culture.items()}
        if observed_counts != {"DE": 98, "JP": 94}:
            errors.append(
                "cross-cultural sample must contain exactly 98 DE and 94 JP books"
            )
        expected_conditions = {"full_text", "chunk_summary", "short_summary"}
        if any(value != expected_conditions for value in conditions_by_book.values()):
            errors.append("cross-cultural books are not complete across conditions")

    metadata_path = ROOT / "data/derived/section_4_1_scoring_metadata.json"
    morals_path = ROOT / "data/derived/story_morals.csv"
    if metadata_path.exists() and morals_path.exists():
        try:
            metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError) as exc:
            errors.append(f"invalid Section 4.1 scoring metadata: {exc}")
        else:
            if metadata.get("embedding_revision") != SECTION_4_1_MODEL_REVISION:
                errors.append("Section 4.1 embedding revision does not match the release")
            if metadata.get("input_sha256") != sha256(morals_path):
                errors.append("Section 4.1 scoring metadata does not match story_morals.csv")

    return errors


# Local files that are git-ignored and therefore never published (mirrors
# .gitignore). They must stay out of the manifest, or a fresh clone fails it.
MANIFEST_EXCLUDED_NAMES = {"MANIFEST.sha256", ".DS_Store", ".RData", ".Rhistory"}
MANIFEST_EXCLUDED_DIRS = {
    ".git", "__pycache__", ".Rproj.user", ".ipynb_checkpoints", ".venv",
}


def is_manifest_target(path: Path) -> bool:
    rel = path.relative_to(ROOT)
    if not path.is_file() or path.name in MANIFEST_EXCLUDED_NAMES:
        return False
    if path.suffix in {".pyc", ".pyo", ".pyd"}:
        return False
    if MANIFEST_EXCLUDED_DIRS.intersection(rel.parts):
        return False
    return rel.parts[:2] != ("renv", "library")


def write_manifest() -> None:
    targets = [path for path in ROOT.rglob("*") if is_manifest_target(path)]
    lines = [f"{sha256(path)}  {path.relative_to(ROOT).as_posix()}" for path in sorted(targets)]
    (ROOT / "MANIFEST.sha256").write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description="Validate the summary-only replication package.")
    parser.add_argument("--write-manifest", action="store_true")
    args = parser.parse_args()

    errors = validate()
    if errors:
        for error in errors:
            print(f"ERROR: {error}")
        raise SystemExit(1)

    if args.write_manifest:
        write_manifest()
        print("Validation passed; wrote MANIFEST.sha256.")
    else:
        print("Validation passed.")


if __name__ == "__main__":
    main()
