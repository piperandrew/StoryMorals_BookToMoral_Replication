#!/usr/bin/env python3
"""Score every moral pair needed for manuscript Section 4.1.

Each model/input condition contains three morals. Cross-condition and
cross-model comparisons retain all 3 x 3 = 9 pairwise cosine similarities;
the R analysis subsequently averages those nine scores into one set-level
observation.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
from itertools import combinations, product
from pathlib import Path

import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_INPUT = ROOT / "data" / "derived" / "story_morals.csv"
DEFAULT_OUTPUT_DIR = ROOT / "data" / "derived"
MODEL_NAME = "sentence-transformers/all-MiniLM-L6-v2"
MODEL_REVISION = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41"


def canonical_pair(left: str, right: str) -> str:
    return "__".join(sorted((left, right)))


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def embed_morals(
    df: pd.DataFrame, model_name: str, revision: str
) -> dict[int, np.ndarray]:
    model = SentenceTransformer(model_name, revision=revision)
    embeddings = model.encode(
        df["moral"].astype(str).tolist(),
        normalize_embeddings=True,
        show_progress_bar=True,
        convert_to_numpy=True,
    )
    return dict(zip(df["moral_id"].astype(int), embeddings, strict=True))


def cosine(lookup: dict[int, np.ndarray], left: int, right: int) -> float:
    # Normalized float32 embeddings can exceed the mathematical bounds by a
    # few machine-epsilon units, so clamp before serializing the cosine score.
    return float(np.clip(np.dot(lookup[int(left)], lookup[int(right)]), -1.0, 1.0))


def cross_input_pairs(df: pd.DataFrame, lookup: dict[int, np.ndarray]) -> pd.DataFrame:
    rows = []
    keys = ["book_id", "Class", "model"]
    for (book_id, book_class, model), group in df.groupby(keys, sort=True, dropna=False):
        conditions = {name: part for name, part in group.groupby("input_type", sort=True)}
        for input_a, input_b in combinations(sorted(conditions), 2):
            left, right = conditions[input_a], conditions[input_b]
            if len(left) != 3 or len(right) != 3:
                continue
            for (_, moral_a), (_, moral_b) in product(left.iterrows(), right.iterrows()):
                rows.append({
                    "book_id": book_id,
                    "Class": book_class,
                    "generating_model": model,
                    "input_type_a": input_a,
                    "input_type_b": input_b,
                    "input_pair": canonical_pair(input_a, input_b),
                    "moral_id_a": int(moral_a.moral_id),
                    "moral_id_b": int(moral_b.moral_id),
                    "moral_num_a": int(moral_a.moral_num),
                    "moral_num_b": int(moral_b.moral_num),
                    "similarity": cosine(lookup, moral_a.moral_id, moral_b.moral_id),
                })
    return pd.DataFrame(rows)


def cross_model_pairs(df: pd.DataFrame, lookup: dict[int, np.ndarray]) -> pd.DataFrame:
    rows = []
    keys = ["book_id", "Class", "input_type"]
    for (book_id, book_class, input_type), group in df.groupby(keys, sort=True, dropna=False):
        models = {name: part for name, part in group.groupby("model", sort=True)}
        for model_a, model_b in combinations(sorted(models), 2):
            left, right = models[model_a], models[model_b]
            if len(left) != 3 or len(right) != 3:
                continue
            for (_, moral_a), (_, moral_b) in product(left.iterrows(), right.iterrows()):
                rows.append({
                    "book_id": book_id,
                    "Class": book_class,
                    "input_type": input_type,
                    "model_a": model_a,
                    "model_b": model_b,
                    "model_pair": canonical_pair(model_a, model_b),
                    "moral_id_a": int(moral_a.moral_id),
                    "moral_id_b": int(moral_b.moral_id),
                    "moral_num_a": int(moral_a.moral_num),
                    "moral_num_b": int(moral_b.moral_num),
                    "similarity": cosine(lookup, moral_a.moral_id, moral_b.moral_id),
                })
    return pd.DataFrame(rows)


def within_full_text_pairs(df: pd.DataFrame, lookup: dict[int, np.ndarray]) -> pd.DataFrame:
    rows = []
    full = df[df["input_type"] == "full_text"]
    keys = ["book_id", "Class", "model"]
    for (book_id, book_class, model), group in full.groupby(keys, sort=True, dropna=False):
        if len(group) != 3:
            continue
        for (_, moral_a), (_, moral_b) in combinations(group.iterrows(), 2):
            rows.append({
                "book_id": book_id,
                "Class": book_class,
                "generating_model": model,
                "moral_id_a": int(moral_a.moral_id),
                "moral_id_b": int(moral_b.moral_id),
                "moral_num_a": int(moral_a.moral_num),
                "moral_num_b": int(moral_b.moral_num),
                "similarity": cosine(lookup, moral_a.moral_id, moral_b.moral_id),
            })
    return pd.DataFrame(rows)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--model", default=MODEL_NAME)
    parser.add_argument("--revision", default=MODEL_REVISION)
    args = parser.parse_args()

    morals = pd.read_csv(args.input)
    required = {"moral_id", "book_id", "Class", "model", "input_type", "moral_num", "moral"}
    missing = required.difference(morals.columns)
    if missing:
        raise ValueError(f"Missing required columns: {sorted(missing)}")
    if morals["moral_id"].duplicated().any():
        raise ValueError("moral_id must be unique")

    args.output_dir.mkdir(parents=True, exist_ok=True)
    embeddings = embed_morals(morals, args.model, args.revision)

    outputs = {
        "section_4_1_cross_input_pairs_all.csv": cross_input_pairs(morals, embeddings),
        "section_4_1_cross_model_pairs_all.csv": cross_model_pairs(morals, embeddings),
        "section_4_1_within_full_text_pairs.csv": within_full_text_pairs(morals, embeddings),
    }
    for filename, table in outputs.items():
        path = args.output_dir / filename
        table.to_csv(path, index=False)
        print(f"{filename}: {len(table):,} rows")

    metadata = {
        "analysis": "paper Section 4.1 all-pairs cosine scoring",
        "input_file": args.input.name,
        "input_sha256": file_sha256(args.input),
        "input_rows": len(morals),
        "embedding_model": args.model,
        "embedding_revision": args.revision,
        "normalized_embeddings": True,
        "python": platform.python_version(),
        "numpy": np.__version__,
        "pandas": pd.__version__,
        "sentence_transformers": __import__("sentence_transformers").__version__,
        "output_rows": {name: len(table) for name, table in outputs.items()},
    }
    metadata_path = args.output_dir / "section_4_1_scoring_metadata.json"
    metadata_path.write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    print(f"{metadata_path.name}: scoring provenance")


if __name__ == "__main__":
    main()
