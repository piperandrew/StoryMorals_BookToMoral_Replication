# From Book to Moral: replication package

Data and code supporting **“From Book to Moral: Evaluating the stability of
LLM-generated moral values of stories.”** This is an analysis-only,
summary-safe release: it contains generated summaries, morals, value labels,
and similarity scores, but no novel-length source texts.

## Repository contents

```text
data/
  derived/    analysis inputs and summary-only book representations
  reference/  fixed value taxonomy
scripts/      four paper analyses, optional similarity scoring, and validation
results/      deposited reference tables, diagnostics, and session information
figures/      deposited paper figures
```

The file-level schemas and row counts are documented in
[`data/README.md`](data/README.md). `MANIFEST.sha256` records checksums for every
replication file, including the deposited outputs.

## Setup

The analyses require R and the packages listed in `requirements-r.txt`:

```r
install.packages(readLines("requirements-r.txt"))
```

Python is needed only to rebuild the Section 4.1 cosine-similarity tables from
the released morals:

```bash
python3 -m pip install -r requirements-python.txt
```

The scored pair tables are already included, so the Python step is optional.
Rebuilding them downloads the pinned `all-MiniLM-L6-v2` model revision.

## Reproduce the analyses

Run from the repository root:

```bash
python3 scripts/validate_release.py
Rscript scripts/analyze_story_morals.R
Rscript scripts/analyze_value_label_stability.R
Rscript scripts/analyze_cross_input_values.R
Rscript scripts/analyze_cross_cultural_case_study.R
```

To rebuild the Section 4.1 pair scores before running its R analysis:

```bash
python3 scripts/score_section_4_1_all_pairs.py
```

Each R script validates its expected sample design and stops if a paper-facing
replication check fails. Generated CSVs and session information are written to
`results/`. Sections 4.1 and 4.2 also regenerate
`figures/figure_2_input_similarity.pdf` and
`figures/figure_3_label_consistency.pdf`.

The repository includes the outputs produced with the released environment so
that users can compare their reruns directly against the deposited results.
Rerunning a script overwrites only that section's corresponding output files.
Exact file checksums are recorded in `MANIFEST.sha256`; stochastic permutation
and bootstrap outputs can differ slightly across R/package versions despite the
fixed seeds, while the paper-facing conclusions should remain unchanged.

For a quick Section 4.4 plumbing check, stochastic iterations can be reduced:

```bash
N_BOOT=20 N_PERM=19 N_SPLIT=20 \
  Rscript scripts/analyze_cross_cultural_case_study.R
```

Use the default 1,000 bootstrap draws and 999 permutations for the reported
Section 4.4 values.

## Analysis definitions and expected checkpoints

### 4.1 Input sensitivity of moral generation

Each input condition produces three morals. All nine cross-condition cosine
scores are averaged into one book × generating-model × input-pair observation.
The script fits mixed models with book random intercepts.

- 37,665 cross-input moral pairs yield 4,185 complete set comparisons.
- Full-text/chunk similarity: 0.3388, 95% CI [0.3338, 0.3437].
- Input sensitivity varies by class: likelihood-ratio
  chi-square(20) = 182.97, p < .001.
- Within-full-text diversity benchmark: 0.2677.
- Balanced input-change and model-change means are 0.3235 and 0.3229;
  difference = 0.00066, p = .593, using 245 books.

### 4.2 Value-labeling stability

One sampled full-text moral per book is labeled by three models over ten
randomized label orders. Labels outside the fixed taxonomy are reported and
excluded.

- 250 morals, 7,496 of 7,500 intended attempts, and 34,915 emissions.
- Mean mode frequency by labeling model: 0.9972–1.0000.
- Mean top-three between-model overlap: 1.976–2.044 labels.
- Statement-level rank stability: rho = 0.8869 at k=1, 0.9492 at k=3,
  and 0.9791 at k=7.

### 4.3 Input sensitivity of value distributions

Each input condition independently sampled one moral per book. Distributions
are therefore standardized equally across generating-model × labeling-model
cells. Core labels appear in more than half of recorded attempts.

| Summary | TVD | Cohen’s w | Holm p |
|---|---:|---:|---:|
| Chunk | 0.12681 | 0.16144 | .01798 |
| Plot arc | 0.11983 | 0.15685 | .02498 |

The descriptive arithmetic mixture of the two standardized distributions has
TVD = 0.08106 and w = 0.11289; it is not assigned a permutation p-value.

### 4.4 German–Japanese case study

The analysis averages normalized book-level value vectors within 98 German and
94 Japanese books. Retention is
`R = TVD_summary / TVD_full`; bootstrap resampling preserves book pairing across
conditions. The ensemble pools three chunk and three short-summary morals per
book.

| Condition | DE–JP TVD | Retention R | 95% bootstrap CI | Raw p |
|---|---:|---:|---:|---:|
| Full text | 0.2054 | 1.0000 | — | .001 |
| Chunk | 0.1796 | 0.8747 | [0.7392, 1.1500] | .001 |
| Ensemble | 0.1658 | 0.8075 | [0.6694, 1.0030] | .001 |
| Short | 0.1580 | 0.7691 | [0.6608, 1.0827] | .021 |

Within-culture full/chunk top-20 rank agreement is rho = 0.891 for DE and
0.903 for JP; both cultures share 18 of 20 labels (Jaccard = 0.818).

## Copyright boundary

No full books are included. A condition value such as `full_text` records the
input used to generate a moral or label; it is not a full-text field. The two
book-summary tables contain only generated chunk, plot-arc, or short summaries.
