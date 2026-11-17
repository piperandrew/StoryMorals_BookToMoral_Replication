# Data dictionary

All files are UTF-8 CSVs. Book identifiers are source filenames retained for joins; the corresponding full-text files are not included.

## Derived data

| File | Rows | Unit of observation | Purpose |
|---|---:|---|---|
| `book_summaries.csv` | 250 | book | Metadata plus chunk and plot-arc summaries |
| `story_morals.csv` | 8,685 | generated moral | Three intended morals per book, model, and input condition; 315 expected slots are absent |
| `section_4_1_cross_input_pairs_all.csv` | 37,665 | individual moral pair | Every available 3 x 3 cross-input comparison |
| `section_4_1_cross_model_pairs_all.csv` | 25,110 | individual moral pair | Every available 3 x 3 cross-model comparison |
| `section_4_1_within_full_text_pairs.csv` | 2,250 | individual moral pair | Three within-full-text moral comparisons per book/model |
| `value_labels_full_text.csv` | 34,915 | emitted label | Ten randomized label-order runs for one sampled moral per book and three label models |
| `value_labels_chunk_summary.csv` | 35,346 | emitted label | Same design for chunk-summary morals |
| `value_labels_plot_arc_summary.csv` | 31,814 | emitted label | Same design for plot-arc-summary morals |
| `cross_cultural_book_summaries.csv` | 192 | book | Chunk and short summaries for the DE-JP case study |
| `cross_cultural_value_labels.csv` | 6,747 | emitted label | Three morals under three conditions for 98 DE and 94 JP books |

### `book_summaries.csv`

- `book_id`: source filename used as the stable identifier.
- `title`, `author_first_name`, `author_last_name`, `publication_year`: bibliographic fields.
- `language`, `country`, `book_class`: sampling metadata.
- `chunk_summary`: concatenated ten-event summaries from 14,000-token chunks.
- `plot_arc_summary`: short Beginning/Conflict/Ending representation (mean 136 words; range 66-172).

### `story_morals.csv`

- `moral_id`: identifier used by the supplied pair and value-label tables.
- `book_id` and bibliographic fields: book metadata.
- `model`: moral-generating model family (`Claude`, `Gemini`, or `GPT`).
- `input_type`: `full_text`, `chunk_summary`, `plotArc_summary`, or `memory`.
- `moral_num`: position within the requested three-moral response.
- `moral`: generated moral statement.

The nominal complete design is 250 books x 3 models x 4 inputs x 3 morals = 9,000 rows. The observed file has 8,685 rows. Missing slots are 300 Claude-memory morals and 15 Gemini-chunk morals.

### Section 4.1 similarity tables

The `*_pairs_all.csv` files retain every available individual cosine score. For
each comparison between two three-moral sets, this is the complete 3 x 3
cross-product of nine scores. The R analysis averages those scores to set-level
tables in `results/` before fitting its regressions.

The paper-facing cross-input unit is therefore one book x generating model x input-pair observation. No moral matching or outcome-dependent pair selection is used. Groups without three morals on both sides are omitted rather than averaged from fewer than nine pairs.

The input-versus-model benchmark uses the 245 books with complete coverage for full text, chunk summary, and plot-arc summary across all three generating models. Memory is excluded from this particular benchmark because Claude-memory output is missing for 100 books.

`section_4_1_scoring_metadata.json` records the pinned embedding-model revision,
input checksum, software versions, and output row counts used to create the
pair-level tables.

### `value_labels_*.csv`

- The unit is one emitted `labels` value, not one API response.
- `moral_id` is condition-local in these files; combine with the filename/condition rather than assuming it identifies the same statement across files.
- `.moral_id` is the sampled-moral sequence used by the labeling run.
- `model` generated the moral; `label_model` assigned values.
- `run` is 1-10; `order_seed` records the randomized taxonomy ordering.
- `labels` is one parsed label from the model response.

The analysis drops labels not present in `data/reference/value_taxonomy.csv`. The raw files retain them for auditability.

For the Section 4.2 full-text stability analysis, the file contains one sampled
moral per book and 7,496 of 7,500 intended moral-by-labeler-by-run attempts.
There are 308 off-taxonomy emissions; after excluding them, seven recorded
attempts contain no valid taxonomy label. The analysis script counts those
attempts in statement-level denominators but excludes their invalid emissions.

The sampled moral and its generating model were selected independently in each
input-condition file. Within book, full text and chunk use the same generating
model for 93 of 250 books; full text and plot arc match for 79 books; all three
conditions match for 32 books. Section 4.3 therefore standardizes distributions
equally across generating-model and labeling-model cells and reports a separate
same-generating-model sensitivity analysis.

### Cross-cultural case-study tables

`cross_cultural_book_summaries.csv` contains `book_id`, `culture`,
`chunk_summary`, and `short_summary`. It contains no full book text.

`cross_cultural_value_labels.csv` is the analysis table for Section 4.4:

- `book_id` and `culture` identify the 98 German and 94 Japanese books.
- `input_condition` is `full_text`, `chunk_summary`, or `short_summary`.
- `moral_index` is 1-3 and `moral_text` is the generated moral, not source text.
- `value_label` is one emitted value label.
- `model` records the single historical model (`gpt-5.4`).
- `run_seed` and `timestamp` retain generation provenance.

All 192 books occur in every condition with exactly three morals. The Section
4.4 script excludes 191 emissions representing 45 strings not found in the
released 62-label taxonomy. No book-condition loses all valid label mass.

## Reference data

`value_taxonomy.csv` maps the 62 labels used by the replication scripts
(`rokeach_value`) to ten higher-order `schwartz_value` categories.
