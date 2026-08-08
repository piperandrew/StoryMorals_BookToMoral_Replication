#!/usr/bin/env Rscript

###############################################################################
# PAPER SECTION 4.4: CROSS-CULTURAL COMPARISON CASE STUDY
#
# Question: does the DE-JP difference in value distributions observed from full
# books survive when the same books are represented by summaries?
#
# Unit of analysis: book. Each book-condition vector pools taxonomy-valid labels
# across its three morals, normalizes them to sum to one, and contributes equally
# to its culture centroid. The ensemble pools the chunk and short morals before
# normalization. All bootstrap resampling preserves the book-level pairing
# across input conditions.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
})

DATA_FILE <- file.path("data", "derived", "cross_cultural_value_labels.csv")
SUMMARY_FILE <- file.path("data", "derived", "cross_cultural_book_summaries.csv")
TAXONOMY_FILE <- file.path("data", "reference", "value_taxonomy.csv")
RESULTS_DIR <- "results"

N_BOOT <- as.integer(Sys.getenv("N_BOOT", "1000"))
N_PERM <- as.integer(Sys.getenv("N_PERM", "999"))
N_SPLIT <- as.integer(Sys.getenv("N_SPLIT", "200"))
SEED <- 1L
TOP_N <- 20L

RAW_CONDITIONS <- c("full_text", "chunk_summary", "short_summary")
ALL_CONDITIONS <- c(RAW_CONDITIONS, "ensemble")
CULTURES <- c("DE", "JP")
EXPECTED_BOOKS <- c(DE = 98L, JP = 94L)

dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

assert_equal <- function(observed, expected, label) {
  if (!identical(as.integer(observed), as.integer(expected))) {
    stop(sprintf("Replication check failed for %s: expected %s, found %s",
                 label, expected, observed), call. = FALSE)
  }
}

assert_near <- function(observed, expected, tolerance, label) {
  if (length(observed) != 1L || is.na(observed) ||
      abs(observed - expected) > tolerance) {
    stop(sprintf(
      "Replication check failed for %s: expected %.4f +/- %.4f, found %.4f",
      label, expected, tolerance, observed
    ), call. = FALSE)
  }
}

tvd <- function(p, q) 0.5 * sum(abs(p - q))

###############################################################################
# SECTION 4.4A: LOAD AND VALIDATE THE RELEASED CASE-STUDY TABLES
###############################################################################

raw <- read_csv(DATA_FILE, show_col_types = FALSE)
summaries <- read_csv(SUMMARY_FILE, show_col_types = FALSE)
taxonomy <- read_csv(TAXONOMY_FILE, show_col_types = FALSE) %>%
  transmute(value_label = trimws(rokeach_value)) %>%
  distinct(value_label) %>%
  pull(value_label)

required_raw <- c(
  "book_id", "culture", "input_condition", "moral_index", "moral_text",
  "value_label", "model", "run_seed", "timestamp"
)
required_summaries <- c("book_id", "culture", "chunk_summary", "short_summary")

missing_raw <- setdiff(required_raw, names(raw))
missing_summaries <- setdiff(required_summaries, names(summaries))
if (length(missing_raw) > 0L) {
  stop("Case-study label table is missing: ", paste(missing_raw, collapse = ", "),
       call. = FALSE)
}
if (length(missing_summaries) > 0L) {
  stop("Case-study summary table is missing: ",
       paste(missing_summaries, collapse = ", "), call. = FALSE)
}

assert_equal(nrow(raw), 6747L, "raw label rows")
assert_equal(nrow(summaries), 192L, "summary rows")
assert_equal(n_distinct(raw$book_id), 192L, "books in label table")
assert_equal(n_distinct(summaries$book_id), 192L, "books in summary table")
assert_equal(length(taxonomy), 62L, "taxonomy labels")
assert_equal(n_distinct(raw$model), 1L, "models")

if (!identical(sort(unique(raw$culture)), sort(CULTURES))) {
  stop("Expected cultures DE and JP only.", call. = FALSE)
}
if (!identical(sort(unique(raw$input_condition)), sort(RAW_CONDITIONS))) {
  stop("Expected full_text, chunk_summary, and short_summary only.", call. = FALSE)
}
if (!identical(unique(raw$model), "gpt-5.4")) {
  stop("Expected the historical single-model case study to use gpt-5.4.",
       call. = FALSE)
}

book_counts <- raw %>%
  distinct(book_id, culture) %>%
  count(culture, name = "books") %>%
  arrange(match(culture, CULTURES))
for (culture in CULTURES) {
  assert_equal(
    book_counts$books[book_counts$culture == culture],
    EXPECTED_BOOKS[[culture]],
    paste(culture, "books")
  )
}

summary_counts <- summaries %>%
  count(culture, name = "books")
for (culture in CULTURES) {
  assert_equal(
    summary_counts$books[summary_counts$culture == culture],
    EXPECTED_BOOKS[[culture]],
    paste(culture, "summary rows")
  )
}

if (!setequal(raw$book_id, summaries$book_id)) {
  stop("Label and summary tables do not contain the same books.", call. = FALSE)
}
if (any(is.na(summaries$chunk_summary) | summaries$chunk_summary == "") ||
    any(is.na(summaries$short_summary) | summaries$short_summary == "")) {
  stop("Every case-study book must have both released summaries.", call. = FALSE)
}

moral_design <- raw %>%
  distinct(book_id, culture, input_condition, moral_index, moral_text) %>%
  count(book_id, culture, input_condition, name = "morals")
if (any(moral_design$morals != 3L) || nrow(moral_design) != 192L * 3L) {
  stop("Expected exactly three morals for every book and raw input condition.",
       call. = FALSE)
}

book_condition_design <- raw %>%
  distinct(book_id, culture, input_condition) %>%
  count(book_id, culture, name = "conditions")
if (any(book_condition_design$conditions != length(RAW_CONDITIONS))) {
  stop("Every book must be observed in all three raw input conditions.",
       call. = FALSE)
}

off_taxonomy <- raw %>%
  filter(!is.na(value_label), !value_label %in% taxonomy)
valid <- raw %>%
  filter(value_label %in% taxonomy)

assert_equal(nrow(off_taxonomy), 191L, "off-taxonomy rows")
assert_equal(n_distinct(off_taxonomy$value_label), 45L, "off-taxonomy label types")

zero_valid_cells <- raw %>%
  distinct(book_id, culture, input_condition) %>%
  anti_join(
    valid %>% distinct(book_id, culture, input_condition),
    by = c("book_id", "culture", "input_condition")
  )
assert_equal(nrow(zero_valid_cells), 0L,
             "book-condition cells with no taxonomy-valid label")

analysis_inventory <- tibble(
  raw_rows = nrow(raw),
  taxonomy_valid_rows = nrow(valid),
  off_taxonomy_rows = nrow(off_taxonomy),
  off_taxonomy_label_types = n_distinct(off_taxonomy$value_label),
  books = n_distinct(raw$book_id),
  de_books = book_counts$books[book_counts$culture == "DE"],
  jp_books = book_counts$books[book_counts$culture == "JP"],
  raw_conditions = n_distinct(raw$input_condition),
  morals = nrow(raw %>% distinct(book_id, input_condition, moral_index, moral_text)),
  models = n_distinct(raw$model),
  model = unique(raw$model)
)
write_csv(analysis_inventory,
          file.path(RESULTS_DIR, "section_4_4_analysis_inventory.csv"))
write_csv(
  off_taxonomy %>% count(value_label, sort = TRUE, name = "rows"),
  file.path(RESULTS_DIR, "section_4_4_off_taxonomy_labels.csv")
)

###############################################################################
# SECTION 4.4B: BOOK VECTORS AND CULTURE-LEVEL TVD EFFECT SIZES
###############################################################################

# The ensemble contains six distinct morals per book: three from each summary.
ensemble <- valid %>%
  filter(input_condition %in% c("chunk_summary", "short_summary")) %>%
  mutate(
    moral_index = if_else(input_condition == "short_summary",
                          moral_index + 3L, moral_index),
    input_condition = "ensemble"
  )
analysis_rows <- bind_rows(valid, ensemble)

build_book_vectors <- function(data, vocabulary) {
  keys <- data %>% distinct(book_id, culture, input_condition)
  counts <- data %>%
    distinct(book_id, culture, input_condition, moral_index, value_label) %>%
    count(book_id, culture, input_condition, value_label, name = "n") %>%
    pivot_wider(
      id_cols = c(book_id, culture, input_condition),
      names_from = value_label,
      values_from = n,
      values_fill = 0
    )

  missing_labels <- setdiff(vocabulary, names(counts))
  if (length(missing_labels) > 0L) counts[missing_labels] <- 0

  wide <- keys %>%
    left_join(counts, by = c("book_id", "culture", "input_condition")) %>%
    mutate(across(all_of(vocabulary), ~replace_na(.x, 0))) %>%
    select(book_id, culture, input_condition, all_of(vocabulary))

  mat <- as.matrix(wide[vocabulary])
  totals <- rowSums(mat)
  if (any(totals == 0)) {
    stop("A book-condition vector has no taxonomy-valid label mass.",
         call. = FALSE)
  }
  wide[vocabulary] <- sweep(mat, 1, totals, "/")
  wide
}

book_vectors <- build_book_vectors(analysis_rows, taxonomy)
if (nrow(book_vectors) != 192L * length(ALL_CONDITIONS)) {
  stop("Book-vector table is incomplete.", call. = FALSE)
}

compute_distances <- function(vectors) {
  map_dfr(ALL_CONDITIONS, function(condition) {
    de <- vectors %>%
      filter(culture == "DE", input_condition == condition) %>%
      select(all_of(taxonomy)) %>%
      as.matrix() %>%
      colMeans()
    jp <- vectors %>%
      filter(culture == "JP", input_condition == condition) %>%
      select(all_of(taxonomy)) %>%
      as.matrix() %>%
      colMeans()
    tibble(condition, TVD = tvd(de, jp))
  }) %>%
    mutate(
      full_text_TVD = TVD[condition == "full_text"],
      retention_R = if_else(condition == "full_text", 1, TVD / full_text_TVD)
    )
}

point_distances <- compute_distances(book_vectors)

###############################################################################
# SECTION 4.4C: PAIRED BOOK BOOTSTRAP FOR RETENTION R
###############################################################################

books_by_culture <- book_vectors %>% distinct(culture, book_id)
set.seed(SEED)
bootstrap_draws <- map_dfr(seq_len(N_BOOT), function(iteration) {
  sampled <- books_by_culture %>%
    group_by(culture) %>%
    reframe(
      book_id = sample(book_id, size = n(), replace = TRUE),
      draw_id = seq_len(n())
    )
  sampled_vectors <- sampled %>%
    left_join(
      book_vectors,
      by = c("culture", "book_id"),
      relationship = "many-to-many"
    )
  compute_distances(sampled_vectors) %>%
    filter(condition != "full_text") %>%
    transmute(iteration, condition, retention_R)
})

bootstrap_ci <- bootstrap_draws %>%
  group_by(condition) %>%
  summarise(
    bootstrap_mean_R = mean(retention_R),
    R_ci_low = quantile(retention_R, 0.025),
    R_ci_high = quantile(retention_R, 0.975),
    .groups = "drop"
  )

fidelity_results <- point_distances %>%
  left_join(bootstrap_ci, by = "condition")
write_csv(fidelity_results,
          file.path(RESULTS_DIR, "section_4_4_fidelity.csv"))

###############################################################################
# SECTION 4.4D: CULTURE-LABEL PERMUTATION TESTS
###############################################################################

permutation_test <- function(vectors, condition) {
  sub <- vectors %>% filter(input_condition == condition)
  labels <- sub$culture
  mat <- as.matrix(sub[taxonomy])
  observed <- point_distances$TVD[point_distances$condition == condition]

  # Resetting the seed by condition reproduces the historical analysis exactly.
  set.seed(SEED)
  null <- replicate(N_PERM, {
    permuted <- sample(labels)
    de <- colMeans(mat[permuted == "DE", , drop = FALSE])
    jp <- colMeans(mat[permuted == "JP", , drop = FALSE])
    tvd(de, jp)
  })
  tibble(
    condition,
    observed_TVD = observed,
    null_mean_TVD = mean(null),
    permutation_p = (1 + sum(null >= observed)) / (N_PERM + 1)
  )
}

permutation_results <- map_dfr(ALL_CONDITIONS, ~permutation_test(book_vectors, .x)) %>%
  mutate(summary_family_holm_p = NA_real_)
summary_rows <- permutation_results$condition != "full_text"
permutation_results$summary_family_holm_p[summary_rows] <- p.adjust(
  permutation_results$permutation_p[summary_rows], method = "holm"
)
write_csv(permutation_results,
          file.path(RESULTS_DIR, "section_4_4_permutation_tests.csv"))

###############################################################################
# SECTION 4.4E: FULL-TEXT VERSUS CHUNK RANK AGREEMENT WITHIN CULTURE
###############################################################################

book_denominators <- valid %>%
  distinct(culture, input_condition, book_id) %>%
  count(culture, input_condition, name = "books")

frequency_table <- valid %>%
  distinct(culture, input_condition, book_id, value_label) %>%
  count(culture, input_condition, value_label, name = "books_with_value") %>%
  left_join(book_denominators, by = c("culture", "input_condition")) %>%
  mutate(proportion = books_with_value / books)

rank_agreement <- map_dfr(CULTURES, function(culture_name) {
  reference <- frequency_table %>%
    filter(culture == culture_name, input_condition == "full_text") %>%
    arrange(desc(proportion), value_label) %>%
    slice_head(n = TOP_N)
  comparison <- frequency_table %>%
    filter(culture == culture_name, input_condition == "chunk_summary") %>%
    arrange(desc(proportion), value_label)

  comparison_props <- map_dbl(reference$value_label, function(label) {
    found <- comparison$proportion[comparison$value_label == label]
    if (length(found) == 0L) 0 else found[[1]]
  })
  comparison_top <- comparison %>% slice_head(n = TOP_N) %>% pull(value_label)
  shared <- length(intersect(reference$value_label, comparison_top))

  tibble(
    culture = culture_name,
    reference_condition = "full_text",
    comparison_condition = "chunk_summary",
    top_n = TOP_N,
    spearman_rho = cor(reference$proportion, comparison_props,
                       method = "spearman"),
    top_n_shared = shared,
    top_n_jaccard = shared / length(union(reference$value_label, comparison_top))
  )
})

rank_detail <- map_dfr(CULTURES, function(culture_name) {
  reference <- frequency_table %>%
    filter(culture == culture_name, input_condition == "full_text") %>%
    arrange(desc(proportion), value_label) %>%
    slice_head(n = TOP_N) %>%
    mutate(full_text_rank = row_number()) %>%
    select(value_label, full_text_proportion = proportion, full_text_rank)
  comparison <- frequency_table %>%
    filter(culture == culture_name, input_condition == "chunk_summary") %>%
    arrange(desc(proportion), value_label) %>%
    mutate(chunk_rank = row_number()) %>%
    select(value_label, chunk_proportion = proportion, chunk_rank)

  reference %>%
    left_join(comparison, by = "value_label") %>%
    mutate(
      culture = culture_name,
      chunk_proportion = replace_na(chunk_proportion, 0),
      chunk_rank = replace_na(chunk_rank, Inf),
      rank_change = chunk_rank - full_text_rank,
      percentage_point_change = 100 * (chunk_proportion - full_text_proportion)
    ) %>%
    select(culture, everything())
})

write_csv(rank_agreement,
          file.path(RESULTS_DIR, "section_4_4_rank_agreement.csv"))
write_csv(rank_detail,
          file.path(RESULTS_DIR, "section_4_4_rank_agreement_detail.csv"))

###############################################################################
# SECTION 4.4F: WITHIN-CULTURE SPLIT-HALF DESCRIPTIVE BENCHMARK
###############################################################################

set.seed(SEED)
split_half <- map_dfr(CULTURES, function(culture_name) {
  sub <- book_vectors %>%
    filter(culture == culture_name, input_condition == "full_text")
  books <- sub$book_id
  draws <- replicate(N_SPLIT, {
    half <- sample(books, floor(length(books) / 2), replace = FALSE)
    a <- sub %>% filter(book_id %in% half) %>% select(all_of(taxonomy)) %>%
      as.matrix() %>% colMeans()
    b <- sub %>% filter(!book_id %in% half) %>% select(all_of(taxonomy)) %>%
      as.matrix() %>% colMeans()
    tvd(a, b)
  })
  tibble(
    culture = culture_name,
    books = length(books),
    mean_split_half_TVD = mean(draws),
    split_half_TVD_ci_low = quantile(draws, 0.025),
    split_half_TVD_ci_high = quantile(draws, 0.975)
  )
})
write_csv(split_half,
          file.path(RESULTS_DIR, "section_4_4_split_half_benchmark.csv"))

###############################################################################
# PAPER-FACING REPLICATION CHECKS AND CONSOLE SUMMARY
###############################################################################

get_result <- function(condition, column, table = fidelity_results) {
  table[[column]][table$condition == condition]
}

assert_near(get_result("chunk_summary", "retention_R"), 0.875, 0.002,
            "chunk retention R")
assert_near(get_result("ensemble", "retention_R"), 0.807, 0.002,
            "ensemble retention R")
assert_near(get_result("short_summary", "retention_R"), 0.769, 0.002,
            "short retention R")
if (N_BOOT == 1000L && N_PERM == 999L) {
  assert_near(get_result("chunk_summary", "R_ci_low"), 0.739, 0.004,
              "chunk lower CI")
  assert_near(get_result("chunk_summary", "R_ci_high"), 1.15, 0.01,
              "chunk upper CI")
  assert_near(
    permutation_results$permutation_p[
      permutation_results$condition == "short_summary"
    ],
    0.021, 0.0001, "short-summary permutation p"
  )
} else {
  message("Skipping stochastic paper-value assertions because iteration counts ",
          "differ from N_BOOT=1000 and N_PERM=999.")
}
assert_near(rank_agreement$spearman_rho[rank_agreement$culture == "DE"],
            0.891, 0.001, "DE rank agreement")
assert_near(rank_agreement$spearman_rho[rank_agreement$culture == "JP"],
            0.903, 0.001, "JP rank agreement")

summary_lines <- c(
  "Section 4.4 cross-cultural case study",
  sprintf("Books: DE=%d; JP=%d", EXPECTED_BOOKS[["DE"]], EXPECTED_BOOKS[["JP"]]),
  sprintf("Full-text DE-JP TVD: %.4f (permutation p=%.3f)",
          get_result("full_text", "TVD"),
          permutation_results$permutation_p[
            permutation_results$condition == "full_text"
          ]),
  sprintf("Chunk: TVD=%.4f; R=%.3f [%.3f, %.3f]; p=%.3f",
          get_result("chunk_summary", "TVD"),
          get_result("chunk_summary", "retention_R"),
          get_result("chunk_summary", "R_ci_low"),
          get_result("chunk_summary", "R_ci_high"),
          permutation_results$permutation_p[
            permutation_results$condition == "chunk_summary"
          ]),
  sprintf("Ensemble: TVD=%.4f; R=%.3f [%.3f, %.3f]; p=%.3f",
          get_result("ensemble", "TVD"),
          get_result("ensemble", "retention_R"),
          get_result("ensemble", "R_ci_low"),
          get_result("ensemble", "R_ci_high"),
          permutation_results$permutation_p[
            permutation_results$condition == "ensemble"
          ]),
  sprintf("Short: TVD=%.4f; R=%.3f [%.3f, %.3f]; p=%.3f",
          get_result("short_summary", "TVD"),
          get_result("short_summary", "retention_R"),
          get_result("short_summary", "R_ci_low"),
          get_result("short_summary", "R_ci_high"),
          permutation_results$permutation_p[
            permutation_results$condition == "short_summary"
          ]),
  sprintf("Rank agreement: DE rho=%.3f; JP rho=%.3f",
          rank_agreement$spearman_rho[rank_agreement$culture == "DE"],
          rank_agreement$spearman_rho[rank_agreement$culture == "JP"])
)
writeLines(summary_lines,
           file.path(RESULTS_DIR, "section_4_4_console_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
