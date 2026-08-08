#!/usr/bin/env Rscript

###############################################################################
# PAPER SECTION 4.2: VALUE LABELING STABILITY
#
# Scope:
#   one sampled full-text moral per book x 250 books
#   three labeling models x ten intended randomized-order runs
#
# All run-convergence analyses use deterministic accumulation of runs 1,...,k
# and compare with the complete ten-run reference. Off-taxonomy emissions are
# retained in the source CSV but excluded from the fixed-taxonomy analysis.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(scales)
  library(tidyr)
})

DATA_FILE <- file.path("data", "derived", "value_labels_full_text.csv")
TAXONOMY_FILE <- file.path("data", "reference", "value_taxonomy.csv")
RESULTS_DIR <- "results"
FIGURES_DIR <- "figures"
K_MAX <- 10L

dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

assert_replication_value <- function(observed, expected, label) {
  if (!identical(as.integer(observed), as.integer(expected))) {
    stop(
      sprintf("Replication check failed for %s: expected %s, found %s",
              label, expected, observed),
      call. = FALSE
    )
  }
}

###############################################################################
# SECTION 4.2A: LOAD, VALIDATE, AND ENFORCE THE FIXED TAXONOMY
###############################################################################

raw_labels <- read_csv(DATA_FILE, show_col_types = FALSE)
taxonomy <- read_csv(TAXONOMY_FILE, show_col_types = FALSE) %>%
  transmute(labels = trimws(rokeach_value)) %>%
  distinct(labels)

required_columns <- c(
  "moral_id", "book_id", "label_model", "run", "order_seed", "labels"
)
missing_columns <- setdiff(required_columns, names(raw_labels))
if (length(missing_columns) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")),
       call. = FALSE)
}

raw_labels <- raw_labels %>%
  mutate(
    labels = trimws(labels),
    attempt_id = paste(moral_id, label_model, run, order_seed, sep = "__")
  )

assert_replication_value(nrow(raw_labels), 34915, "raw label emissions")
assert_replication_value(n_distinct(raw_labels$book_id), 250, "books")
assert_replication_value(n_distinct(raw_labels$moral_id), 250, "sampled morals")
assert_replication_value(n_distinct(raw_labels$label_model), 3, "labeling models")
assert_replication_value(nrow(taxonomy), 62, "taxonomy labels")

attempt_inventory <- raw_labels %>%
  distinct(moral_id, book_id, label_model, run, order_seed, attempt_id)
assert_replication_value(nrow(attempt_inventory), 7496, "recorded labeling attempts")

off_taxonomy <- raw_labels %>% filter(!labels %in% taxonomy$labels)
valid_labels <- raw_labels %>%
  filter(labels %in% taxonomy$labels) %>%
  distinct(attempt_id, labels, .keep_all = TRUE)

zero_valid_attempts <- setdiff(
  attempt_inventory$attempt_id,
  unique(valid_labels$attempt_id)
)

assert_replication_value(nrow(off_taxonomy), 308, "off-taxonomy emissions")
assert_replication_value(length(zero_valid_attempts), 7, "attempts with no valid label")

# Each successfully parsed attempt contributes total mass 1, so verbose
# responses do not receive greater weight in corpus-level distributions.
valid_labels <- valid_labels %>%
  group_by(attempt_id) %>%
  mutate(weight = 1 / n()) %>%
  ungroup()

analysis_inventory <- tibble(
  quantity = c(
    "books", "sampled_morals", "labeling_models", "intended_attempts",
    "recorded_attempts", "valid_attempts", "raw_label_emissions",
    "valid_label_emissions", "off_taxonomy_emissions",
    "attempts_with_no_valid_label", "taxonomy_labels", "observed_valid_labels"
  ),
  value = c(
    n_distinct(raw_labels$book_id),
    n_distinct(raw_labels$moral_id),
    n_distinct(raw_labels$label_model),
    n_distinct(raw_labels$moral_id) * n_distinct(raw_labels$label_model) * K_MAX,
    nrow(attempt_inventory),
    n_distinct(valid_labels$attempt_id),
    nrow(raw_labels),
    nrow(valid_labels),
    nrow(off_taxonomy),
    length(zero_valid_attempts),
    nrow(taxonomy),
    n_distinct(valid_labels$labels)
  )
)
write_csv(analysis_inventory, file.path(RESULTS_DIR, "section_4_2_analysis_inventory.csv"))

off_taxonomy_summary <- off_taxonomy %>%
  count(labels, sort = TRUE, name = "n_emissions")
write_csv(
  off_taxonomy_summary,
  file.path(RESULTS_DIR, "section_4_2_off_taxonomy_labels.csv")
)

###############################################################################
# SECTION 4.2B: WITHIN-MODEL, STATEMENT-LEVEL STABILITY
###############################################################################

# Denominators include every recorded attempt, including attempts whose only
# emitted labels were outside the taxonomy. Such attempts count as non-hits for
# every valid label.
n_attempts <- attempt_inventory %>%
  count(moral_id, label_model, name = "n_attempts")

label_frequency <- valid_labels %>%
  distinct(moral_id, label_model, labels, attempt_id) %>%
  count(moral_id, label_model, labels, name = "n_hit") %>%
  left_join(n_attempts, by = c("moral_id", "label_model")) %>%
  mutate(frequency = n_hit / n_attempts) %>%
  arrange(moral_id, label_model, desc(frequency), labels)

statement_stability <- label_frequency %>%
  group_by(moral_id, label_model) %>%
  summarise(
    mode_frequency = first(frequency),
    top3_mean_frequency = mean(head(frequency, 3)),
    .groups = "drop"
  )

statement_summary <- statement_stability %>%
  group_by(label_model) %>%
  summarise(
    n_morals = n(),
    mean_mode_frequency = mean(mode_frequency),
    min_mode_frequency = min(mode_frequency),
    mean_top3_frequency = mean(top3_mean_frequency),
    .groups = "drop"
  )
write_csv(
  statement_summary,
  file.path(RESULTS_DIR, "section_4_2_statement_stability.csv")
)

###############################################################################
# SECTION 4.2C: BETWEEN-LABELING-MODEL TOP-3 OVERLAP
###############################################################################

top3_labels <- label_frequency %>%
  group_by(moral_id, label_model) %>%
  slice_head(n = 3) %>%
  summarise(top3 = list(labels), .groups = "drop")

model_pairs <- combn(sort(unique(top3_labels$label_model)), 2, simplify = FALSE)
pairwise_overlap <- map_dfr(model_pairs, function(pair) {
  left <- top3_labels %>% filter(label_model == pair[[1]])
  right <- top3_labels %>% filter(label_model == pair[[2]])
  inner_join(left, right, by = "moral_id", suffix = c("_a", "_b")) %>%
    transmute(
      moral_id,
      model_a = pair[[1]],
      model_b = pair[[2]],
      model_pair = paste(pair, collapse = " vs "),
      overlap = map2_int(top3_a, top3_b, ~ length(intersect(.x, .y)))
    )
})

overlap_summary <- pairwise_overlap %>%
  group_by(model_a, model_b, model_pair) %>%
  summarise(
    n_morals = n(),
    mean_overlap = mean(overlap),
    proportion_overlap_3 = mean(overlap == 3),
    proportion_overlap_2 = mean(overlap == 2),
    proportion_overlap_1 = mean(overlap == 1),
    proportion_overlap_0 = mean(overlap == 0),
    .groups = "drop"
  )
write_csv(
  overlap_summary,
  file.path(RESULTS_DIR, "section_4_2_between_model_overlap.csv")
)

###############################################################################
# SECTION 4.2D: CORPUS- AND STATEMENT-LEVEL RANK CONVERGENCE
###############################################################################

compute_distribution <- function(data) {
  data %>%
    group_by(labels) %>%
    summarise(proportion = sum(weight), .groups = "drop") %>%
    mutate(proportion = proportion / sum(proportion)) %>%
    arrange(desc(proportion), labels)
}

lookup_proportions <- function(distribution, labels) {
  values <- distribution$proportion[match(labels, distribution$labels)]
  replace(values, is.na(values), 0)
}

corpus_reference <- compute_distribution(valid_labels)
reference_top20 <- corpus_reference %>% slice_head(n = 20)

corpus_rank_stability <- map_dfr(1:K_MAX, function(k) {
  distribution_k <- valid_labels %>%
    filter(run <= k) %>%
    compute_distribution()
  top20_k <- distribution_k %>% slice_head(n = 20) %>% pull(labels)
  tibble(
    k = k,
    spearman = cor(
      reference_top20$proportion,
      lookup_proportions(distribution_k, reference_top20$labels),
      method = "spearman"
    ),
    top20_shared = length(intersect(reference_top20$labels, top20_k))
  )
})
write_csv(
  corpus_rank_stability,
  file.path(RESULTS_DIR, "section_4_2_corpus_rank_stability.csv")
)

corpus_top20_by_k <- map_dfr(1:K_MAX, function(k) {
  valid_labels %>%
    filter(run <= k) %>%
    compute_distribution() %>%
    slice_head(n = 20) %>%
    mutate(k = k, rank = row_number(), .before = 1)
})
write_csv(
  corpus_top20_by_k,
  file.path(RESULTS_DIR, "section_4_2_corpus_top20_by_k.csv")
)

statement_rank_stability <- map_dfr(sort(unique(valid_labels$moral_id)), function(mid) {
  moral_data <- valid_labels %>% filter(moral_id == mid)
  reference <- compute_distribution(moral_data)
  map_dfr(1:K_MAX, function(k) {
    distribution_k <- moral_data %>%
      filter(run <= k) %>%
      compute_distribution()
    tibble(
      moral_id = mid,
      k = k,
      spearman = cor(
        reference$proportion,
        lookup_proportions(distribution_k, reference$labels),
        method = "spearman"
      )
    )
  })
})

statement_rank_summary <- statement_rank_stability %>%
  group_by(k) %>%
  summarise(
    n_morals = sum(!is.na(spearman)),
    mean_spearman = mean(spearman, na.rm = TRUE),
    p05_spearman = quantile(spearman, 0.05, na.rm = TRUE),
    p95_spearman = quantile(spearman, 0.95, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(
  statement_rank_summary,
  file.path(RESULTS_DIR, "section_4_2_statement_rank_stability.csv")
)

###############################################################################
# SECTION 4.2E: LABEL DISCOVERY AND COVERAGE
###############################################################################

# The unit here is a moral-label association, pooled across labeling models.
# A label is present in a run if at least one labeling model assigned that label
# to the moral in that indexed run.
run_labels <- valid_labels %>%
  distinct(moral_id, run, labels)

label_life <- run_labels %>%
  group_by(moral_id, labels) %>%
  summarise(
    first_run = min(run),
    frequency_k10 = n_distinct(run) / K_MAX,
    .groups = "drop"
  )

discovery <- label_life %>%
  group_by(k_discovered = first_run) %>%
  summarise(
    n_moral_label_associations = n(),
    mean_frequency_k10 = mean(frequency_k10),
    p25_frequency = quantile(frequency_k10, 0.25),
    p75_frequency = quantile(frequency_k10, 0.75),
    max_frequency = max(frequency_k10),
    proportion_majority = mean(frequency_k10 > 0.5),
    .groups = "drop"
  )
write_csv(discovery, file.path(RESULTS_DIR, "section_4_2_label_discovery.csv"))

moral_totals <- label_life %>% count(moral_id, name = "n_total")
coverage_by_moral <- crossing(moral_id = moral_totals$moral_id, k = 1:K_MAX) %>%
  left_join(label_life, by = "moral_id", relationship = "many-to-many") %>%
  group_by(moral_id, k) %>%
  summarise(n_seen = n_distinct(labels[first_run <= k]), .groups = "drop") %>%
  left_join(moral_totals, by = "moral_id") %>%
  mutate(coverage = n_seen / n_total)

total_associations <- nrow(label_life)
total_observed_labels <- n_distinct(label_life$labels)
coverage_summary <- map_dfr(1:K_MAX, function(k) {
  moral_k <- coverage_by_moral %>% filter(.data$k == .env$k)
  tibble(
    k = k,
    mean_moral_coverage = mean(moral_k$coverage),
    p05_moral_coverage = quantile(moral_k$coverage, 0.05),
    moral_label_association_coverage = mean(label_life$first_run <= k),
    corpus_distinct_label_coverage =
      n_distinct(label_life$labels[label_life$first_run <= k]) / total_observed_labels,
    n_moral_label_associations = total_associations,
    n_distinct_labels_observed = total_observed_labels
  )
})
write_csv(coverage_summary, file.path(RESULTS_DIR, "section_4_2_label_coverage.csv"))

###############################################################################
# SECTION 4.2 FIGURE: FREQUENCY OF NEW MORAL-LABEL ASSOCIATIONS
###############################################################################

figure_3 <- ggplot(discovery, aes(k_discovered, mean_frequency_k10)) +
  geom_hline(
    yintercept = 0.5, linetype = "dashed", color = "grey45", linewidth = 0.5
  ) +
  geom_ribbon(
    aes(ymin = p25_frequency, ymax = p75_frequency),
    fill = "coral", alpha = 0.20
  ) +
  geom_line(color = "coral", linewidth = 0.8) +
  geom_point(color = "coral", size = 2.5) +
  geom_text(
    aes(label = percent(mean_frequency_k10, accuracy = 0.1)),
    vjust = -0.7, size = 3, color = "coral"
  ) +
  annotate(
    "text", x = 8.7, y = 0.52, label = "50% of runs",
    hjust = 0, size = 3, color = "grey40"
  ) +
  scale_x_continuous(breaks = 1:K_MAX) +
  scale_y_continuous(
    limits = c(0, 1.05), breaks = seq(0, 1, 0.2), labels = percent
  ) +
  labs(
    x = "First observed run (k)",
    y = "Mean frequency across 10 runs"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(RESULTS_DIR, "section_4_2_figure_label_discovery.pdf"),
  figure_3, width = 6.5, height = 4.5, units = "in"
)
ggsave(
  file.path(FIGURES_DIR, "figure_3_label_consistency.pdf"),
  figure_3, width = 6.5, height = 4.5, units = "in"
)

writeLines(
  capture.output(sessionInfo()),
  file.path(RESULTS_DIR, "section_4_2_session_info.txt")
)

cat("Section 4.2 complete.\n")
cat("Sampled morals:", n_distinct(raw_labels$moral_id), "\n")
cat("Recorded attempts:", nrow(attempt_inventory), "of 7500 intended\n")
cat("Off-taxonomy emissions excluded:", nrow(off_taxonomy), "\n")
cat("Run-1 mean moral-label coverage:",
    sprintf("%.4f", coverage_summary$mean_moral_coverage[[1]]), "\n")
