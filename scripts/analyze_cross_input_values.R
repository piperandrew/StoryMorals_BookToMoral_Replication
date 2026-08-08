#!/usr/bin/env Rscript

###############################################################################
# PAPER SECTION 4.3: INPUT SENSITIVITY OF VALUE-LABEL DISTRIBUTIONS
#
# Each condition contains one independently sampled moral per book. Because the
# generating model assigned to a book is not generally the same across input
# conditions, every distribution is standardized equally over the 3 generating
# models x 3 labeling models before distances are calculated.
#
# Core label: appears in >50% of all recorded attempts for a
# moral x labeling-model combination. Recorded attempts that emit only invalid
# labels remain in the denominator.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
})

DATA_DIR <- file.path("data", "derived")
TAXONOMY_FILE <- file.path("data", "reference", "value_taxonomy.csv")
RESULTS_DIR <- "results"
N_PERM <- as.integer(Sys.getenv("N_PERM", "1000"))
N_FLOOR <- as.integer(Sys.getenv("N_FLOOR", "1000"))
CORE_THRESHOLD <- 0.5
K_MAX <- 10L

dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

CONDITION_FILES <- c(
  full_text = file.path(DATA_DIR, "value_labels_full_text.csv"),
  chunk = file.path(DATA_DIR, "value_labels_chunk_summary.csv"),
  plotArc = file.path(DATA_DIR, "value_labels_plot_arc_summary.csv")
)
EXPECTED_ROWS <- c(full_text = 34915L, chunk = 35346L, plotArc = 31814L)
EXPECTED_ATTEMPTS <- c(full_text = 7496L, chunk = 7500L, plotArc = 7500L)
EXPECTED_OFF_TAXONOMY <- c(full_text = 308L, chunk = 244L, plotArc = 242L)
EXPECTED_ZERO_VALID <- c(full_text = 7L, chunk = 1L, plotArc = 7L)
EXPECTED_MODELS <- c("Claude", "Gemini", "GPT")
EXPECTED_LABELERS <- c("Claude", "Gemini", "GPT")

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
# SECTION 4.3A: LOAD, VALIDATE, AND CONSTRUCT CORE LABEL SETS
###############################################################################

taxonomy <- read_csv(TAXONOMY_FILE, show_col_types = FALSE) %>%
  transmute(labels = trimws(rokeach_value)) %>%
  distinct(labels) %>%
  pull(labels)
assert_replication_value(length(taxonomy), 62, "taxonomy labels")

load_condition <- function(condition) {
  data <- read_csv(CONDITION_FILES[[condition]], show_col_types = FALSE) %>%
    mutate(
      condition = condition,
      labels = trimws(labels),
      attempt_id = paste(moral_id, label_model, run, order_seed, sep = "__")
    )

  required <- c(
    "moral_id", "book_id", "Class", "model", "moral_num", "moral",
    "label_model", "run", "order_seed", "labels", "attempt_id"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing: %s", condition, paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  attempts <- data %>%
    distinct(
      condition, moral_id, book_id, Class, model, moral_num,
      label_model, run, order_seed, attempt_id
    )
  valid <- data %>%
    filter(labels %in% taxonomy) %>%
    distinct(attempt_id, labels, .keep_all = TRUE)
  off_taxonomy <- data %>% filter(!labels %in% taxonomy)
  zero_valid <- setdiff(attempts$attempt_id, valid$attempt_id)
  samples <- data %>%
    distinct(condition, moral_id, book_id, Class, model, moral_num, moral)

  assert_replication_value(nrow(data), EXPECTED_ROWS[[condition]],
                           paste(condition, "raw emissions"))
  assert_replication_value(nrow(attempts), EXPECTED_ATTEMPTS[[condition]],
                           paste(condition, "recorded attempts"))
  assert_replication_value(nrow(off_taxonomy), EXPECTED_OFF_TAXONOMY[[condition]],
                           paste(condition, "off-taxonomy emissions"))
  assert_replication_value(length(zero_valid), EXPECTED_ZERO_VALID[[condition]],
                           paste(condition, "attempts with no valid label"))
  assert_replication_value(n_distinct(samples$book_id), 250,
                           paste(condition, "books"))
  assert_replication_value(nrow(samples), 250,
                           paste(condition, "sampled morals"))

  list(
    raw = data,
    attempts = attempts,
    valid = valid,
    off_taxonomy = off_taxonomy,
    zero_valid = zero_valid,
    samples = samples
  )
}

conditions <- set_names(map(names(CONDITION_FILES), load_condition),
                        names(CONDITION_FILES))

to_core <- function(condition_data) {
  denominators <- condition_data$attempts %>%
    count(
      condition, moral_id, book_id, Class, model, moral_num, label_model,
      name = "n_attempts"
    )

  condition_data$valid %>%
    distinct(
      condition, moral_id, book_id, Class, model, moral_num,
      label_model, attempt_id, labels
    ) %>%
    count(
      condition, moral_id, book_id, Class, model, moral_num,
      label_model, labels,
      name = "n_hits"
    ) %>%
    left_join(
      denominators,
      by = c(
        "condition", "moral_id", "book_id", "Class", "model",
        "moral_num", "label_model"
      )
    ) %>%
    filter(n_hits > CORE_THRESHOLD * n_attempts) %>%
    mutate(label_index = match(labels, taxonomy))
}

core <- map(conditions, to_core)

analysis_inventory <- map_dfr(names(conditions), function(condition) {
  x <- conditions[[condition]]
  tibble(
    condition,
    books = n_distinct(x$samples$book_id),
    sampled_morals = nrow(x$samples),
    raw_emissions = nrow(x$raw),
    recorded_attempts = nrow(x$attempts),
    valid_attempts = n_distinct(x$valid$attempt_id),
    off_taxonomy_emissions = nrow(x$off_taxonomy),
    attempts_with_no_valid_label = length(x$zero_valid),
    core_rows = nrow(core[[condition]])
  )
})
write_csv(
  analysis_inventory,
  file.path(RESULTS_DIR, "section_4_3_analysis_inventory.csv")
)

# Audit the independent moral/model sampling across input conditions.
sample_pairing <- combn(names(conditions), 2, simplify = FALSE) %>%
  map_dfr(function(pair) {
    left <- conditions[[pair[[1]]]]$samples
    right <- conditions[[pair[[2]]]]$samples
    joined <- inner_join(
      left, right, by = c("book_id", "Class"), suffix = c("_a", "_b")
    )
    tibble(
      condition_a = pair[[1]],
      condition_b = pair[[2]],
      n_books = nrow(joined),
      n_same_generating_model = sum(joined$model_a == joined$model_b),
      proportion_same_generating_model = mean(joined$model_a == joined$model_b),
      n_same_moral_number = sum(joined$moral_num_a == joined$moral_num_b),
      proportion_same_moral_number = mean(joined$moral_num_a == joined$moral_num_b),
      n_same_moral_text = sum(joined$moral_a == joined$moral_b)
    )
  })
write_csv(sample_pairing, file.path(RESULTS_DIR, "section_4_3_sample_pairing.csv"))

all_samples <- conditions$full_text$samples %>%
  select(book_id, full_model = model) %>%
  inner_join(
    conditions$chunk$samples %>% select(book_id, chunk_model = model),
    by = "book_id"
  ) %>%
  inner_join(
    conditions$plotArc$samples %>% select(book_id, plot_model = model),
    by = "book_id"
  )
assert_replication_value(
  sum(all_samples$full_model == all_samples$chunk_model &
        all_samples$full_model == all_samples$plot_model),
  32,
  "books with the same generating model in all three conditions"
)

###############################################################################
# SECTION 4.3B: DISTRIBUTIONS, DISTANCES, AND MODEL STANDARDIZATION
###############################################################################

standardized_distribution <- function(data) {
  model_factor <- factor(data$model, levels = EXPECTED_MODELS)
  labeler_factor <- factor(data$label_model, levels = EXPECTED_LABELERS)
  cell <- interaction(model_factor, labeler_factor, drop = FALSE)
  cell_levels <- levels(cell)

  by_cell <- vapply(cell_levels, function(cell_name) {
    counts <- tabulate(data$label_index[cell == cell_name], nbins = length(taxonomy))
    if (sum(counts) == 0) {
      stop(sprintf("Empty model-by-labeler cell: %s", cell_name), call. = FALSE)
    }
    counts / sum(counts)
  }, numeric(length(taxonomy)))

  setNames(rowMeans(by_cell), taxonomy)
}

tvd <- function(p, q) 0.5 * sum(abs(p - q))

jsd <- function(p, q) {
  midpoint <- 0.5 * (p + q)
  kl <- function(a, b) sum(ifelse(a > 0, a * log2(a / b), 0))
  0.5 * kl(p, midpoint) + 0.5 * kl(q, midpoint)
}

cohen_w_from_jsd <- function(jsd_base2) {
  sqrt(2 * jsd_base2 * log(2))
}

paired_permutation <- function(reference, summary, summary_name, seed, n_perm) {
  books <- intersect(unique(reference$book_id), unique(summary$book_id))
  both <- bind_rows(reference, summary)
  p_reference <- standardized_distribution(reference)
  p_summary <- standardized_distribution(summary)
  observed_tvd <- tvd(p_reference, p_summary)

  set.seed(seed)
  null <- replicate(n_perm, {
    flipped_books <- books[runif(length(books)) < 0.5]
    permuted <- both
    swap <- permuted$book_id %in% flipped_books
    permuted$condition[swap] <- ifelse(
      permuted$condition[swap] == "full_text", summary_name, "full_text"
    )
    tvd(
      standardized_distribution(filter(permuted, condition == "full_text")),
      standardized_distribution(filter(permuted, condition == summary_name))
    )
  })

  list(
    n_books = length(books),
    reference_distribution = p_reference,
    summary_distribution = p_summary,
    observed_tvd = observed_tvd,
    null = null,
    p_value = (1 + sum(null >= observed_tvd)) / (n_perm + 1)
  )
}

###############################################################################
# SECTION 4.3C: WITHIN-FULL-TEXT HALF-RUN BENCHMARK
###############################################################################

# Efficient matrix representation of full-text runs. Every split assigns exactly
# five indexed runs per book to each half, then reconstructs majority core sets.
full_raw <- conditions$full_text$raw
full_valid <- conditions$full_text$valid

book_levels <- sort(unique(full_raw$book_id))
attempt_cells <- full_raw %>%
  distinct(book_id, model, label_model, run) %>%
  mutate(present = 1L) %>%
  complete(
    nesting(book_id, model, label_model),
    run = 1:K_MAX,
    fill = list(present = 0L)
  ) %>%
  arrange(book_id, model, label_model, run)

attempt_meta <- attempt_cells %>%
  distinct(book_id, model, label_model) %>%
  arrange(book_id, model, label_model) %>%
  mutate(
    attempt_cell_id = row_number(),
    book_index = match(book_id, book_levels)
  )
attempt_matrix <- attempt_cells %>%
  select(book_id, model, label_model, run, present) %>%
  pivot_wider(names_from = run, values_from = present) %>%
  arrange(book_id, model, label_model) %>%
  select(all_of(as.character(1:K_MAX))) %>%
  as.matrix()

label_cells <- full_valid %>%
  distinct(book_id, model, label_model, labels, run) %>%
  mutate(present = 1L) %>%
  complete(
    nesting(book_id, model, label_model, labels),
    run = 1:K_MAX,
    fill = list(present = 0L)
  ) %>%
  arrange(book_id, model, label_model, labels, run)

label_meta <- label_cells %>%
  distinct(book_id, model, label_model, labels) %>%
  arrange(book_id, model, label_model, labels) %>%
  mutate(
    book_index = match(book_id, book_levels),
    label_index = match(labels, taxonomy),
    model_index = match(model, EXPECTED_MODELS),
    labeler_index = match(label_model, EXPECTED_LABELERS),
    attempt_cell_id = match(
      paste(book_id, model, label_model, sep = "__"),
      paste(attempt_meta$book_id, attempt_meta$model,
            attempt_meta$label_model, sep = "__")
    )
  )
label_matrix <- label_cells %>%
  select(book_id, model, label_model, labels, run, present) %>%
  pivot_wider(names_from = run, values_from = present) %>%
  arrange(book_id, model, label_model, labels) %>%
  select(all_of(as.character(1:K_MAX))) %>%
  as.matrix()

distribution_from_core_flags <- function(core_flags) {
  cell_index <- (label_meta$model_index - 1L) * length(EXPECTED_LABELERS) +
    label_meta$labeler_index
  distributions <- vapply(1:9, function(cell_id) {
    counts <- tabulate(
      label_meta$label_index[core_flags & cell_index == cell_id],
      nbins = length(taxonomy)
    )
    if (sum(counts) == 0) {
      stop(sprintf("Empty half-run model-by-labeler cell: %s", cell_id),
           call. = FALSE)
    }
    counts / sum(counts)
  }, numeric(length(taxonomy)))
  setNames(rowMeans(distributions), taxonomy)
}

set.seed(4310)
self_noise_values <- replicate(N_FLOOR, {
  half_a <- t(replicate(length(book_levels), {
    selected <- sample.int(K_MAX, K_MAX / 2)
    seq_len(K_MAX) %in% selected
  }))

  attempt_a <- rowSums(
    attempt_matrix * half_a[attempt_meta$book_index, , drop = FALSE]
  )
  attempt_b <- rowSums(attempt_matrix) - attempt_a
  hits_a <- rowSums(label_matrix * half_a[label_meta$book_index, , drop = FALSE])
  hits_b <- rowSums(label_matrix) - hits_a

  core_a <- hits_a > CORE_THRESHOLD * attempt_a[label_meta$attempt_cell_id]
  core_b <- hits_b > CORE_THRESHOLD * attempt_b[label_meta$attempt_cell_id]

  tvd(
    distribution_from_core_flags(core_a),
    distribution_from_core_flags(core_b)
  )
})

self_noise_summary <- tibble(
  n_splits = N_FLOOR,
  mean_tvd = mean(self_noise_values),
  median_tvd = median(self_noise_values),
  lower_95 = quantile(self_noise_values, 0.025),
  upper_95 = quantile(self_noise_values, 0.975)
)
write_csv(
  self_noise_summary,
  file.path(RESULTS_DIR, "section_4_3_within_full_text_benchmark.csv")
)

###############################################################################
# SECTION 4.3D: GLOBAL INPUT-CONDITION FIDELITY
###############################################################################

global_seeds <- c(chunk = 4321L, plotArc = 4322L)
global_tests <- map(c("chunk", "plotArc"), function(summary_name) {
  paired_permutation(
    core$full_text,
    core[[summary_name]],
    summary_name,
    global_seeds[[summary_name]],
    N_PERM
  )
}) %>% set_names(c("chunk", "plotArc"))

global_fidelity <- map_dfr(names(global_tests), function(summary_name) {
  test <- global_tests[[summary_name]]
  jsd_value <- jsd(test$reference_distribution, test$summary_distribution)
  tibble(
    summary = summary_name,
    n_books = test$n_books,
    TVD = test$observed_tvd,
    JSD = jsd_value,
    cohen_w = cohen_w_from_jsd(jsd_value),
    within_full_text_benchmark = self_noise_summary$mean_tvd,
    permutation_null_mean = mean(test$null),
    p_raw = test$p_value
  )
}) %>%
  mutate(p_holm = p.adjust(p_raw, method = "holm"))
write_csv(
  global_fidelity,
  file.path(RESULTS_DIR, "section_4_3_global_fidelity.csv")
)

# The pooled mixture is the arithmetic mean of the two standardized summary
# distributions. It is descriptive and receives no paired permutation p-value.
p_full <- standardized_distribution(core$full_text)
p_chunk <- standardized_distribution(core$chunk)
p_plot <- standardized_distribution(core$plotArc)
p_pooled <- 0.5 * (p_chunk + p_plot)
pooled_jsd <- jsd(p_full, p_pooled)
pooled_fidelity <- tibble(
  condition = "pooled_chunk_plot",
  construction = "arithmetic mean of standardized chunk and plot distributions",
  TVD = tvd(p_full, p_pooled),
  JSD = pooled_jsd,
  cohen_w = cohen_w_from_jsd(pooled_jsd),
  within_full_text_benchmark = self_noise_summary$mean_tvd,
  inferential_test = "none; constructed descriptive mixture"
)
write_csv(
  pooled_fidelity,
  file.path(RESULTS_DIR, "section_4_3_pooled_fidelity.csv")
)

###############################################################################
# SECTION 4.3E: FIDELITY BY BOOK CLASS
###############################################################################

classes <- sort(unique(core$full_text$Class))
class_seed <- function(summary_name, class_name) {
  summary_offset <- ifelse(summary_name == "chunk", 0L, 100L)
  4400L + summary_offset + match(class_name, classes)
}

class_fidelity <- map_dfr(c("chunk", "plotArc"), function(summary_name) {
  map_dfr(classes, function(class_name) {
    test <- paired_permutation(
      filter(core$full_text, Class == class_name),
      filter(core[[summary_name]], Class == class_name),
      summary_name,
      class_seed(summary_name, class_name),
      N_PERM
    )
    tibble(
      summary = summary_name,
      Class = class_name,
      n_books = test$n_books,
      TVD = test$observed_tvd,
      permutation_null_mean = mean(test$null),
      p_raw = test$p_value
    )
  })
}) %>%
  mutate(p_holm = p.adjust(p_raw, method = "holm"))
write_csv(
  class_fidelity,
  file.path(RESULTS_DIR, "section_4_3_fidelity_by_class.csv")
)

###############################################################################
# SECTION 4.3F: BETWEEN-CLASS STRUCTURE PRESERVATION
###############################################################################

class_pairs <- combn(classes, 2, simplify = FALSE)
class_distribution <- function(condition_name, class_name) {
  standardized_distribution(filter(core[[condition_name]], Class == class_name))
}

structure_pair_distances <- map_dfr(class_pairs, function(pair) {
  full_distance <- tvd(
    class_distribution("full_text", pair[[1]]),
    class_distribution("full_text", pair[[2]])
  )
  chunk_distance <- tvd(
    class_distribution("chunk", pair[[1]]),
    class_distribution("chunk", pair[[2]])
  )
  plot_distance <- tvd(
    class_distribution("plotArc", pair[[1]]),
    class_distribution("plotArc", pair[[2]])
  )
  pooled_distance <- tvd(
    0.5 * (class_distribution("chunk", pair[[1]]) +
             class_distribution("plotArc", pair[[1]])),
    0.5 * (class_distribution("chunk", pair[[2]]) +
             class_distribution("plotArc", pair[[2]]))
  )
  tibble(
    class_a = pair[[1]],
    class_b = pair[[2]],
    full_text = full_distance,
    chunk = chunk_distance,
    plotArc = plot_distance,
    pooled_chunk_plot = pooled_distance
  )
})
write_csv(
  structure_pair_distances,
  file.path(RESULTS_DIR, "section_4_3_class_pair_distances.csv")
)

structure_preservation <- c("chunk", "plotArc", "pooled_chunk_plot") %>%
  map_dfr(function(condition_name) {
    tibble(
      condition = condition_name,
      n_class_pairs = nrow(structure_pair_distances),
      pearson = cor(
        structure_pair_distances$full_text,
        structure_pair_distances[[condition_name]]
      ),
      spearman = cor(
        structure_pair_distances$full_text,
        structure_pair_distances[[condition_name]],
        method = "spearman"
      )
    )
  })
write_csv(
  structure_preservation,
  file.path(RESULTS_DIR, "section_4_3_class_structure.csv")
)

###############################################################################
# SECTION 4.3G: VALUE PREVALENCE AND RANK TABLE
###############################################################################

standardized_prevalence <- function(condition_name) {
  condition_core <- core[[condition_name]]
  instances <- conditions[[condition_name]]$attempts %>%
    distinct(moral_id, book_id, model, label_model)

  map_dfr(EXPECTED_MODELS, function(generating_model) {
    map_dfr(EXPECTED_LABELERS, function(labeling_model) {
      cell_instances <- instances %>%
        filter(model == generating_model, label_model == labeling_model)
      denominator <- nrow(cell_instances)
      hits <- condition_core %>%
        filter(model == generating_model, label_model == labeling_model) %>%
        count(labels, name = "n_hits") %>%
        complete(labels = taxonomy, fill = list(n_hits = 0L))
      hits %>%
        transmute(
          labels,
          generating_model,
          labeling_model,
          prevalence = n_hits / denominator
        )
    })
  }) %>%
    group_by(labels) %>%
    summarise(prevalence = mean(prevalence), .groups = "drop")
}

prevalence_full <- standardized_prevalence("full_text") %>%
  rename(prevalence_full = prevalence)
prevalence_chunk <- standardized_prevalence("chunk") %>%
  rename(prevalence_chunk = prevalence)
prevalence_plot <- standardized_prevalence("plotArc") %>%
  rename(prevalence_plotArc = prevalence)

value_rank_changes <- prevalence_full %>%
  left_join(prevalence_chunk, by = "labels") %>%
  left_join(prevalence_plot, by = "labels") %>%
  mutate(
    prevalence_pooled = 0.5 * (prevalence_chunk + prevalence_plotArc),
    rank_full = rank(-prevalence_full, ties.method = "min"),
    rank_pooled = rank(-prevalence_pooled, ties.method = "min"),
    rank_change = rank_full - rank_pooled,
    delta_percentage_points = 100 * (prevalence_pooled - prevalence_full)
  ) %>%
  arrange(rank_full, labels)
write_csv(
  value_rank_changes,
  file.path(RESULTS_DIR, "section_4_3_value_rank_changes.csv")
)
write_csv(
  slice_head(value_rank_changes, n = 10),
  file.path(RESULTS_DIR, "section_4_3_table_top10_values.csv")
)

###############################################################################
# SECTION 4.3H: SAME-GENERATING-MODEL SENSITIVITY ANALYSIS
###############################################################################

matched_model_results <- map_dfr(c("chunk", "plotArc"), function(summary_name) {
  matched_books <- inner_join(
    conditions$full_text$samples %>% select(book_id, full_model = model),
    conditions[[summary_name]]$samples %>% select(book_id, summary_model = model),
    by = "book_id"
  ) %>%
    filter(full_model == summary_model) %>%
    pull(book_id)

  test <- paired_permutation(
    filter(core$full_text, book_id %in% matched_books),
    filter(core[[summary_name]], book_id %in% matched_books),
    summary_name,
    ifelse(summary_name == "chunk", 4511L, 4512L),
    N_PERM
  )
  jsd_value <- jsd(test$reference_distribution, test$summary_distribution)
  tibble(
    summary = summary_name,
    n_books = test$n_books,
    TVD = test$observed_tvd,
    JSD = jsd_value,
    cohen_w = cohen_w_from_jsd(jsd_value),
    permutation_null_mean = mean(test$null),
    p_raw = test$p_value
  )
}) %>%
  mutate(p_holm = p.adjust(p_raw, method = "holm"))
write_csv(
  matched_model_results,
  file.path(RESULTS_DIR, "section_4_3_matched_model_sensitivity.csv")
)

writeLines(
  capture.output(sessionInfo()),
  file.path(RESULTS_DIR, "section_4_3_session_info.txt")
)

cat("Section 4.3 complete.\n")
cat("Within-full-text half-run benchmark:",
    sprintf("%.4f", self_noise_summary$mean_tvd), "\n")
cat("Global summary TVDs:",
    paste(global_fidelity$summary, sprintf("%.4f", global_fidelity$TVD),
          collapse = "; "), "\n")
cat("Pooled descriptive TVD:", sprintf("%.4f", pooled_fidelity$TVD), "\n")
