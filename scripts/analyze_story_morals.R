#!/usr/bin/env Rscript

###############################################################################
# PAPER SECTION 4.1: INPUT SENSITIVITY ON STORY MORAL GENERATION
#
# Unit of analysis for input comparisons:
#   book x generating model x input pair
#
# Each input condition produces three deliberately different morals in one
# response. The full-text within-response benchmark measures this requested
# diversity, not instability across independent model calls.
# We score all 3 x 3 = 9
# cross-condition moral pairs and average those nine cosine similarities into
# one set-level observation. No semantic matching or pair selection is used.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(emmeans)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
  library(readr)
  library(tidyr)
})

# Resolve paths from either the project root or its scripts directory, including
# when individual lines are run interactively. Do not change the working directory.
PROJECT_ROOT <- if (dir.exists(file.path("data", "derived"))) {
  normalizePath(".")
} else if (dir.exists(file.path("..", "data", "derived"))) {
  normalizePath("..")
} else {
  stop("Run this script from the project root or its scripts directory.",
       call. = FALSE)
}

DATA_DIR <- file.path(PROJECT_ROOT, "data", "derived")
RESULTS_DIR <- file.path(PROJECT_ROOT, "results")
FIGURES_DIR <- file.path(PROJECT_ROOT, "figures")
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

INPUT_PAIR_FILE <- file.path(DATA_DIR, "section_4_1_cross_input_pairs_all.csv")
MODEL_PAIR_FILE <- file.path(DATA_DIR, "section_4_1_cross_model_pairs_all.csv")
WITHIN_FULL_FILE <- file.path(DATA_DIR, "section_4_1_within_full_text_pairs.csv")

input_pairs <- read_csv(INPUT_PAIR_FILE, show_col_types = FALSE)
model_pairs <- read_csv(MODEL_PAIR_FILE, show_col_types = FALSE)
within_full_pairs <- read_csv(WITHIN_FULL_FILE, show_col_types = FALSE)

assert_replication_value <- function(observed, expected, label) {
  if (!identical(as.integer(observed), as.integer(expected))) {
    stop(
      sprintf("Replication check failed for %s: expected %s, found %s",
              label, expected, observed),
      call. = FALSE
    )
  }
}

# These checks ensure that the released pair tables reproduce the exact design
# analyzed in the manuscript subsection.
assert_replication_value(nrow(input_pairs), 37665, "cross-input moral pairs")
assert_replication_value(nrow(model_pairs), 25110, "cross-model moral pairs")
assert_replication_value(nrow(within_full_pairs), 2250, "within-full-text moral pairs")

pair_levels <- c(
  "chunk_summary__full_text",
  "full_text__plotArc_summary",
  "full_text__memory",
  "chunk_summary__plotArc_summary",
  "chunk_summary__memory",
  "memory__plotArc_summary"
)

###############################################################################
# SECTION 4.1A: CONSTRUCT SET-LEVEL SIMILARITY
###############################################################################

input_set_similarity <- input_pairs %>%
  group_by(
    book_id, Class, generating_model,
    input_type_a, input_type_b, input_pair
  ) %>%
  summarise(
    mean_similarity = mean(similarity),
    sd_pair_similarity = sd(similarity),
    n_moral_pairs = n(),
    .groups = "drop"
  ) %>%
  filter(n_moral_pairs == 9) %>%
  mutate(input_pair = factor(input_pair, levels = pair_levels))

model_set_similarity <- model_pairs %>%
  group_by(book_id, Class, input_type, model_a, model_b, model_pair) %>%
  summarise(
    mean_similarity = mean(similarity),
    sd_pair_similarity = sd(similarity),
    n_moral_pairs = n(),
    .groups = "drop"
  ) %>%
  filter(n_moral_pairs == 9)

within_full_set_similarity <- within_full_pairs %>%
  group_by(book_id, Class, generating_model) %>%
  summarise(
    mean_similarity = mean(similarity),
    n_moral_pairs = n(),
    .groups = "drop"
  ) %>%
  filter(n_moral_pairs == 3)

assert_replication_value(nrow(input_set_similarity), 4185, "cross-input moral sets")
assert_replication_value(nrow(model_set_similarity), 2790, "cross-model moral sets")
assert_replication_value(nrow(within_full_set_similarity), 750, "within-full-text moral sets")

write_csv(
  input_set_similarity,
  file.path(RESULTS_DIR, "section_4_1_cross_input_set_similarity.csv")
)
write_csv(
  model_set_similarity,
  file.path(RESULTS_DIR, "section_4_1_cross_model_set_similarity.csv")
)
write_csv(
  within_full_set_similarity,
  file.path(RESULTS_DIR, "section_4_1_within_full_text_set_similarity.csv")
)

section_inventory <- tibble(
  analysis_table = c(
    "cross_input_pair_level",
    "cross_input_set_level",
    "cross_model_pair_level",
    "cross_model_set_level",
    "within_full_text_pair_level",
    "within_full_text_set_level"
  ),
  rows = c(
    nrow(input_pairs),
    nrow(input_set_similarity),
    nrow(model_pairs),
    nrow(model_set_similarity),
    nrow(within_full_pairs),
    nrow(within_full_set_similarity)
  ),
  unique_books = c(
    n_distinct(input_pairs$book_id),
    n_distinct(input_set_similarity$book_id),
    n_distinct(model_pairs$book_id),
    n_distinct(model_set_similarity$book_id),
    n_distinct(within_full_pairs$book_id),
    n_distinct(within_full_set_similarity$book_id)
  )
)
write_csv(section_inventory, file.path(RESULTS_DIR, "section_4_1_analysis_inventory.csv"))

input_coverage <- input_set_similarity %>%
  count(Class, generating_model, input_pair, name = "n_books")
model_coverage <- model_set_similarity %>%
  count(Class, input_type, model_pair, name = "n_books")
write_csv(
  input_coverage,
  file.path(RESULTS_DIR, "section_4_1_cross_input_coverage.csv")
)
write_csv(
  model_coverage,
  file.path(RESULTS_DIR, "section_4_1_cross_model_coverage.csv")
)

###############################################################################
# SECTION 4.1B: WHICH INPUT REPRESENTATION IS CLOSEST TO FULL TEXT?
#
# Paper model:
# mean_similarity ~ input_pair + generating_model + Class + (1 | book_id)
###############################################################################

input_model <- lmer(
  mean_similarity ~ input_pair + generating_model + Class + (1 | book_id),
  data = input_set_similarity,
  REML = TRUE
)

input_pair_emmeans <- as.data.frame(emmeans(input_model, ~ input_pair))
input_pair_contrasts <- as.data.frame(pairs(emmeans(input_model, ~ input_pair)))

write_csv(
  input_pair_emmeans,
  file.path(RESULTS_DIR, "section_4_1_input_pair_emmeans.csv")
)
write_csv(
  input_pair_contrasts,
  file.path(RESULTS_DIR, "section_4_1_input_pair_contrasts.csv")
)

input_model_coefficients <- as.data.frame(coef(summary(input_model)))
input_model_coefficients$term <- rownames(input_model_coefficients)
rownames(input_model_coefficients) <- NULL
write_csv(
  input_model_coefficients,
  file.path(RESULTS_DIR, "section_4_1_input_model_coefficients.csv")
)

###############################################################################
# SECTION 4.1C: DOES INPUT SENSITIVITY VARY BY BOOK CLASS?
#
# Omnibus likelihood-ratio test of the input_pair x Class interaction.
###############################################################################

input_model_no_interaction <- lmer(
  mean_similarity ~ input_pair + generating_model + Class + (1 | book_id),
  data = input_set_similarity,
  REML = FALSE
)
input_model_by_class <- lmer(
  mean_similarity ~ input_pair * Class + generating_model + (1 | book_id),
  data = input_set_similarity,
  REML = FALSE
)

interaction_lrt <- as.data.frame(anova(input_model_no_interaction, input_model_by_class))
interaction_lrt$model <- rownames(interaction_lrt)
rownames(interaction_lrt) <- NULL
write_csv(
  interaction_lrt,
  file.path(RESULTS_DIR, "section_4_1_input_pair_by_class_lrt.csv")
)

input_pair_by_class_emmeans <- as.data.frame(
  emmeans(input_model_by_class, ~ input_pair | Class)
)
write_csv(
  input_pair_by_class_emmeans,
  file.path(RESULTS_DIR, "section_4_1_input_pair_by_class_emmeans.csv")
)

###############################################################################
# SECTION 4.1C SENSITIVITY: GPT AND GEMINI ONLY
#
# Claude memory output is missing for 100 books, concentrated in LOWLIB,
# LOWRATE, and WORLD, so the class-specific memory estimates above rely on few
# Claude books plus the additive generating-model term. This refit drops Claude
# entirely and repeats the input-pair x class model on the two generating
# models with near-complete coverage. It is a sensitivity check, not a
# replacement for the paper model.
###############################################################################

sensitivity_models <- c("GPT", "Gemini")
sensitivity_data <- input_set_similarity %>%
  filter(generating_model %in% sensitivity_models)
assert_replication_value(nrow(sensitivity_data), 2985, "GPT/Gemini cross-input moral sets")

sensitivity_no_interaction <- lmer(
  mean_similarity ~ input_pair + generating_model + Class + (1 | book_id),
  data = sensitivity_data,
  REML = FALSE
)
sensitivity_by_class <- lmer(
  mean_similarity ~ input_pair * Class + generating_model + (1 | book_id),
  data = sensitivity_data,
  REML = FALSE
)

sensitivity_lrt <- as.data.frame(anova(sensitivity_no_interaction, sensitivity_by_class))
sensitivity_lrt$model <- c("gpt_gemini_no_interaction", "gpt_gemini_by_class")
rownames(sensitivity_lrt) <- NULL
write_csv(
  sensitivity_lrt,
  file.path(RESULTS_DIR, "section_4_1_gpt_gemini_sensitivity_lrt.csv")
)

# Side-by-side estimates: the paper model (all three generating models) and the
# GPT/Gemini-only refit, with the number of book x model sets behind each cell.
# Asymptotic df matches the paper model, which exceeds emmeans' default
# Kenward-Roger limit of 3,000 observations; this refit falls below it.
sensitivity_emmeans <- as.data.frame(
  emmeans(sensitivity_by_class, ~ input_pair | Class, lmer.df = "asymptotic")
) %>%
  transmute(
    input_pair = as.character(input_pair), Class = as.character(Class),
    emmean_gpt_gemini = emmean, SE_gpt_gemini = SE,
    lower_CI_gpt_gemini = asymp.LCL, upper_CI_gpt_gemini = asymp.UCL
  )
sensitivity_comparison <- input_pair_by_class_emmeans %>%
  transmute(
    input_pair = as.character(input_pair), Class = as.character(Class),
    emmean_all_models = emmean
  ) %>%
  left_join(sensitivity_emmeans, by = c("input_pair", "Class")) %>%
  left_join(
    input_set_similarity %>%
      mutate(input_pair = as.character(input_pair)) %>%
      count(input_pair, Class, generating_model) %>%
      pivot_wider(
        names_from = generating_model, values_from = n,
        names_prefix = "n_sets_", values_fill = 0
      ),
    by = c("input_pair", "Class")
  ) %>%
  mutate(difference = emmean_gpt_gemini - emmean_all_models)
write_csv(
  sensitivity_comparison,
  file.path(RESULTS_DIR, "section_4_1_gpt_gemini_sensitivity_emmeans.csv")
)

###############################################################################
# SECTION 4.1D: WITHIN-FULL-TEXT DIVERSITY BENCHMARK
#
# This is descriptive. It is not a random or chance baseline.
###############################################################################

within_full_benchmark <- within_full_set_similarity %>%
  summarise(
    sd_between_sets = sd(mean_similarity),
    mean_similarity = mean(mean_similarity),
    n_book_model_sets = n(),
    n_books = n_distinct(book_id)
  )
write_csv(
  within_full_benchmark,
  file.path(RESULTS_DIR, "section_4_1_within_full_text_benchmark.csv")
)
within_full_mean <- within_full_benchmark$mean_similarity[[1]]

###############################################################################
# SECTION 4.1E: INPUT CHANGE VERSUS MODEL CHANGE
#
# For a symmetric comparison, use the three conditions with complete coverage
# across model families: full text, chunk summary, and plot-arc summary.
# Each retained book contributes nine set comparisons to each change type:
#   input change = 3 input pairs x 3 generating models
#   model change = 3 model pairs x 3 input conditions
# Books missing any of these 18 set comparisons are excluded from this test.
###############################################################################

benchmark_inputs <- c("full_text", "chunk_summary", "plotArc_summary")
benchmark_input_pairs <- c(
  "chunk_summary__full_text",
  "full_text__plotArc_summary",
  "chunk_summary__plotArc_summary"
)

input_change_by_book <- input_set_similarity %>%
  filter(input_pair %in% benchmark_input_pairs) %>%
  group_by(book_id, Class) %>%
  summarise(
    mean_similarity = mean(mean_similarity),
    n_set_comparisons = n(),
    .groups = "drop"
  ) %>%
  filter(n_set_comparisons == 9) %>%
  mutate(change_type = "input_change")

model_change_by_book <- model_set_similarity %>%
  filter(input_type %in% benchmark_inputs) %>%
  group_by(book_id, Class) %>%
  summarise(
    mean_similarity = mean(mean_similarity),
    n_set_comparisons = n(),
    .groups = "drop"
  ) %>%
  filter(n_set_comparisons == 9) %>%
  mutate(change_type = "model_change")

benchmark_books <- intersect(input_change_by_book$book_id, model_change_by_book$book_id)
assert_replication_value(length(benchmark_books), 245, "balanced benchmark books")
change_type_data <- bind_rows(
  filter(input_change_by_book, book_id %in% benchmark_books),
  filter(model_change_by_book, book_id %in% benchmark_books)
) %>%
  mutate(change_type = relevel(factor(change_type), ref = "input_change"))

change_type_model <- lmer(
  mean_similarity ~ change_type + Class + (1 | book_id),
  data = change_type_data,
  REML = TRUE
)
change_type_emmeans <- as.data.frame(emmeans(change_type_model, ~ change_type))
change_type_contrast <- as.data.frame(pairs(emmeans(change_type_model, ~ change_type)))

write_csv(
  change_type_data,
  file.path(RESULTS_DIR, "section_4_1_change_type_book_means.csv")
)
write_csv(
  change_type_emmeans,
  file.path(RESULTS_DIR, "section_4_1_change_type_emmeans.csv")
)
write_csv(
  change_type_contrast,
  file.path(RESULTS_DIR, "section_4_1_change_type_contrast.csv")
)

# Paper-facing benchmark means in one table. The within-full-text value is a
# descriptive within-response diversity reference; only the
# input-change/model-change contrast is tested.
benchmark_means <- bind_rows(
  change_type_emmeans %>%
    transmute(
      comparison = as.character(change_type),
      mean_similarity = emmean,
      SE = SE,
      lower_CI = lower.CL,
      upper_CI = upper.CL,
      role = "balanced model estimate"
    ),
  tibble(
    comparison = "within_full_text",
    mean_similarity = within_full_mean,
    SE = NA_real_,
    lower_CI = NA_real_,
    upper_CI = NA_real_,
    role = "descriptive within-response moral diversity (full text)"
  )
)
write_csv(
  benchmark_means,
  file.path(RESULTS_DIR, "section_4_1_benchmark_means.csv")
)

change_type_coefficients <- as.data.frame(coef(summary(change_type_model)))
change_type_coefficients$term <- rownames(change_type_coefficients)
rownames(change_type_coefficients) <- NULL
write_csv(
  change_type_coefficients,
  file.path(RESULTS_DIR, "section_4_1_change_type_model_coefficients.csv")
)

###############################################################################
# SECTION 4.1 FIGURE: FULL TEXT VERSUS THE THREE ALTERNATIVES BY BOOK CLASS
###############################################################################

figure_data <- input_pair_by_class_emmeans %>%
  filter(input_pair %in% c(
    "chunk_summary__full_text",
    "full_text__plotArc_summary",
    "full_text__memory"
  )) %>%
  mutate(
    input_pair = factor(
      input_pair,
      levels = c(
        "chunk_summary__full_text",
        "full_text__plotArc_summary",
        "full_text__memory"
      ),
      labels = c("Full->Chunk", "Full->Plot", "Full->Memory")
    ),
    Class = factor(Class, levels = c("SUPER", "BEST", "LOWLIB", "LOWRATE", "WORLD"))
  )

figure_2 <- ggplot(
  figure_data,
  aes(input_pair, emmean, group = Class, color = Class)
) +
  geom_hline(
    yintercept = within_full_mean,
    linetype = "dashed", color = "grey40", linewidth = 0.5
  ) +
  annotate(
    "text", x = 0.75, y = within_full_mean + 0.005, hjust = 0,
    label = "within-response moral similarity (full text)",
    size = 3, color = "grey40"
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(expand = expansion(add = 0.3)) +
  labs(x = "Input comparison", y = "Mean moral-set similarity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(RESULTS_DIR, "section_4_1_figure_input_similarity.pdf"),
  figure_2, width = 6.5, height = 4.5, units = "in"
)
ggsave(
  file.path(FIGURES_DIR, "figure_2_input_similarity.pdf"),
  figure_2, width = 6.5, height = 4.5, units = "in"
)

cat("Section 4.1 complete.\n")
cat("Cross-input set observations:", nrow(input_set_similarity), "\n")
cat("Books in balanced input-vs-model benchmark:", length(benchmark_books), "\n")
cat("Within-response moral diversity (full text):", sprintf("%.4f", within_full_mean), "\n")
cat("GPT/Gemini-only sensitivity, full text vs memory by class:\n")
sensitivity_comparison %>%
  filter(input_pair == "full_text__memory") %>%
  transmute(Class, all_models = round(emmean_all_models, 3),
            gpt_gemini = round(emmean_gpt_gemini, 3)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

writeLines(
  capture.output(sessionInfo()),
  file.path(RESULTS_DIR, "section_4_1_session_info.txt")
)
