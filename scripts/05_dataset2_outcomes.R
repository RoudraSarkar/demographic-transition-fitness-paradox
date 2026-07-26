# ============================================================================
# 05_dataset2_outcomes.R
# DATASET 2 (H3) — PART 1: Derive outcome variables from Dataset 1
# Creates country-level gradient characteristics (outcomes for H3 models)
# from the education-fertility panel (Dataset 1).
#
# Inputs:  data/derived/etfr_data.rds, data/derived/gradient_shape.rds
# Outputs: data/derived/dataset2_outcomes.rds
# ============================================================================

library(dplyr)
library(tidyr)

setwd("/Users/whiz/Desktop/dissertation")

# ── 1. Load Dataset 1 ────────────────────────────────────────────────────────

etfr_data      <- readRDS("data/derived/etfr_data.rds")
gradient_shape <- readRDS("data/derived/gradient_shape.rds")

cat("=== Data loaded ===\n")
cat("etfr_data:", nrow(etfr_data), "rows,", n_distinct(etfr_data$country), "countries\n")
cat("gradient_shape:", nrow(gradient_shape), "rows\n")

# ── 2. Compute country-level means (average across 2007–2024 panel) ──────────

country_means <- etfr_data |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_etfr,
              names_prefix = "etfr_")

cat("\n=== Country means computed ===\n")
print(country_means, n = 5)

# ── 3. Derive outcome variables ──────────────────────────────────────────────

outcomes <- country_means |>
  mutate(
    # Gradient steepness: low - high (positive = low > high)
    gradient_steepness = etfr_low - etfr_high,
    
    # Alternative: log ratio (interpretable as percentage difference)
    gradient_log_ratio = log(etfr_low / etfr_high),
    
    # U-shape indicator: medium is lowest tier
    u_shape = as.integer(etfr_medium < etfr_low & etfr_medium < etfr_high),
    
    # Gradient range: max - min across three tiers
    gradient_range = pmax(etfr_low, etfr_medium, etfr_high) - 
                     pmin(etfr_low, etfr_medium, etfr_high)
  )

# ── 4. Merge with gradient shape classification ──────────────────────────────

outcomes <- outcomes |>
  left_join(
    gradient_shape |> select(country, shape, share_low, share_medium, share_high),
    by = "country"
  )

# Recode shape to binary indicators for modeling
outcomes <- outcomes |>
  mutate(
    shape_monotonic   = as.integer(shape == "monotonic_negative"),
    shape_u_broad     = as.integer(shape == "j_curve_broad"),
    shape_u_comp      = as.integer(shape == "j_curve_composition"),
    shape_inverted    = as.integer(shape == "inverted_bottom")
  )

# ── 5. Summary and checks ─────────────────────────────────────────────────────

cat("\n=== Dataset 2 outcomes (country-level) ===\n")
print(outcomes, n = Inf)

cat("\n=== Outcome variable distributions ===\n")
cat("Gradient steepness: mean =", round(mean(outcomes$gradient_steepness), 3),
    ", SD =", round(sd(outcomes$gradient_steepness), 3), "\n")
cat("U-shape countries:", sum(outcomes$u_shape), "of", nrow(outcomes), "\n")
cat("Shape distribution:\n")
print(table(outcomes$shape))

# Check for missing values
cat("\n=== Missing values check ===\n")
print(colSums(is.na(outcomes)))

# ── 6. Save ───────────────────────────────────────────────────────────────────

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

saveRDS(outcomes, "data/derived/dataset2_outcomes.rds")

cat("\nSaved to data/derived/dataset2_outcomes.rds\n")
cat("Rows:", nrow(outcomes), "| Columns:", ncol(outcomes), "\n")


