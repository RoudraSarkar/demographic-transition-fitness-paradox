# ============================================================================
# 02b_gradient_shape.R
# Classify the 21 countries by education–fertility gradient shape.
#
# Inputs:  data/derived/etfr_data.rds   (period eTFR by country × year × education)
#          data/derived/asfr_data.rds   (age-specific FR, used for population shares)
# Outputs: data/derived/gradient_shape.rds
# Depends: 02_build_dataset1.R
#
# Classification logic (from methodology log entries 23–25):
#   monotonic_negative   : low > medium > high  (classic H1 pattern)
#   j_curve_composition  : U-shape (high > medium) AND share_low < 12%  (Type A — Roma/minority composition effect)
#   j_curve_broad        : U-shape (high > medium) AND share_low ≥ 12%  (Type B — structural)
#   inverted_bottom      : low ≤ medium
#   other_flat           : residual
# ============================================================================

library(dplyr)
library(tidyr)

setwd("/Users/whiz/Desktop/dissertation")

# ── 1. Load inputs ───────────────────────────────────────────────────────────

etfr_data <- readRDS("data/derived/etfr_data.rds")
asfr_data <- readRDS("data/derived/asfr_data.rds")

cat("etfr_data:", nrow(etfr_data), "rows,", n_distinct(etfr_data$country), "countries\n")
cat("asfr_data:", nrow(asfr_data), "rows\n")

# ── 2. Mean eTFR by country × education (across all years) ───────────────────

gradient_by_country <- etfr_data |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_etfr) |>
  mutate(
    monotonic_neg = low > medium & medium > high,
    j_curve = (low > medium) & (high > medium)                      # U-shape: high above medium
  )

# ── 3. Population share by education (ages 25–39, mean across panel) ─────────
# Using 25–39 because that's the prime reproductive window where the
# education distribution is most informative about the "low tier" composition.

share_by_country <- asfr_data |>
  filter(age_group %in% c("25-29", "30-34", "35-39")) |>
  group_by(country, year, education) |>
  summarise(pop = sum(population, na.rm = TRUE), .groups = "drop") |>
  group_by(country, year) |>
  mutate(share_pct = 100 * pop / sum(pop, na.rm = TRUE)) |>
  group_by(country, education) |>
  summarise(mean_share_pct = mean(share_pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_share_pct,
              names_prefix = "share_")

# ── 4. Combine and classify ──────────────────────────────────────────────────

gradient_shape <- gradient_by_country |>
  left_join(share_by_country, by = "country") |>
  mutate(
    shape = case_when(
      monotonic_neg                          ~ "monotonic_negative",
      j_curve & share_low <  12              ~ "j_curve_composition",   # Type A
      j_curve & share_low >= 12              ~ "j_curve_broad",         # Type B
      low <= medium                          ~ "inverted_bottom",
      TRUE                                   ~ "other_flat"
    )
  ) |>
  select(country, shape, low, medium, high,
         share_low, share_medium, share_high) |>
  arrange(shape, country)

# ── 5. Report and save ───────────────────────────────────────────────────────

cat("\n=== Gradient shape classification ===\n")
print(gradient_shape, n = Inf)

cat("\n=== Shape counts ===\n")
print(table(gradient_shape$shape))

saveRDS(gradient_shape, "data/derived/gradient_shape.rds")

cat("\nSaved to data/derived/gradient_shape.rds\n")
