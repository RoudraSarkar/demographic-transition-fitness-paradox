# ============================================================================
# 09_robustness_age2549.R
# ROBUSTNESS CHECK — Restricting the eTFR to ages 25–49
#
# Motivation (dissertation §3.2):
#   Education-specific female population is estimated by multiplying total
#   population by the education share within each five-year age band. This
#   assumes attainment is approximately uniform within the band. That is
#   defensible from 25 onward, but false at 15–19 and 20–24, where education
#   is still in progress and the low-attainment share is mechanically inflated.
#   Because those cells feed low-tier eTFR, which drives gradient steepness
#   (the H3 outcome), the U-shape could in principle be an artefact of them.
#
#   This script rebuilds the eTFR over ages 25–49 only (five age groups rather
#   than seven) and re-runs the H1 and H2 tests plus the shape classification.
#
# NOTE ON LEVELS: the restricted eTFR is mechanically LOWER than the full eTFR
#   because real births at 15–24 are dropped. This is expected and is not the
#   quantity of interest. With country and year FE on a logged outcome, level
#   shifts are absorbed; what identifies the coefficients is the relative
#   spacing across education tiers. The question is whether medium still sits
#   below both other tiers.
#
# Inputs:  data/derived/asfr_data.rds
#          data/derived/etfr_data.rds
#          data/derived/gradient_shape.rds
# Outputs: data/derived/etfr_data_2549.rds
#          data/derived/gradient_shape_2549.rds
#          data/models/h1_h2_models_2549.rds
#          output/figures/robustness_age2549_gradient.png
# Depends: 02_descriptive_dataset1.R, 02b_gradient_shape.R
# ============================================================================

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)

setwd("/Users/whiz/Desktop/dissertation")

set.seed(42)

# ── 0. Load inputs ───────────────────────────────────────────────────────────

asfr_data      <- readRDS("data/derived/asfr_data.rds")
etfr_full      <- readRDS("data/derived/etfr_data.rds")
gradient_shape <- readRDS("data/derived/gradient_shape.rds")

cat("=== Inputs loaded ===\n")
cat("asfr_data     :", nrow(asfr_data), "rows,",
    n_distinct(asfr_data$country), "countries\n")
cat("etfr_full     :", nrow(etfr_full), "rows\n")
cat("Age groups    :", paste(sort(unique(asfr_data$age_group)), collapse = ", "), "\n")

# ── 1. Rebuild eTFR restricted to ages 25–49 ─────────────────────────────────
# Full eTFR sums 7 age groups (15-19 ... 45-49); restricted sums 5 (25-29 ... 45-49).
# The 5x multiplier is the width of each age group and is unchanged.

ages_2549 <- c("25-29", "30-34", "35-39", "40-44", "45-49")

stopifnot(all(ages_2549 %in% unique(asfr_data$age_group)))

etfr_2549 <- asfr_data |>
  filter(age_group %in% ages_2549) |>
  group_by(country, year, education) |>
  summarise(
    n_age_groups = n(),
    etfr         = 5 * sum(asfr, na.rm = TRUE),
    .groups      = "drop"
  )

cat("\n=== Restricted panel built ===\n")
cat("etfr_2549     :", nrow(etfr_2549), "rows\n")
cat("Coverage: should be 5 age groups per cell\n")
print(table(etfr_2549$n_age_groups))

# Guard: the restricted panel must cover the same cells as the full panel.
# If it does not, the comparison is confounded by differential coverage.
cells_full <- etfr_full |> select(country, year, education) |> arrange(country, year, education)
cells_2549 <- etfr_2549 |> select(country, year, education) |> arrange(country, year, education)
cat("\nCells in full panel      :", nrow(cells_full), "\n")
cat("Cells in restricted panel:", nrow(cells_2549), "\n")
cat("Cells identical?         :", identical(cells_full, cells_2549), "\n")
if (!identical(cells_full, cells_2549)) {
  cat("WARNING: cell coverage differs between panels. Inspect before interpreting.\n")
  print(anti_join(cells_full, cells_2549, by = c("country", "year", "education")))
}

# ── 2. Descriptive comparison of levels and ordering ─────────────────────────

compare_tiers <- bind_rows(
  etfr_full |> mutate(panel = "Full (15-49)"),
  etfr_2549 |> mutate(panel = "Restricted (25-49)")
) |>
  group_by(panel, education) |>
  summarise(
    mean_etfr = round(mean(etfr, na.rm = TRUE), 3),
    min_etfr  = round(min(etfr,  na.rm = TRUE), 3),
    max_etfr  = round(max(etfr,  na.rm = TRUE), 3),
    .groups   = "drop"
  ) |>
  mutate(education = factor(education, levels = c("low", "medium", "high"))) |>
  arrange(panel, education)

cat("\n=== Pooled tier means: full vs restricted ===\n")
cat("(Levels fall by construction; the ordering is what matters.)\n")
print(compare_tiers, n = Inf)

# Is medium still the lowest tier on pooled means?
pooled_2549 <- compare_tiers |>
  filter(panel == "Restricted (25-49)") |>
  select(education, mean_etfr) |>
  pivot_wider(names_from = education, values_from = mean_etfr)

cat("\nMedium still lowest tier on pooled means (restricted)?: ",
    pooled_2549$medium < pooled_2549$low & pooled_2549$medium < pooled_2549$high, "\n")

# ── 3. Re-classify gradient shape on the restricted panel ────────────────────
# Uses the same rules as 02b_gradient_shape.R. Note that share_low is computed
# from ages 25-39 in that script and is therefore unchanged by this restriction,
# so the 12% threshold and its country assignments carry over untouched. Only
# the eTFR ordering can move.

gradient_by_country_2549 <- etfr_2549 |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_etfr) |>
  mutate(
    monotonic_neg = low > medium & medium > high,
    # NB: this matches 02b_gradient_shape.R's definition — "medium is lowest".
    # It is NOT the stricter definition in 02_descriptive_dataset1.R
    # (low > high & high > medium), which additionally requires low > high.
    u_shape_medium_lowest = (low > medium) & (high > medium),
    u_shape_strict        = (low > high) & (high > medium)
  )

shares <- gradient_shape |> select(country, share_low, share_medium, share_high)

gradient_shape_2549 <- gradient_by_country_2549 |>
  left_join(shares, by = "country") |>
  mutate(
    shape = case_when(
      monotonic_neg                                  ~ "monotonic_negative",
      u_shape_medium_lowest & share_low <  12        ~ "j_curve_composition",
      u_shape_medium_lowest & share_low >= 12        ~ "j_curve_broad",
      low <= medium                                  ~ "inverted_bottom",
      TRUE                                           ~ "other_flat"
    )
  ) |>
  select(country, shape, low, medium, high,
         share_low, share_medium, share_high,
         monotonic_neg, u_shape_medium_lowest, u_shape_strict) |>
  arrange(shape, country)

cat("\n=== Shape classification (restricted 25-49) ===\n")
print(gradient_shape_2549, n = Inf)

cat("\n=== Shape counts: full vs restricted ===\n")
counts_full <- gradient_shape |> count(shape, name = "full_15_49")
counts_2549 <- gradient_shape_2549 |> count(shape, name = "restricted_25_49")
print(full_join(counts_full, counts_2549, by = "shape"))

cat("\n=== Headline counts ===\n")
cat("Medium-lowest countries, full panel      :",
    sum(gradient_shape$shape %in% c("j_curve_composition", "j_curve_broad")), "\n")
cat("Medium-lowest countries, restricted panel:",
    sum(gradient_shape_2549$u_shape_medium_lowest), "\n")
cat("Strict U-shape (low>high>medium), restricted:",
    sum(gradient_shape_2549$u_shape_strict), "\n")
cat("Monotonic negative, restricted           :",
    sum(gradient_shape_2549$monotonic_neg), "\n")

# Which countries changed classification?
switched <- gradient_shape |>
  select(country, shape_full = shape) |>
  left_join(gradient_shape_2549 |> select(country, shape_2549 = shape), by = "country") |>
  filter(shape_full != shape_2549)

cat("\n=== Countries whose classification changed ===\n")
if (nrow(switched) == 0) {
  cat("None — classification is fully stable under the 25-49 restriction.\n")
} else {
  print(switched, n = Inf)
}

# Margin table: how close is each country to flipping?
# Small gaps here are the countries flagged in §4.4 (Poland, Greece, Estonia).
margins_2549 <- gradient_shape_2549 |>
  mutate(
    gap_low_medium  = round(low - medium, 3),
    gap_high_medium = round(high - medium, 3),
    min_gap         = round(pmin(abs(low - medium), abs(high - medium)), 3)
  ) |>
  select(country, shape, low, medium, high,
         gap_low_medium, gap_high_medium, min_gap) |>
  arrange(min_gap)

cat("\n=== Margin to reclassification (restricted panel, ascending) ===\n")
print(margins_2549, n = Inf)

# ── 4. H1 on the restricted panel ────────────────────────────────────────────
# Matches 03_h1_models.R: log outcome, medium as reference, two-way FE,
# country-clustered SEs.

prep <- function(d) {
  d |>
    mutate(
      log_etfr  = log(etfr),
      education = factor(education, levels = c("medium", "low", "high"))
    )
}

etfr_2549_m <- prep(etfr_2549)
etfr_full_m <- prep(etfr_full)

stopifnot(all(is.finite(etfr_2549_m$log_etfr)))

m1_full <- feols(log_etfr ~ education | country + year,
                 data = etfr_full_m, cluster = "country")

m1_2549 <- feols(log_etfr ~ education | country + year,
                 data = etfr_2549_m, cluster = "country")

cat("\n\n=== M1 baseline (full 15-49) ===\n")
summary(m1_full)

cat("\n\n=== M1 restricted (25-49) ===\n")
summary(m1_2549)

# Strict H1 contrast: high vs low. Recovered by re-estimating with low as
# reference, matching how the -0.187 figure is obtained in the dissertation.
refit_ref_low <- function(d) {
  feols(log_etfr ~ education | country + year,
        data    = d |> mutate(education = relevel(education, ref = "low")),
        cluster = "country")
}

m1_full_reflow <- refit_ref_low(etfr_full_m)
m1_2549_reflow <- refit_ref_low(etfr_2549_m)

cat("\n=== Strict H1 contrast (high vs low), full panel ===\n")
print(broom::tidy(m1_full_reflow, conf.int = TRUE) |> filter(term == "educationhigh"))

cat("\n=== Strict H1 contrast (high vs low), restricted panel ===\n")
print(broom::tidy(m1_2549_reflow, conf.int = TRUE) |> filter(term == "educationhigh"))

# ── 5. H2 quadratic on the restricted panel ──────────────────────────────────
# Matches 04_h2_models.R: educ coded 0/1/2, positive beta2 = U-shape.

add_educ_num <- function(d) {
  d |>
    mutate(
      educ_num = case_when(
        education == "low"    ~ 0,
        education == "medium" ~ 1,
        education == "high"   ~ 2
      ),
      educ_num_sq = educ_num^2
    )
}

q_full <- add_educ_num(etfr_full_m)
q_2549 <- add_educ_num(etfr_2549_m)

m_quad_full <- feols(log_etfr ~ educ_num + educ_num_sq | country + year,
                     data = q_full, cluster = "country")

m_quad_2549 <- feols(log_etfr ~ educ_num + educ_num_sq | country + year,
                     data = q_2549, cluster = "country")

cat("\n\n=== Quadratic (full 15-49) ===\n")
summary(m_quad_full)

cat("\n\n=== Quadratic (restricted 25-49) ===\n")
summary(m_quad_2549)

curve_min <- function(m) {
  b1 <- coef(m)["educ_num"]; b2 <- coef(m)["educ_num_sq"]
  if (b2 <= 0) return(NA_real_)
  as.numeric(-b1 / (2 * b2))
}

# A positive beta2 is NOT sufficient to establish a U-shape. It only means the
# curve is convex. For the curve to turn within the data, the minimum
# -beta1/(2*beta2) must lie inside the observed education range [0, 2]. If the
# minimum falls outside that interval, the fitted relationship is monotonic over
# the range the data actually cover — convex, but with no interior trough.
u_shape_verdict <- function(m, label) {
  b1 <- coef(m)["educ_num"]
  b2 <- coef(m)["educ_num_sq"]
  p2 <- broom::tidy(m) |> filter(term == "educ_num_sq") |> pull(p.value)
  mn <- curve_min(m)
  interior <- !is.na(mn) && mn > 0 && mn < 2
  verdict <- if (!is.na(b2) && b2 > 0 && p2 < 0.05 && interior) {
    "YES - convex with interior minimum"
  } else if (!is.na(b2) && b2 > 0 && !interior) {
    "NO - convex but minimum outside [0,2]; monotonic over the data range"
  } else if (!is.na(b2) && b2 > 0 && p2 >= 0.05) {
    "NO - curvature not distinguishable from zero"
  } else {
    "NO - not convex"
  }
  cat(label, "\n")
  cat("  beta1            :", round(b1, 4), "\n")
  cat("  beta2            :", round(b2, 4), "(p =", signif(p2, 3), ")\n")
  cat("  curve minimum    :", if (is.na(mn)) "n/a (concave)" else round(mn, 3), "\n")
  cat("  minimum in [0,2] :", interior, "\n")
  cat("  U-SHAPE VERDICT  :", verdict, "\n\n")
  invisible(list(b1 = b1, b2 = b2, p2 = p2, min = mn, interior = interior))
}

cat("\n=== U-shape curvature comparison ===\n")
uv_full <- u_shape_verdict(m_quad_full, "Full (15-49):")
uv_2549 <- u_shape_verdict(m_quad_2549, "Restricted (25-49):")

cat("HEADLINE - does the U-shape survive the 25-49 restriction?\n")
cat("  Full panel      :",
    if (uv_full$b2 > 0 && uv_full$interior) "U-shape" else "no U-shape", "\n")
cat("  Restricted panel:",
    if (uv_2549$b2 > 0 && uv_2549$interior) "U-shape" else "no U-shape", "\n")

# ── 6. Comparison table for the dissertation ─────────────────────────────────
# Formatted to slot in as an additional row of Table 5.3.

extract_row <- function(m, m_reflow, label) {
  tf <- broom::tidy(m)
  tr <- broom::tidy(m_reflow)
  data.frame(
    Specification = label,
    N             = nobs(m),
    low_beta      = round(tf$estimate[tf$term == "educationlow"],  3),
    low_se        = round(tf$std.error[tf$term == "educationlow"], 3),
    low_p         = round(tf$p.value[tf$term == "educationlow"],   3),
    high_beta     = round(tf$estimate[tf$term == "educationhigh"],  3),
    high_se       = round(tf$std.error[tf$term == "educationhigh"], 3),
    high_p        = round(tf$p.value[tf$term == "educationhigh"],   3),
    strict_beta   = round(tr$estimate[tr$term == "educationhigh"],  3),
    strict_se     = round(tr$std.error[tr$term == "educationhigh"], 3),
    strict_p      = round(tr$p.value[tr$term == "educationhigh"],   3),
    row.names     = NULL
  )
}

robust_table <- bind_rows(
  extract_row(m1_full, m1_full_reflow, "Baseline (ages 15-49)"),
  extract_row(m1_2549, m1_2549_reflow, "Restricted (ages 25-49)")
)

cat("\n\n=== TABLE 5.3 ROW — copy into the dissertation ===\n")
cat("Columns: low and high are contrasts vs medium; strict is high vs low.\n\n")
print(robust_table)

# ── 7. Figure ────────────────────────────────────────────────────────────────

plot_df <- bind_rows(
  etfr_full |> mutate(panel = "Full (15-49)"),
  etfr_2549 |> mutate(panel = "Restricted (25-49)")
) |>
  group_by(panel, country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  mutate(education = factor(education, levels = c("low", "medium", "high")))

p_robust <- ggplot(plot_df, aes(x = education, y = mean_etfr, group = country)) +
  geom_line(colour = "grey65", linewidth = 0.5) +
  geom_point(size = 1, colour = "grey35") +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               colour = "#2166ac", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point",
               colour = "#2166ac", size = 2.5) +
  facet_wrap(~ panel) +
  labs(
    title    = "Education-fertility gradient: full vs age-restricted panel",
    subtitle = "Grey = countries; blue = pooled mean. Levels fall under restriction by construction.",
    x        = "Education level",
    y        = "Mean period eTFR"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/robustness_age2549_gradient.png",
       p_robust, width = 9, height = 5, dpi = 300)

cat("\nFigure saved to output/figures/robustness_age2549_gradient.png\n")

# ── 8. Save ──────────────────────────────────────────────────────────────────

dir.create("data/models", showWarnings = FALSE, recursive = TRUE)

saveRDS(etfr_2549,           "data/derived/etfr_data_2549.rds")
saveRDS(gradient_shape_2549, "data/derived/gradient_shape_2549.rds")
saveRDS(
  list(
    m1_full        = m1_full,
    m1_2549        = m1_2549,
    m1_full_reflow = m1_full_reflow,
    m1_2549_reflow = m1_2549_reflow,
    m_quad_full    = m_quad_full,
    m_quad_2549    = m_quad_2549,
    robust_table   = robust_table,
    margins_2549   = margins_2549
  ),
  "data/models/h1_h2_models_2549.rds"
)

cat("\nSaved:\n")
cat("  data/derived/etfr_data_2549.rds\n")
cat("  data/derived/gradient_shape_2549.rds\n")
cat("  data/models/h1_h2_models_2549.rds\n")
