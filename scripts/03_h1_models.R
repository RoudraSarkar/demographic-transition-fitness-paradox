# ============================================================================
# 03_h1_models.R
# H1 FORMAL MODELLING — Education-fertility gradient (period eTFR)
# MSc Applied Social Data Science, TCD
#
# Hypothesis: For European countries 2007–2024, the relationship between
# education and period eTFR is negative. Given descriptive evidence of a
# U-shape (medium is lowest), we test whether H1 holds for the low–high
# contrast and document non-monotonicity as the substantive finding.
#
# Outcome:    log(eTFR)   — bounded positive, multiplicative structure
# Reference:  education = "medium"  — cleanest contrast for U-shape
# Panel:      21 countries × 2007–2024 (unbalanced)
# FE:         country + year (two-way)
# Clustering: country (21 clusters — report wild-bootstrap SEs as robustness)
# ============================================================================
install.packages("fixest")
install.packages("patchwork")
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(patchwork)  

setwd("/Users/whiz/Desktop/dissertation")

# ── 0. Load data ─────────────────────────────────────────────────────────────

etfr_data      <- readRDS("data/derived/etfr_data.rds")
gradient_shape <- readRDS("data/derived/gradient_shape.rds")

# Quick sanity checks
cat("=== Data dimensions ===\n")
cat("etfr_data:", nrow(etfr_data), "rows\n")
cat("Countries:", n_distinct(etfr_data$country), "\n")
cat("Years:", min(etfr_data$year), "–", max(etfr_data$year), "\n")
cat("Education levels:", paste(unique(etfr_data$education), collapse = ", "), "\n")

cat("\n=== eTFR range check (log safety) ===\n")
cat("Min eTFR:", min(etfr_data$etfr, na.rm = TRUE), "\n")
cat("Max eTFR:", max(etfr_data$etfr, na.rm = TRUE), "\n")
cat("Any NA?:", any(is.na(etfr_data$etfr)), "\n")

# ── 1. Prepare data ───────────────────────────────────────────────────────────

etfr_data <- etfr_data |>
  mutate(
    log_etfr  = log(etfr),
    education = factor(education, levels = c("medium", "low", "high"))  # medium = reference
  )

cat("\n=== Education distribution ===\n")
print(table(etfr_data$education))

# ── 2. Baseline model: two-way FE, education contrasts ───────────────────────
# m1: pooled gradient — core H1 test

m1 <- feols(
  log_etfr ~ education | country + year,
  data    = etfr_data,
  cluster = "country"
)

cat("\n\n=== M1: Baseline two-way FE (ref = medium) ===\n")
summary(m1)

# Manual coefficient extraction for clean reporting
m1_coefs <- broom::tidy(m1, conf.int = TRUE)
cat("\n=== M1 coefficients ===\n")
print(m1_coefs)

# Derived contrast: high vs low (H1 strict test)
cat("\n=== Wald test: high vs low (H1 strict) ===\n")
wald(m1, "educationhigh - educationlow")
# Negative coefficient → H1 holds; positive → H1 rejected at pooled level

# ── 3. Cross-country heterogeneity: country × education interactions ──────────
# m2: allows each country's gradient to deviate from the reference (Sweden)

# Check Sweden is in data
stopifnot("Sweden" %in% etfr_data$country)

m2 <- feols(
  log_etfr ~ education + i(country, education, ref = "Sweden") | country + year,
  data    = etfr_data,
  cluster = "country"
)

cat("\n\n=== M2: Country × education interactions (ref country = Sweden) ===\n")
summary(m2)

# ── 4. Robustness checks ──────────────────────────────────────────────────────

# 4a. Balanced subsample (countries with full 2007–2024 coverage)
full_coverage_countries <- etfr_data |>
  group_by(country) |>
  summarise(
    n_years = n_distinct(year),
    .groups = "drop"
  ) |>
  filter(n_years == max(n_years)) |>
  pull(country)

cat("\n=== Countries with full panel coverage ===\n")
print(full_coverage_countries)

m1_balanced <- feols(
  log_etfr ~ education | country + year,
  data    = etfr_data |> filter(country %in% full_coverage_countries),
  cluster = "country"
)

cat("\n=== M1 (balanced subsample) ===\n")
summary(m1_balanced)

# 4b. Exclude pandemic years (2020–2021)
m1_no_covid <- feols(
  log_etfr ~ education | country + year,
  data    = etfr_data |> filter(!year %in% c(2020, 2021)),
  cluster = "country"
)

cat("\n=== M1 (excluding 2020–2021) ===\n")
summary(m1_no_covid)

# 4c. Exclude composition-driven Type A countries
# (small low-tier likely driven by Roma/ethnic minority populations)
type_a_countries <- gradient_shape |>
  filter(shape == "j_curve_composition") |>
  pull(country)

cat("\n=== Excluded (Type A composition-driven) ===\n")
print(type_a_countries)

m1_no_type_a <- feols(
  log_etfr ~ education | country + year,
  data    = etfr_data |> filter(!country %in% type_a_countries),
  cluster = "country"
)

cat("\n=== M1 (excluding Type A composition countries) ===\n")
summary(m1_no_type_a)

# 4d. Reference category sensitivity: "low" as reference
m1_ref_low <- feols(
  log_etfr ~ factor(education, levels = c("low", "medium", "high")) | country + year,
  data    = etfr_data,
  cluster = "country"
)

cat("\n=== M1 (ref = low, sensitivity check) ===\n")
summary(m1_ref_low)

# 4d. Wild cluster bootstrap SEs (more reliable with N=21 clusters)
# Use boottest or fwildclusterboot if available
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  library(fwildclusterboot)
  boot_m1 <- boottest(m1,
                      clustid = "country",
                      param = c("educationlow", "educationhigh"),
                      B = 9999,
                      seed = 42)
  cat("\n=== Wild bootstrap p-values (N=21 clusters) ===\n")
  print(summary(boot_m1))
} else {
  cat("\nNote: fwildclusterboot not installed. Run:\n")
  cat("  install.packages('fwildclusterboot')\n")
  cat("Wild bootstrap SEs recommended with only 21 clusters.\n")
}

# 4e. Population-weighted (weight by implied female population per cell)
asfr_data <- readRDS("data/derived/asfr_data.rds")

pop_weights <- asfr_data |>
  group_by(country, year, education) |>
  summarise(pop_weight = sum(population, na.rm = TRUE), .groups = "drop")

etfr_weighted <- etfr_data |>
  mutate(log_etfr = log(etfr)) |>
  left_join(pop_weights, by = c("country", "year", "education"))

m1_weighted <- feols(
  log_etfr ~ education | country + year,
  data    = etfr_weighted,
  weights = ~pop_weight,
  cluster = "country"
)
cat("\n=== M1 (population-weighted) ===\n")
summary(m1_weighted)

# ── 5. Results table ──────────────────────────────────────────────────────────

# Build a clean coefficient table across all main specifications
models_list <- list(
  "M1 Baseline"          = m1,
  "M1 Balanced"          = m1_balanced,
  "M1 No COVID"          = m1_no_covid,
  "M1 No Type-A"         = m1_no_type_a
)

cat("\n\n=== RESULTS TABLE — all specifications ===\n")
etable(
  models_list,
  keep    = c("educationlow", "educationhigh"),
  digits  = 3,
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

# ── 6. Country-level gradient visualisation ───────────────────────────────────

# 6a. Gradient by country type (shape classification)
plot_data <- etfr_data |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  left_join(gradient_shape |> select(country, shape), by = "country") |>
  mutate(education = factor(education, levels = c("low", "medium", "high")))

p_gradient <- ggplot(plot_data,
                     aes(x = education, y = mean_etfr,
                         group = country, colour = shape)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  facet_wrap(~ shape, ncol = 2) +
  scale_colour_manual(
    values = c(
      "monotonic_negative"  = "#2166ac",
      "j_curve_composition" = "#d73027",
      "j_curve_broad"       = "#fc8d59",
      "inverted_bottom"     = "#1a9641"
    ),
    guide = "none"
  ) +
  labs(
    title    = "Education–fertility gradient by country and gradient type",
    subtitle = "Mean period eTFR 2007–2024; each line = one country",
    x        = "Education level",
    y        = "Period eTFR",
    caption  = "Source: Eurostat demo_faeduc / edat_lfse_03"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# 6b. Pooled gradient with CI from M1
m1_plot_data <- data.frame(
  education = c("low", "medium", "high"),
  coef      = c(
    coef(m1)["educationlow"],
    0,  # reference
    coef(m1)["educationhigh"]
  ),
  ci_lo     = c(
    confint(m1)["educationlow", 1],
    NA,
    confint(m1)["educationhigh", 1]
  ),
  ci_hi     = c(
    confint(m1)["educationlow", 2],
    NA,
    confint(m1)["educationhigh", 2]
  )
) |>
  mutate(education = factor(education, levels = c("low", "medium", "high")))

p_pooled <- ggplot(m1_plot_data, aes(x = education, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(
    aes(ymin = ci_lo, ymax = ci_hi),
    colour = "#2166ac", size = 0.8
  ) +
  labs(
    title    = "Pooled education-fertility gradient (M1)",
    subtitle = "Coefficients from two-way FE; reference = medium; 95% CI",
    x        = "Education level",
    y        = "β (log eTFR, relative to medium)"
  ) +
  theme_minimal(base_size = 11)

# Save figures
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

ggsave("output/figures/h1_gradient_by_type.png",
       p_gradient, width = 10, height = 7, dpi = 300)

ggsave("output/figures/h1_pooled_fe_coefs.png",
       p_pooled, width = 6, height = 5, dpi = 300)

cat("\nFigures saved to output/figures/\n")

# ── 7. Save model objects ─────────────────────────────────────────────────────

dir.create("data/models", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  list(
    m1           = m1,
    m2           = m2,
    m1_balanced  = m1_balanced,
    m1_no_covid  = m1_no_covid,
    m1_no_type_a = m1_no_type_a,
    m1_ref_low   = m1_ref_low,
    m1_weighted  = m1_weighted
  ),
  "data/models/h1_models.rds"
)

cat("Model objects saved to data/models/h1_models.rds\n")

# ── 8. Methodology log — new entries ─────────────────────────────────────────
cat("
====================================================================
METHODOLOGY LOG — NEW ENTRIES FROM H1 MODELLING
====================================================================

26. H1 outcome: log(eTFR). Log transformation applied because eTFR is
    bounded below at zero, right-skewed across education groups, and
    because the multiplicative structure of fertility-education
    relationships is better captured on the log scale. Coefficients
    are interpreted as percentage differences in fertility by education.

27. Reference category: 'medium' education. Chosen over 'low' because
    (a) 'low' is the compositionally noisy category in 8 countries
    (Type A U-shape countries with Roma/ethnic minority populations),
    and (b) medium-as-reference produces two contrasts (low-vs-med,
    high-vs-med) that directly quantify the U-shape. The H1 strict
    test (high vs low) is derived via Wald test.

28. Baseline specification: feols(log_etfr ~ education | country +
    year, cluster = 'country'). Two-way fixed effects absorb country-
    level time-invariant confounders and common year shocks.
    Standard errors clustered at country level (N=21 clusters).
    Wild bootstrap SEs reported as robustness given small N clusters.

29. Robustness checks: (a) balanced subsample (full 2007-2024 coverage,
    N=X countries); (b) excluding 2020-2021 pandemic years; (c)
    (c) excluding composition-driven Type A countries (Croatia, Czechia,
Estonia, Finland, Latvia, Poland, Slovakia, Slovenia); (d) reference category
    sensitivity (low as reference); (e) population-weighted;
    (f) wild bootstrap SEs. Results [broadly stable / vary as follows].
====================================================================
")

cat("\nDone. Run the script and paste the M1 coefficient output back.\n")
cat("Key quantities to note:\n")
cat("  1. coef(m1)['educationlow']  — expected positive\n")
cat("  2. coef(m1)['educationhigh'] — expected positive (U-shape confirmation)\n")
cat("  3. Wald test high vs low     — sign determines H1 verdict\n")



