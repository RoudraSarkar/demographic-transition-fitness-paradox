# ============================================================================
# 12_h1_cr2_clubsandwich.R
# H1 — Small-cluster inference via CR2 corrections with Satterthwaite df
#
# Fixes fix-list item 4.8. The dissertation (§3.5, §5.3, §7.3) reports that
# the wild cluster bootstrap recommended at 21 clusters (Cameron, Gelbach &
# Miller 2008) could not be run because fwildclusterboot was unavailable for
# R 4.5.1, and falls back on standard cluster-robust SEs "with appropriate
# caution". clubSandwich provides CR2 corrections with Satterthwaite degrees
# of freedom, is actively maintained, and addresses the same 21-cluster
# concern. Running it converts a stated weakness into a robustness result.
#
# WHY CR2. The standard cluster-robust (CR0) estimator is biased downward when
# the number of clusters is small, because residuals from a fitted model are
# systematically too small. CR2 applies a bias reduction analogous to HC2 in
# the heteroskedastic case, and pairing it with Satterthwaite degrees of
# freedom rather than G-1 gives tests with much better size at small G.
# See Pustejovsky & Tipton (2018).
#
# IMPLEMENTATION NOTE. clubSandwich has no method for fixest objects, so each
# model is refitted with lm() and country/year entered as dummies. This is
# algebraically the same regression (Frisch-Waugh-Lovell), and the script
# ASSERTS that the coefficients match fixest to numerical tolerance rather
# than assuming it.
#
# Inputs:  data/derived/etfr_data.rds, data/derived/gradient_shape.rds
# Outputs: data/models/h1_cr2.rds
# Depends: 02_descriptive_dataset1.R, 02b_gradient_shape.R
# ============================================================================
install.packages("clubSandwich")
library(dplyr)
library(tidyr)
library(fixest)
library(clubSandwich)

setwd("/Users/whiz/Desktop/dissertation")
set.seed(42)

etfr_data <- readRDS("data/derived/etfr_data.rds")
gradient_shape <- readRDS("data/derived/gradient_shape.rds")

etfr_data <- etfr_data |>
  mutate(
    log_etfr  = log(etfr),
    education = factor(education, levels = c("medium", "low", "high"))
  )

cat("=== Input ===\n")
cat("etfr_data:", nrow(etfr_data), "rows\n")
cat("Clusters (countries):", n_distinct(etfr_data$country), "\n\n")

# ── 1. Refit M1 as lm and verify equivalence with fixest ─────────────────────

m1_fe <- feols(log_etfr ~ education | country + year,
               data = etfr_data, cluster = "country")

m1_lm <- lm(log_etfr ~ education + factor(country) + factor(year),
            data = etfr_data)

cf_fe <- coef(m1_fe)[c("educationlow", "educationhigh")]
cf_lm <- coef(m1_lm)[c("educationlow", "educationhigh")]

cat("=== Equivalence check: fixest vs lm refit ===\n")
print(rbind(fixest = round(cf_fe, 6), lm = round(cf_lm, 6)))
max_diff <- max(abs(cf_fe - cf_lm))
cat("Max absolute difference:", format(max_diff, scientific = TRUE), "\n")
stopifnot(max_diff < 1e-8)
cat("PASS - the lm refit is the same regression.\n\n")

# ── 2. CR2 with Satterthwaite df ─────────────────────────────────────────────

cr2 <- coef_test(m1_lm, vcov = "CR2", cluster = etfr_data$country,
                 test = "Satterthwaite")
cr2_edu <- cr2[rownames(cr2) %in% c("educationlow", "educationhigh"), ]

cat("=== CR2 with Satterthwaite df (reference: medium) ===\n")
print(cr2_edu)

# CR0 for comparison — this is what fixest reports.
cr0 <- coef_test(m1_lm, vcov = "CR0", cluster = etfr_data$country,
                 test = "naive-t")
cr0_edu <- cr0[rownames(cr0) %in% c("educationlow", "educationhigh"), ]

cat("\n=== CR0 (conventional CRVE, for comparison) ===\n")
print(cr0_edu)

cat("\n=== fixest clustered SEs (what the dissertation currently reports) ===\n")
print(round(se(m1_fe)[c("educationlow", "educationhigh")], 4))

# ── 3. Side-by-side comparison ───────────────────────────────────────────────

comparison <- data.frame(
  term       = rownames(cr2_edu),
  estimate   = round(cr2_edu$beta, 4),
  se_fixest  = round(as.numeric(se(m1_fe)[rownames(cr2_edu)]), 4),
  se_CR0     = round(cr0_edu$SE, 4),
  se_CR2     = round(cr2_edu$SE, 4),
  df_Satt    = round(cr2_edu$df_Satt, 1),
  p_fixest   = round(as.numeric(pvalue(m1_fe)[rownames(cr2_edu)]), 4),
  p_CR2      = round(cr2_edu$p_Satt, 4),
  row.names  = NULL
) |>
  mutate(se_inflation = round(se_CR2 / se_fixest, 3))

cat("\n\n=== COMPARISON: conventional vs CR2 ===\n")
cat("se_inflation > 1 means CR2 is more conservative, as expected at G = 21.\n\n")
print(comparison)

# ── 4. Strict H1 contrast (high vs low) under CR2 ────────────────────────────
# Recovered by refitting with low as reference, matching how the -0.187
# headline figure is obtained in the dissertation.

etfr_reflow <- etfr_data |>
  mutate(education = relevel(education, ref = "low"))

m1_lm_reflow <- lm(log_etfr ~ education + factor(country) + factor(year),
                   data = etfr_reflow)
m1_fe_reflow <- feols(log_etfr ~ education | country + year,
                      data = etfr_reflow, cluster = "country")

stopifnot(abs(coef(m1_lm_reflow)["educationhigh"] -
              coef(m1_fe_reflow)["educationhigh"]) < 1e-8)

cr2_strict <- coef_test(m1_lm_reflow, vcov = "CR2",
                        cluster = etfr_reflow$country,
                        test = "Satterthwaite")
cr2_strict_row <- cr2_strict[rownames(cr2_strict) == "educationhigh", ]

cat("\n\n=== STRICT H1 CONTRAST (high vs low) UNDER CR2 ===\n")
cat("This is the dissertation's headline H1 figure, currently -0.187 (p = 0.002).\n\n")
print(cr2_strict_row)

cat("\nSide by side:\n")
strict_cmp <- data.frame(
  method   = c("fixest CRVE", "clubSandwich CR2"),
  estimate = round(c(coef(m1_fe_reflow)["educationhigh"],
                     cr2_strict_row$beta), 4),
  se       = round(c(as.numeric(se(m1_fe_reflow)["educationhigh"]),
                     cr2_strict_row$SE), 4),
  df       = c(n_distinct(etfr_data$country) - 1,
               round(cr2_strict_row$df_Satt, 1)),
  p_value  = round(c(as.numeric(pvalue(m1_fe_reflow)["educationhigh"]),
                     cr2_strict_row$p_Satt), 4),
  row.names = NULL
)
print(strict_cmp)

cat("\nDoes H1 survive CR2?:",
    ifelse(cr2_strict_row$beta < 0 & cr2_strict_row$p_Satt < 0.05,
           "YES - still negative and significant at 5%",
           "NO - see p-value above"), "\n")

# ── 5. CR2 across the robustness specifications ──────────────────────────────
# Applies CR2 to each row of Table 5.3 so the whole battery uses small-cluster
# inference rather than only the baseline.

full_coverage_countries <- etfr_data |>
  group_by(country) |>
  summarise(n_years = n_distinct(year), .groups = "drop") |>
  filter(n_years == max(n_years)) |>
  pull(country)

type_a_countries <- gradient_shape |>
  filter(shape == "j_curve_composition") |>
  pull(country)

specs <- list(
  "Baseline"                 = etfr_data,
  "Balanced subsample"       = etfr_data |> filter(country %in% full_coverage_countries),
  "Excluding 2020-2021"      = etfr_data |> filter(!year %in% c(2020, 2021)),
  "Excluding U-shape comp."  = etfr_data |> filter(!country %in% type_a_countries)
)

cr2_row <- function(d, label) {
  m <- lm(log_etfr ~ education + factor(country) + factor(year), data = d)
  ct <- coef_test(m, vcov = "CR2", cluster = d$country, test = "Satterthwaite")
  lo <- ct[rownames(ct) == "educationlow", ]
  hi <- ct[rownames(ct) == "educationhigh", ]
  data.frame(
    Specification = label,
    N             = nrow(d),
    G             = n_distinct(d$country),
    low_beta      = round(lo$beta, 3),
    low_se_CR2    = round(lo$SE, 3),
    low_df        = round(lo$df_Satt, 1),
    low_p_CR2     = round(lo$p_Satt, 3),
    high_beta     = round(hi$beta, 3),
    high_se_CR2   = round(hi$SE, 3),
    high_df       = round(hi$df_Satt, 1),
    high_p_CR2    = round(hi$p_Satt, 3),
    row.names     = NULL
  )
}

cat("\n\n=== TABLE 5.3 UNDER CR2 ===\n")
cat("Note: the population-weighted row is omitted - clubSandwich handles\n")
cat("weighted lm differently and it needs separate treatment.\n\n")

cr2_table <- lapply(names(specs), function(nm) cr2_row(specs[[nm]], nm)) |>
  bind_rows()
print(cr2_table)

# ── 6. Does anything change? ─────────────────────────────────────────────────

cat("\n\n=== DOES CR2 CHANGE ANY CONCLUSION? ===\n")
flips <- cr2_table |>
  mutate(
    low_sig_CR2  = low_p_CR2  < 0.05,
    high_sig_CR2 = high_p_CR2 < 0.05
  ) |>
  select(Specification, low_beta, low_p_CR2, low_sig_CR2,
         high_beta, high_p_CR2, high_sig_CR2)
print(flips)

cat("\nBaseline high-vs-medium was p = 0.022 under fixest CRVE.\n")
cat("Under CR2 it is p =", cr2_table$high_p_CR2[cr2_table$Specification == "Baseline"], "\n")

# ── 7. Save ──────────────────────────────────────────────────────────────────

dir.create("data/models", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    comparison   = comparison,
    strict_cmp   = strict_cmp,
    cr2_table    = cr2_table,
    cr2_full     = cr2,
    cr2_strict   = cr2_strict_row,
    m1_lm        = m1_lm
  ),
  "data/models/h1_cr2.rds"
)
cat("\nSaved to data/models/h1_cr2.rds\n")


