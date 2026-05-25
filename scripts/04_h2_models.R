# ============================================================================
# 04_h2_models.R
# H2 FORMAL TESTING — Non-monotonicity (U-shape) of education-fertility gradient
# MSc Applied Social Data Science, TCD
#
# Hypothesis: The education-fertility relationship is non-monotonic (U-shaped),
# with medium-education women having the lowest fertility. This tests the 
# pattern discovered in H1 (not the J-curve predicted by the proposal).
#
# Methods: Quadratic fixed-effects model, quantile regression across the
# fertility distribution, statistical tests for curvature.
#
# Data:    etfr_data.rds (21 countries × 2007–2024 × 3 education levels)
# Outputs: data/models/h2_models.rds, output/figures/h2_*.png
# ============================================================================
install.packages("SparseM")
library(dplyr)
library(tidyr)
library(fixest)
library(quantreg)  # install.packages("quantreg") if needed
library(ggplot2)
library(patchwork)
library(SparseM)
setwd("/Users/whiz/Desktop/dissertation")

# ── 0. Load data ─────────────────────────────────────────────────────────────

etfr_data      <- readRDS("data/derived/etfr_data.rds")
gradient_shape <- readRDS("data/derived/gradient_shape.rds")

# Create log_etfr (needed for models)
etfr_data <- etfr_data |>
  mutate(log_etfr = log(etfr))

cat("=== Data loaded ===\n")

cat("=== Data loaded ===\n")
cat("etfr_data:", nrow(etfr_data), "rows\n")
cat("Education levels:", paste(unique(etfr_data$education), collapse = ", "), "\n")

# ── 1. Prepare education as numeric ordinal ──────────────────────────────────
# Coding: low = 0, medium = 1, high = 2
# This makes the intercept interpretable as low-education fertility,
# and positive coefficients mean fertility increases with education.

etfr_data <- etfr_data |>
  mutate(
    educ_num = case_when(
      education == "low"    ~ 0,
      education == "medium" ~ 1,
      education == "high"   ~ 2,
      TRUE ~ NA_real_
    ),
    educ_num_sq = educ_num^2  # quadratic term
  )

cat("\n=== Education coding check ===\n")
print(etfr_data |> 
        distinct(education, educ_num) |> 
        arrange(educ_num))

# ── 2. Linear model (baseline from H1, but with numeric education) ───────────
# For comparison with quadratic

m_linear <- feols(
  log_etfr ~ educ_num | country + year,
  data    = etfr_data,
  cluster = "country"
)

cat("\n\n=== M_LINEAR: education as linear predictor ===\n")
summary(m_linear)

# ── 3. Quadratic model (core H2 test) ────────────────────────────────────────
# H2 prediction: educ_num_sq coefficient should be POSITIVE (U-shape)
# Negative linear + positive quadratic = U-shape with minimum at medium

m_quad <- feols(
  log_etfr ~ educ_num + educ_num_sq | country + year,
  data    = etfr_data,
  cluster = "country"
)

cat("\n\n=== M_QUAD: quadratic education term (core H2 test) ===\n")
summary(m_quad)

# Extract coefficients for interpretation
beta_linear <- coef(m_quad)["educ_num"]
beta_quad   <- coef(m_quad)["educ_num_sq"]

cat("\n=== Interpretation ===\n")
cat("Linear term (β₁):", round(beta_linear, 4), "\n")
cat("Quadratic term (β₂):", round(beta_quad, 4), "\n")

if (beta_quad > 0) {
  cat("\n✓ U-shape confirmed: quadratic term is POSITIVE.\n")
  cat("  Minimum occurs at educ_num =", round(-beta_linear / (2 * beta_quad), 2), "\n")
  cat("  (Expected: ~1.0 for medium education)\n")
} else {
  cat("\n✗ Quadratic term is negative or zero — not a U-shape.\n")
}

# ── 4. Model comparison: linear vs quadratic ─────────────────────────────────

cat("\n\n=== Model comparison (AIC/BIC) ===\n")
cat("Linear AIC: ", AIC(m_linear), "\n")
cat("Quad   AIC: ", AIC(m_quad), "\n")
cat("Linear BIC: ", BIC(m_linear), "\n")
cat("Quad   BIC: ", BIC(m_quad), "\n")

# Likelihood ratio test (if models are nested)
# Note: fixest doesn't provide LRT directly; use AIC/BIC or wald test
cat("\n=== Wald test: quadratic term = 0 ===\n")
wald_quad <- wald(m_quad, "educ_num_sq")
print(wald_quad)

# ── 5. Quantile regression ───────────────────────────────────────────────────
# Test whether the U-shape varies across the fertility distribution
# (e.g., is the U-shape steeper in high-fertility vs low-fertility contexts?)

cat("\n\n=== Quantile regression (education as 3-level factor) ===\n")

# Quantiles to estimate
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# Note: quantreg doesn't support two-way FE with |, so we include country and year
# as fixed factors directly. This will be slower and less clean than fixest.
# Also: no clustering support in rq(), so SEs are less reliable.

q_models <- lapply(quantiles, function(tau) {
  cat("Fitting quantile", tau, "...\n")
  rq(log_etfr ~ factor(education) + factor(country) + factor(year),
     data = etfr_data,
     tau  = tau)
})

names(q_models) <- paste0("q_", quantiles)

# Extract coefficients for low and high (relative to medium, which we'll set as reference)
# rq() uses alphabetical ordering, so we need to check the factor levels
cat("\n=== Quantile regression coefficients (education contrasts) ===\n")

q_coefs <- lapply(quantiles, function(tau) {
  model <- q_models[[paste0("q_", tau)]]
  coefs <- coef(model)
  
  # Extract education coefficients (factor levels)
  # Default reference is first level alphabetically, which is "high"
  # We want medium as reference, so we need to refit or extract carefully
  
  # For now, just show the raw coefficients
  educ_coefs <- coefs[grepl("factor\\(education\\)", names(coefs))]
  data.frame(
    quantile = tau,
    term     = names(educ_coefs),
    estimate = as.numeric(educ_coefs)
  )
}) |> bind_rows()

print(q_coefs)

cat("\nNote: quantreg uses alphabetical reference level (likely 'high').\n")
cat("Coefficients show contrasts relative to that reference.\n")
cat("Interpretation: if educationlow is positive, low > reference.\n")

# ── 6. Visualizations ─────────────────────────────────────────────────────────

# 6a. Predicted values from quadratic model (pooled)
pred_data <- data.frame(
  educ_num = c(0, 1, 2),
  education = c("low", "medium", "high")
)

# Predict on the centered scale (country and year FE absorbed)
# We'll use the quadratic equation: β₀ + β₁*x + β₂*x²
# But since we have FE, we plot the quadratic curve only (relative to FE baseline)

pred_data$log_etfr_pred <- coef(m_quad)["educ_num"] * pred_data$educ_num + 
                           coef(m_quad)["educ_num_sq"] * pred_data$educ_num^2

pred_data$etfr_pred <- exp(pred_data$log_etfr_pred)

cat("\n=== Predicted fertility from quadratic model (relative scale) ===\n")
print(pred_data)

# Plot: quadratic curve
p_quad <- ggplot(pred_data, aes(x = educ_num, y = log_etfr_pred)) +
  geom_line(colour = "#2166ac", linewidth = 1) +
  geom_point(size = 3, colour = "#2166ac") +
  scale_x_continuous(
    breaks = c(0, 1, 2),
    labels = c("Low", "Medium", "High")
  ) +
  labs(
    title = "H2: Quadratic education-fertility relationship (pooled)",
    subtitle = paste0("β₁ = ", round(beta_linear, 3), 
                     ", β₂ = ", round(beta_quad, 3),
                     " | U-shape minimum at educ ≈ ", 
                     round(-beta_linear / (2 * beta_quad), 2)),
    x = "Education level",
    y = "log(eTFR) [relative to country-year FE]"
  ) +
  theme_minimal(base_size = 11)

# 6b. Quantile regression coefficients plot
# Show how the education contrasts vary across quantiles

if (nrow(q_coefs) > 0) {
  p_quantile <- ggplot(q_coefs, 
                       aes(x = quantile, y = estimate, colour = term)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    scale_colour_brewer(palette = "Set1") +
    labs(
      title = "Quantile regression: education coefficients across fertility distribution",
      subtitle = "How does the education gradient vary by fertility level?",
      x = "Quantile (τ)",
      y = "Coefficient (log eTFR)",
      colour = "Contrast"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
} else {
  p_quantile <- NULL
}

# Save figures
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

ggsave("output/figures/h2_quadratic_curve.png",
       p_quad, width = 7, height = 5, dpi = 300)

if (!is.null(p_quantile)) {
  ggsave("output/figures/h2_quantile_coefficients.png",
         p_quantile, width = 8, height = 5, dpi = 300)
}

cat("\nFigures saved to output/figures/\n")

# ── 7. Save model objects ─────────────────────────────────────────────────────

dir.create("data/models", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  list(
    m_linear  = m_linear,
    m_quad    = m_quad,
    q_models  = q_models,
    pred_data = pred_data,
    q_coefs   = q_coefs
  ),
  "data/models/h2_models.rds"
)

cat("\nModel objects saved to data/models/h2_models.rds\n")

# ── 8. Summary table ──────────────────────────────────────────────────────────

cat("\n\n=== RESULTS TABLE — linear vs quadratic ===\n")
etable(
  list("Linear" = m_linear, "Quadratic" = m_quad),
  digits  = 4,
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2", "aic", "bic")
)

# ── 9. Methodology log — new entries ─────────────────────────────────────────

cat("
====================================================================
METHODOLOGY LOG — NEW ENTRIES FROM H2 MODELLING
====================================================================

31. H2 specification: Quadratic fixed-effects model to test for U-shape.
    Education recoded as ordinal numeric (low = 0, medium = 1, high = 2).
    Model: feols(log_etfr ~ educ_num + educ_num_sq | country + year).
    N = 933 observations (21 countries × 2007–2024).

32. H2 result: [TO BE FILLED AFTER RUNNING]
    Linear term (β₁): [paste value]
    Quadratic term (β₂): [paste value, p-value]
    U-shape confirmed: [YES/NO — if β₂ > 0 and p < 0.05]
    Minimum at educ_num ≈ [paste -β₁/(2β₂)]
    AIC comparison: Linear [value] vs Quadratic [value]
    
33. Quantile regression: [TO BE FILLED]
    Estimated at τ = 0.1, 0.25, 0.5, 0.75, 0.9 to test whether
    the U-shape varies across the fertility distribution.
    [Describe pattern if clear — e.g., 'U-shape steeper at 
    lower quantiles' or 'gradient stable across quantiles']
    Note: SEs unreliable due to lack of clustering support in rq().
    
====================================================================
")

cat("\nDone. Run the script and paste back:\n")
cat("  1. The m_quad summary output\n")
cat("  2. The Wald test result for educ_num_sq\n")
cat("  3. The AIC/BIC comparison table\n")
cat("  4. The predicted values table\n")
