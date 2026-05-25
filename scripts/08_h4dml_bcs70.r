# =============================================================================
# 09_h4_dml_estimation.R
# H4: Double Machine Learning Causal Estimation
# Education → Fertility (BCS70 cohort)
#
# Dissertation: MSc Applied Social Data Science, Trinity College Dublin
# Chapter 6 — Mechanisms Results
#
# Hypotheses addressed:
#   H1: Negative education-fertility gradient (confirmed via θ < 0)
#   H4: Education → fertility mediated by attitudes > economics
#
# Method: Partially Linear Regression (PLR) via DoubleML
#   Y = θ·D + g(X) + ε   (total effect)
#   Sequential mediation to decompose pathways M1/M2/M3
# =============================================================================

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────

required_pkgs <- c("DoubleML", "mlr3", "mlr3learners", "ranger",
                   "dplyr", "tidyr", "ggplot2", "stringr", "purrr",
                   "data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(data.table)

set.seed(20250525)  # Reproducibility

# ── 1. LOAD DATA ──────────────────────────────────────────────────────────────

cat("Loading analytical data...\n")

data_path <- "~/Desktop/dissertation/data/processed/h4_analytical_data.rds"
data_list  <- readRDS(data_path)

df_analysis <- data_list$df_analysis   # N = 8,318 (pooled)
df_women    <- data_list$df_women      # N = 4,445
df_men      <- data_list$df_men        # N = 3,873

cat(sprintf("Loaded: pooled N=%d | women N=%d | men N=%d\n",
            nrow(df_analysis), nrow(df_women), nrow(df_men)))

# ── 2. VARIABLE SETUP ─────────────────────────────────────────────────────────

# Core variables
Y_VAR <- "y"          # Completed fertility (age 42)
D_VAR <- "d"          # NVQ level at age 30

# Mediator variable names
M_ATTITUDINAL  <- c("m_trad_gender", "m_profamily")
M_ECONOMIC     <- c("m_log_earn")
M_PARTNERSHIP  <- c("m_partner")
ALL_MEDIATORS  <- c(M_ATTITUDINAL, M_ECONOMIC, M_PARTNERSHIP)

# Candidate confounders (pre-treatment, age ≤ 16)
X_CANDIDATES <- c(
  "soc_birth", "mage", "mage_fb", "parity", "birth_order",  # birth
  "cog_10", "soc_10", "inc_10",                              # age 10
  "cog_16", "soc_16", "malaise_16"                           # age 16
)

# Helper: select confounders with >30% non-missing coverage in a given df
select_confounders <- function(df, candidates, min_coverage = 0.30,
                               add_vars = NULL) {
  available <- candidates[candidates %in% names(df)]
  coverage  <- sapply(available, function(v) mean(!is.na(df[[v]])))
  kept      <- names(coverage[coverage >= min_coverage])
  if (!is.null(add_vars)) kept <- union(kept, add_vars[add_vars %in% names(df)])
  cat(sprintf("  Confounders kept (%d): %s\n",
              length(kept), paste(kept, collapse = ", ")))
  kept
}

# ── 3. COMPLETE-CASE PREPARATION ─────────────────────────────────────────────

prepare_dml_data <- function(df, y_var, d_var, x_vars,
                             extra_vars = NULL, label = "") {
  keep_vars <- unique(c(y_var, d_var, x_vars, extra_vars))
  keep_vars <- keep_vars[keep_vars %in% names(df)]

  df_cc <- df %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    tidyr::drop_na()

  cat(sprintf("  [%s] complete cases: %d / %d (%.1f%%)\n",
              label, nrow(df_cc), nrow(df),
              100 * nrow(df_cc) / nrow(df)))
  df_cc
}

# ── 4. DML FITTING FUNCTION ───────────────────────────────────────────────────

fit_dml <- function(df_cc, y_var, d_var, x_vars,
                    n_folds = 5, n_rep = 3, label = "") {
  # Convert to data.table (DoubleML requirement)
  df_cc <- data.table::as.data.table(df_cc)
  
  # Build DoubleMLData object
  dml_data <- DoubleMLData$new(
    data         = df_cc,
    y_col        = y_var,
    d_cols       = d_var,
    x_cols       = x_vars
  )

  # Random forest learner (shared for both nuisance models)
  learner_y <- lrn("regr.ranger",
                   num.trees      = 500,
                   min.node.size  = 5,
                   num.threads    = 4)
  learner_d <- lrn("regr.ranger",
                   num.trees      = 500,
                   min.node.size  = 5,
                   num.threads    = 4)

  # Partially Linear Regression (PLR) — DML2 with partialling-out score
  dml_model <- DoubleMLPLR$new(
    data          = dml_data,
    ml_l          = learner_y,  # nuisance: E[Y | X]
    ml_m          = learner_d,  # nuisance: E[D | X]
    n_folds       = n_folds,
    n_rep         = n_rep,
    score         = "partialling out",
    dml_procedure = "dml2"
  )

  cat(sprintf("  Fitting DML for [%s] (n=%d, folds=%d, reps=%d)...\n",
              label, nrow(df_cc), n_folds, n_rep))

  dml_model$fit(store_predictions = TRUE)

  # Extract results
  smry     <- dml_model$summary()
  coef_val <- dml_model$coef[D_VAR]
  se_val   <- dml_model$se[D_VAR]

  # Nuisance performance (MSE)
  nuisance_scores <- dml_model$nuisance_loss

  cat(sprintf("  θ = %.4f (SE = %.4f, p = %.4f)\n",
              coef_val, se_val, smry[1, "Pr(>|t|)"]))

  list(
    model      = dml_model,
    coef       = coef_val,
    se         = se_val,
    pval       = smry[1, "Pr(>|t|)"],
    ci_lo      = coef_val - 1.96 * se_val,
    ci_hi      = coef_val + 1.96 * se_val,
    n          = nrow(df_cc),
    label      = label,
    nuisance   = nuisance_scores
  )
}

# ── 5. TOTAL EFFECT MODELS ────────────────────────────────────────────────────

cat("\n=== PHASE 1: TOTAL EFFECT (no mediators) ===\n")

# ---- 5a. WOMEN ---------------------------------------------------------------
cat("\n-- Women --\n")
x_women  <- select_confounders(df_women, X_CANDIDATES)
df_w_cc  <- prepare_dml_data(df_women, Y_VAR, D_VAR, x_women, label = "women")
res_w    <- fit_dml(df_w_cc, Y_VAR, D_VAR, x_women, label = "Women (total)")

# ---- 5b. MEN -----------------------------------------------------------------
cat("\n-- Men --\n")
x_men   <- select_confounders(df_men, X_CANDIDATES)
df_m_cc <- prepare_dml_data(df_men, Y_VAR, D_VAR, x_men, label = "men")
res_m   <- fit_dml(df_m_cc, Y_VAR, D_VAR, x_men, label = "Men (total)")

# ---- 5c. POOLED (add female as confounder) -----------------------------------
cat("\n-- Pooled --\n")
x_pooled <- select_confounders(df_analysis, X_CANDIDATES, add_vars = "female")
df_p_cc  <- prepare_dml_data(df_analysis, Y_VAR, D_VAR, x_pooled, label = "pooled")
res_p    <- fit_dml(df_p_cc, Y_VAR, D_VAR, x_pooled, label = "Pooled (total)")

# ── 6. RESULTS TABLE ──────────────────────────────────────────────────────────

total_results <- purrr::map_dfr(
  list(res_w, res_m, res_p),
  ~ tibble::tibble(
    Sample    = .x$label,
    N         = .x$n,
    theta     = round(.x$coef, 4),
    SE        = round(.x$se, 4),
    CI_lo     = round(.x$ci_lo, 4),
    CI_hi     = round(.x$ci_hi, 4),
    p_value   = round(.x$pval, 4),
    Sig       = dplyr::case_when(
      .x$pval < 0.001 ~ "***",
      .x$pval < 0.01  ~ "**",
      .x$pval < 0.05  ~ "*",
      .x$pval < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )
)

cat("\n=== TOTAL EFFECT RESULTS ===\n")
print(total_results, n = Inf)

# ── 7. MEDIATION ANALYSIS ─────────────────────────────────────────────────────
# Sequential controlled direct effect (CDE) approach:
#   Model 0 (baseline): Y ~ θ·D | X                    → total effect
#   Model 1 (attitudes): Y ~ θ·D | X + M_att           → CDE via attitudes
#   Model 2 (earnings):  Y ~ θ·D | X + M_att + M_earn  → CDE via economics
#   Model 3 (partner):   Y ~ θ·D | X + M_all           → full CDE
#
# Indirect effects recovered by subtraction:
#   Indirect_att  = θ_model0 − θ_model1
#   Indirect_earn = θ_model1 − θ_model2
#   Indirect_part = θ_model2 − θ_model3

cat("\n=== PHASE 2: MEDIATION DECOMPOSITION ===\n")

run_mediation <- function(df_raw, x_vars, sex_label) {
  cat(sprintf("\n--- Mediation: %s ---\n", sex_label))

  mediator_sets <- list(
    "M0 (total, no mediators)"           = character(0),
    "M1 (+attitudes)"                    = M_ATTITUDINAL,
    "M2 (+attitudes +earnings)"          = c(M_ATTITUDINAL, M_ECONOMIC),
    "M3 (+attitudes +earnings +partner)" = ALL_MEDIATORS
  )

  results <- vector("list", length(mediator_sets))

  for (i in seq_along(mediator_sets)) {
    med_name <- names(mediator_sets)[i]
    meds     <- mediator_sets[[i]]

    # Check availability
    meds_avail <- meds[meds %in% names(df_raw)]
    if (length(meds) > 0 && length(meds_avail) < length(meds)) {
      cat(sprintf("  Warning: mediators not found: %s\n",
                  paste(setdiff(meds, meds_avail), collapse = ", ")))
    }
    
    # Filter by coverage (only if we have mediators)
    if (length(meds_avail) > 0) {
      coverage_check <- vapply(meds_avail, 
                               function(v) mean(!is.na(df_raw[[v]])) > 0.3,
                               FUN.VALUE = logical(1))
      meds_avail <- meds_avail[coverage_check]
    }

    # Augment confounder set with available mediators
    x_augmented <- unique(c(x_vars, meds_avail))
    df_cc <- prepare_dml_data(df_raw, Y_VAR, D_VAR, x_augmented,
                               label = paste(sex_label, med_name))

    results[[i]] <- fit_dml(df_cc, Y_VAR, D_VAR, x_augmented,
                             label = paste(sex_label, med_name))
    results[[i]]$model_name <- med_name
    results[[i]]$mediators  <- meds_avail
  }

  # Compute indirect effects by subtraction
  theta <- sapply(results, `[[`, "coef")
  se    <- sapply(results, `[[`, "se")

  indirect_att  <- theta[1] - theta[2]
  indirect_earn <- theta[2] - theta[3]
  indirect_part <- theta[3] - theta[4]
  
  # Create named vector explicitly
  indirect_vec <- c(indirect_att, indirect_earn, indirect_part)
  names(indirect_vec) <- c("att", "earn", "part")

  cat(sprintf("\n  Total effect (θ):           %.4f\n", theta[1]))
  cat(sprintf("  Indirect via attitudes:     %.4f (%.1f%%)\n",
              indirect_att,  100 * abs(indirect_att)  / abs(theta[1])))
  cat(sprintf("  Indirect via earnings:      %.4f (%.1f%%)\n",
              indirect_earn, 100 * abs(indirect_earn) / abs(theta[1])))
  cat(sprintf("  Indirect via partnership:   %.4f (%.1f%%)\n",
              indirect_part, 100 * abs(indirect_part) / abs(theta[1])))
  cat(sprintf("  Direct effect (residual):   %.4f (%.1f%%)\n",
              theta[4],      100 * abs(theta[4])       / abs(theta[1])))

  list(
    models      = results,
    theta       = theta,
    se          = se,
    indirect    = indirect_vec,
    sex_label   = sex_label
  )
}

med_women <- run_mediation(df_women, x_women,  "Women")
med_men   <- run_mediation(df_men,   x_men,    "Men")

# ── 8. MEDIATION RESULTS TABLE ────────────────────────────────────────────────

make_mediation_table <- function(med_res) {
  models <- med_res$models
  purrr::map_dfr(models, ~ tibble::tibble(
    Sample     = med_res$sex_label,
    Model      = .x$model_name,
    Mediators  = paste(.x$mediators, collapse = ", "),
    N          = .x$n,
    theta      = round(.x$coef, 4),
    SE         = round(.x$se, 4),
    CI_lo      = round(.x$ci_lo, 4),
    CI_hi      = round(.x$ci_hi, 4),
    p_value    = round(.x$pval, 4)
  ))
}

mediation_table <- dplyr::bind_rows(
  make_mediation_table(med_women),
  make_mediation_table(med_men)
)

cat("\n=== MEDIATION RESULTS TABLE ===\n")
print(mediation_table, n = Inf)

# ── 9. PATHWAY DECOMPOSITION SUMMARY ─────────────────────────────────────────

make_pathway_table <- function(med_res) {
  # Safety check
  if (is.null(med_res$indirect) || length(med_res$theta) < 4) {
    warning(sprintf("Incomplete mediation results for %s", med_res$sex_label))
    return(tibble::tibble())
  }
  
  total <- med_res$theta[1]
  tibble::tibble(
    Sample    = med_res$sex_label,
    Pathway   = c("Attitudinal", "Economic (earnings)",
                  "Partnership", "Direct"),
    Effect    = c(med_res$indirect[["att"]],
                  med_res$indirect[["earn"]],
                  med_res$indirect[["part"]],
                  med_res$theta[4]),
    Pct_Total = round(100 * c(med_res$indirect[["att"]],
                              med_res$indirect[["earn"]],
                              med_res$indirect[["part"]],
                              med_res$theta[4]) / total, 1)
  )
}

pathway_table <- dplyr::bind_rows(
  make_pathway_table(med_women),
  make_pathway_table(med_men)
)

cat("\n=== PATHWAY DECOMPOSITION ===\n")
print(pathway_table, n = Inf)

# ── 10. VISUALISATIONS ────────────────────────────────────────────────────────

fig_dir <- "~/Desktop/dissertation/output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 10a. Coefficient plot: total effects ────────────────────────────────────
coef_plot_data <- total_results %>%
  dplyr::mutate(
    Sample  = factor(Sample,
                     levels = c("Women (total)", "Pooled (total)", "Men (total)")),
    sig_col = dplyr::if_else(p_value < 0.05, "#1a5276", "#7f8c8d")
  )

p_coef <- ggplot(coef_plot_data,
                 aes(x = theta, y = Sample, colour = sig_col)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = CI_lo, xmax = CI_hi), height = 0.2, linewidth = 0.8) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("θ = %.3f%s", theta, Sig)),
            vjust = -0.8, size = 3.5, colour = "black") +
  scale_colour_identity() +
  scale_x_continuous(
    limits = c(min(coef_plot_data$CI_lo) - 0.05,
               max(coef_plot_data$CI_hi) + 0.05)
  ) +
  labs(
    title    = "DML Causal Effect of Education on Completed Fertility",
    subtitle = "Partially Linear Regression (PLR) with Random Forest nuisance models",
    x        = "θ: Causal effect per NVQ level (children)",
    y        = NULL,
    caption  = "BCS70, N = 8,318. Confounders: birth characteristics, cognition, SES at ages 10 and 16.\n95% confidence intervals. Blue = p < 0.05."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "h4_dml_total_effects.png"),
       p_coef, width = 8, height = 4, dpi = 300)
cat("Saved: h4_dml_total_effects.png\n")

# ---- 10b. Mediation waterfall plot ───────────────────────────────────────────
waterfall_data <- pathway_table %>%
  dplyr::mutate(
    Pathway = factor(Pathway,
                     levels = c("Attitudinal", "Economic (earnings)",
                                "Partnership", "Direct")),
    fill_col = dplyr::case_when(
      Pathway == "Attitudinal"          ~ "#1a5276",
      Pathway == "Economic (earnings)"  ~ "#1e8449",
      Pathway == "Partnership"          ~ "#7d3c98",
      TRUE                              ~ "#e67e22"
    ),
    label = sprintf("%.3f\n(%.1f%%)", Effect, Pct_Total)
  )

p_waterfall <- ggplot(waterfall_data,
                      aes(x = Pathway, y = Effect, fill = fill_col)) +
  geom_col(width = 0.65, colour = "white") +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_text(aes(label = label,
                vjust = dplyr::if_else(Effect >= 0, -0.3, 1.2)),
            size = 3.2, colour = "black") +
  scale_fill_identity() +
  facet_wrap(~Sample) +
  labs(
    title    = "Mediation Decomposition: Education → Fertility Pathways",
    subtitle = "Sequential DML controlled direct effects",
    x        = NULL,
    y        = "Indirect effect (children per NVQ level)",
    caption  = "H4 test: attitudinal pathway vs economic pathway. BCS70 cohort."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text    = element_text(face = "bold", size = 12)
  )

ggsave(file.path(fig_dir, "h4_dml_mediation_waterfall.png"),
       p_waterfall, width = 10, height = 5, dpi = 300)
cat("Saved: h4_dml_mediation_waterfall.png\n")

# ── 11. SAVE MODEL OBJECTS & TABLES ───────────────────────────────────────────

models_dir <- "~/Desktop/dissertation/data/models"
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

output <- list(
  total_effects   = total_results,
  mediation_table = mediation_table,
  pathway_table   = pathway_table,
  models_total    = list(women = res_w$model, men = res_m$model, pooled = res_p$model),
  models_mediation = list(women = med_women, men = med_men),
  date_run        = Sys.time()
)

saveRDS(output, file.path(models_dir, "h4_dml_results.rds"))
cat("Saved: h4_dml_results.rds\n")

# Also write CSVs for dissertation tables
readr::write_csv(total_results,   file.path(models_dir, "h4_total_effects.csv"))
readr::write_csv(pathway_table,   file.path(models_dir, "h4_pathway_decomposition.csv"))
readr::write_csv(mediation_table, file.path(models_dir, "h4_mediation_sequential.csv"))

cat("\nAll outputs saved.\n")

# ── 12. SANITY CHECKS ─────────────────────────────────────────────────────────

cat("\n=== SANITY CHECKS ===\n")

check_result <- function(res, expected_sign = -1, label = "") {
  sign_ok  <- sign(res$coef) == expected_sign
  plaus_ok <- abs(res$coef) < 0.5   # not implausibly large
  se_ok    <- res$se > 0.01 & res$se < 0.2

  cat(sprintf(
    "  [%s] θ=%.4f | sign %s | plausible %s | SE %s\n",
    label,
    res$coef,
    ifelse(sign_ok,  "✓", "✗"),
    ifelse(plaus_ok, "✓", "✗"),
    ifelse(se_ok,    "✓", "✗")
  ))
}

check_result(res_w, label = "Women total")
check_result(res_m, label = "Men total")
check_result(res_p, label = "Pooled total")

# Check H4 prediction: attitudinal > economic
for (med in list(med_women, med_men)) {
  att_dom <- abs(med$indirect[["att"]]) > abs(med$indirect[["earn"]])
  cat(sprintf("  [%s] Attitudinal > Economic: %s (att=%.4f, earn=%.4f)\n",
              med$sex_label,
              ifelse(att_dom, "✓ (H4 supported)", "✗ (H4 not supported)"),
              med$indirect[["att"]],
              med$indirect[["earn"]]))
}

cat("\nDone! Script completed successfully.\n")

