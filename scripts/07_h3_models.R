# ============================================================================
# 07_h3_models.R (UPDATED — with ESS attitudinal predictors)
# H3 ANALYSIS: Cultural vs Economic Explanations of SES-Fertility Gradient
# MSc Applied Social Data Science, TCD
#
# Tests whether cross-country variation in the education-fertility gradient
# is better predicted by attitudinal/cultural indicators or economic indicators.
#
# Three-part approach:
#   PART 1: XGBoost regression + LOOCV (feature discovery, 12 predictors)
#   PART 2: SHAP interpretation (3-way: Attitudinal vs Institutional vs Economic)
#   PART 3: Bayesian linear models (6 models, confirmatory inference)
#
# Predictor groups:
#   ATTITUDINAL (ESS, 3): ess_secular, ess_gender_egal, ess_autonomy
#   INSTITUTIONAL (V-Dem, 5): libdem, gender_equal, relig_free, polyarchy, egaldem
#   ECONOMIC (3): gdp_percap, flfp, contraceptive
#   REGIONAL (1): post_socialist
#
# Input:  data/derived/dataset2_full.rds (21 countries × 32 variables)
# Output: data/models/h3_xgboost.rds
#         data/models/h3_bayesian.rds
#         output/figures/h3_*.png
# ============================================================================

library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xgboost)
library(brms)
library(loo)

setwd("/Users/whiz/Desktop/dissertation")

# ── 0. LOAD DATA AND PREPARE ─────────────────────────────────────────────────

dataset2 <- readRDS("data/derived/dataset2_full.rds")

cat("=== Dataset 2 loaded ===\n")
cat("Dimensions:", nrow(dataset2), "×", ncol(dataset2), "\n\n")

# Convert any NaN to NA
dataset2 <- dataset2 |>
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# ── Define predictor groups (UPDATED: 4 groups, 12 predictors) ──
attitudinal_vars  <- c("ess_secular", "ess_gender_egal", "ess_autonomy")
institutional_vars <- c("libdem", "gender_equal", "relig_free", "polyarchy", "egaldem")
economic_vars     <- c("gdp_percap", "flfp", "contraceptive")
regional_vars     <- c("post_socialist")

all_predictors <- c(attitudinal_vars, institutional_vars, economic_vars, regional_vars)

# Outcome
outcome_var <- "gradient_steepness"

cat("=== Predictor groups (UPDATED) ===\n")
cat("Attitudinal  (", length(attitudinal_vars), "):", paste(attitudinal_vars, collapse = ", "), "\n")
cat("Institutional(", length(institutional_vars), "):", paste(institutional_vars, collapse = ", "), "\n")
cat("Economic     (", length(economic_vars), "):", paste(economic_vars, collapse = ", "), "\n")
cat("Regional     (", length(regional_vars), "):", paste(regional_vars, collapse = ", "), "\n")
cat("Total predictors:", length(all_predictors), "\n")
cat("Outcome:", outcome_var, "\n\n")

# ── Verify ESS columns exist ──
ess_check <- all(attitudinal_vars %in% names(dataset2))
cat("ESS columns present:", ess_check, "\n")
if (!ess_check) stop("ESS columns missing from dataset2_full.rds. Run ESS processing first.")

# ── Missing data summary ──
cat("\n=== Missing data by predictor ===\n")
for (v in all_predictors) {
  n_miss <- sum(is.na(dataset2[[v]]))
  if (n_miss > 0) {
    missing_countries <- dataset2$country[is.na(dataset2[[v]])]
    cat(sprintf("  %-18s %d missing (%s)\n", v, n_miss, paste(missing_countries, collapse = ", ")))
  }
}
cat("\n")


# ── 1. CORRELATION CHECK ─────────────────────────────────────────────────────

cat("=== Correlation matrix (complete cases) ===\n\n")

cor_data <- dataset2 |>
  select(all_of(c(outcome_var, all_predictors))) |>
  filter(complete.cases(pick(everything())))

cat("Complete cases for correlation:", nrow(cor_data), "of 21\n\n")

cor_matrix <- cor(cor_data, use = "complete.obs")
print(round(cor_matrix, 2))

# Flag high correlations among predictors
cat("\n=== High predictor correlations (|r| > 0.7) ===\n")
pred_cor <- cor(cor_data |> select(-all_of(outcome_var)), use = "complete.obs")
high_cor <- which(abs(pred_cor) > 0.7 & upper.tri(pred_cor), arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  for (i in 1:nrow(high_cor)) {
    cat(sprintf("  %s — %s: r = %.2f\n",
                rownames(pred_cor)[high_cor[i, 1]],
                colnames(pred_cor)[high_cor[i, 2]],
                pred_cor[high_cor[i, 1], high_cor[i, 2]]))
  }
} else {
  cat("  None found.\n")
}

# Correlations with outcome
cat("\n=== Correlations with gradient_steepness ===\n")
outcome_cors <- cor_matrix[outcome_var, all_predictors]
outcome_cors_sorted <- sort(abs(outcome_cors), decreasing = TRUE)
for (v in names(outcome_cors_sorted)) {
  group_label <- case_when(
    v %in% attitudinal_vars ~ "[ATT]",
    v %in% institutional_vars ~ "[INST]",
    v %in% economic_vars ~ "[ECON]",
    v %in% regional_vars ~ "[REG]",
    TRUE ~ ""
  )
  cat(sprintf("  %-18s r = %+.3f  (|r| = %.3f) %s\n", v, outcome_cors[v], abs(outcome_cors[v]), group_label))
}

# ── Save correlation plot ──
png("output/figures/h3_correlation_matrix.png", width = 1000, height = 900, res = 120)
cor_plot_data <- as.data.frame(as.table(cor_matrix))
names(cor_plot_data) <- c("Var1", "Var2", "value")
ggplot(cor_plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(value, 2)), size = 2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "H3: Predictor Correlation Matrix (incl. ESS)", fill = "r")
dev.off()
cat("\nSaved: output/figures/h3_correlation_matrix.png\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 1: XGBoost REGRESSION (Feature Discovery — 12 predictors)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  PART 1: XGBoost REGRESSION — 12 predictors\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

# Prepare XGBoost matrix
xgb_data <- dataset2 |> select(all_of(c(outcome_var, all_predictors)))

X <- as.matrix(xgb_data |> select(all_of(all_predictors)))
y <- xgb_data[[outcome_var]]

cat("XGBoost matrix: N =", nrow(X), "| p =", ncol(X), "\n")
cat("Missing values in X:", sum(is.na(X)), "(XGBoost handles natively)\n")
cat("Outcome range:", round(range(y), 3), "\n\n")

# ── 1a. Hyperparameter specification ──
params <- list(
  objective        = "reg:squarederror",
  max_depth        = 2,
  eta              = 0.1,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  lambda           = 5,
  alpha            = 1,
  min_child_weight = 3
)

cat("=== XGBoost hyperparameters ===\n")
for (p in names(params)) cat(sprintf("  %-20s = %s\n", p, params[[p]]))

# ── 1b. Find optimal nrounds via internal CV ──
dtrain <- xgb.DMatrix(data = X, label = y)

set.seed(42)
cv_result <- xgb.cv(
  params  = params,
  data    = dtrain,
  nrounds = 200,
  nfold   = 5,
  verbose = FALSE,
  early_stopping_rounds = 20,
  print_every_n = 50
)

# Robust accessor for best_nrounds (fixes version compatibility)
best_nrounds <- cv_result$best_iteration
if (is.null(best_nrounds) || length(best_nrounds) == 0) {
  best_nrounds <- which.min(cv_result$evaluation_log$test_rmse_mean)
}
best_rmse <- cv_result$evaluation_log$test_rmse_mean[best_nrounds]

cat("\n=== CV results ===\n")
cat("Best nrounds:", best_nrounds, "\n")
cat("Best CV RMSE:", round(best_rmse, 4), "\n")
cat("Outcome SD:", round(sd(y), 4), "(baseline for comparison)\n\n")

# ── 1c. Fit final model on all data ──
set.seed(42)
xgb_model <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = best_nrounds,
  verbose = FALSE
)

# Training performance
y_pred <- predict(xgb_model, dtrain)
train_rmse <- sqrt(mean((y - y_pred)^2))
train_r2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)

cat("=== Training performance ===\n")
cat("Training RMSE:", round(train_rmse, 4), "\n")
cat("Training R²:", round(train_r2, 4), "\n\n")

# ── 1d. Leave-one-out cross-validation ──
cat("=== LOOCV (N = 21) ===\n")
loocv_preds <- numeric(nrow(X))

for (i in 1:nrow(X)) {
  X_train <- X[-i, , drop = FALSE]
  y_train <- y[-i]
  X_test  <- X[i, , drop = FALSE]

  dtrain_loo <- xgb.DMatrix(data = X_train, label = y_train)
  dtest_loo  <- xgb.DMatrix(data = X_test)

  model_loo <- xgb.train(
    params  = params,
    data    = dtrain_loo,
    nrounds = best_nrounds,
    verbose = FALSE
  )

  loocv_preds[i] <- predict(model_loo, dtest_loo)
}

loocv_rmse <- sqrt(mean((y - loocv_preds)^2))
loocv_r2 <- 1 - sum((y - loocv_preds)^2) / sum((y - mean(y))^2)

cat("LOOCV RMSE:", round(loocv_rmse, 4), "\n")
cat("LOOCV R²:", round(loocv_r2, 4), "\n")
cat("Baseline RMSE (mean prediction):", round(sd(y) * sqrt(20/21), 4), "\n\n")

# Save predictions vs actual
loocv_df <- data.frame(
  country = dataset2$country,
  actual = y,
  predicted = loocv_preds,
  residual = y - loocv_preds
)
cat("=== LOOCV predictions by country ===\n")
print(loocv_df |> arrange(desc(abs(residual))) |> mutate(across(where(is.numeric), ~round(., 3))))

# ── 1e. Feature importance (gain) ──
cat("\n=== XGBoost feature importance (gain) ===\n")
importance <- xgb.importance(model = xgb_model)
print(importance)


# ══════════════════════════════════════════════════════════════════════════════
# PART 2: SHAP INTERPRETATION (3-way horse-race)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  PART 2: SHAP INTERPRETATION (3-way comparison)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

# ── 2a. Compute SHAP values ──
shap_values <- predict(xgb_model, dtrain, predcontrib = TRUE)

# Last column is BIAS — remove it
shap_matrix <- shap_values[, 1:ncol(X)]
colnames(shap_matrix) <- colnames(X)

cat("SHAP matrix: ", nrow(shap_matrix), "×", ncol(shap_matrix), "\n\n")

# ── 2b. Global SHAP importance (mean |SHAP|) ──
mean_abs_shap <- colMeans(abs(shap_matrix))
shap_importance <- data.frame(
  feature = names(mean_abs_shap),
  mean_abs_shap = mean_abs_shap
) |>
  arrange(desc(mean_abs_shap)) |>
  mutate(
    group = case_when(
      feature %in% attitudinal_vars ~ "Attitudinal (ESS)",
      feature %in% institutional_vars ~ "Institutional (V-Dem)",
      feature %in% economic_vars ~ "Economic",
      feature %in% regional_vars ~ "Regional"
    ),
    rank = row_number()
  )

cat("=== SHAP Feature Importance (mean |SHAP|) ===\n")
print(shap_importance |> mutate(mean_abs_shap = round(mean_abs_shap, 4)))

# ── 2c. THREE-WAY SHAP comparison ──
attitudinal_total   <- sum(mean_abs_shap[attitudinal_vars])
institutional_total <- sum(mean_abs_shap[institutional_vars])
economic_total      <- sum(mean_abs_shap[economic_vars])
regional_total      <- sum(mean_abs_shap[regional_vars])
total_importance    <- attitudinal_total + institutional_total + economic_total + regional_total

cat("\n=== H3 THREE-WAY HORSE-RACE ===\n")
cat(sprintf("Attitudinal (ESS):     %.4f (%.1f%%)\n", attitudinal_total, 100 * attitudinal_total / total_importance))
cat(sprintf("Institutional (V-Dem): %.4f (%.1f%%)\n", institutional_total, 100 * institutional_total / total_importance))
cat(sprintf("Economic:              %.4f (%.1f%%)\n", economic_total, 100 * economic_total / total_importance))
cat(sprintf("Regional:              %.4f (%.1f%%)\n", regional_total, 100 * regional_total / total_importance))

# Combined cultural = attitudinal + institutional
cultural_combined <- attitudinal_total + institutional_total
cat(sprintf("\nCultural combined:     %.4f (%.1f%%)\n", cultural_combined, 100 * cultural_combined / total_importance))
cat(sprintf("Economic:              %.4f (%.1f%%)\n", economic_total, 100 * economic_total / total_importance))

cat("\n")
if (cultural_combined > economic_total) {
  cat(">>> Cultural indicators (attitudinal + institutional) dominate — supports H3\n")
} else {
  cat(">>> Economic indicators dominate — H3 not supported\n")
}
if (attitudinal_total > institutional_total) {
  cat(">>> Attitudinal (ESS) outperforms Institutional (V-Dem) within cultural block\n")
} else {
  cat(">>> Institutional (V-Dem) outperforms Attitudinal (ESS) within cultural block\n")
}

# ── 2d. SHAP importance bar plot (4-group colours) ──
png("output/figures/h3_shap_importance.png", width = 1000, height = 700, res = 120)
ggplot(shap_importance, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap, fill = group)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Attitudinal (ESS)" = "#E66101",
    "Institutional (V-Dem)" = "#2166AC",
    "Economic" = "#B2182B",
    "Regional" = "#4DAF4A"
  )) +
  theme_minimal(base_size = 12) +
  labs(
    title = "H3: SHAP Feature Importance (12 predictors)",
    subtitle = "Mean |SHAP value| — Attitudinal vs Institutional vs Economic",
    x = NULL, y = "Mean |SHAP value|",
    fill = "Predictor Group"
  ) +
  theme(legend.position = "bottom")
dev.off()
cat("\nSaved: output/figures/h3_shap_importance.png\n")

# ── 2e. SHAP beeswarm plot ──
shap_long <- as.data.frame(shap_matrix) |>
  mutate(country = dataset2$country) |>
  pivot_longer(-country, names_to = "feature", values_to = "shap_value") |>
  left_join(
    dataset2 |> select(country, all_of(all_predictors)) |>
      pivot_longer(-country, names_to = "feature", values_to = "feature_value"),
    by = c("country", "feature")
  )

shap_long <- shap_long |>
  left_join(shap_importance |> select(feature, rank), by = "feature") |>
  mutate(feature = reorder(feature, -rank))

png("output/figures/h3_shap_beeswarm.png", width = 1000, height = 700, res = 120)
ggplot(shap_long, aes(x = shap_value, y = feature)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = feature_value), size = 2.5, alpha = 0.8) +
  scale_colour_gradient2(low = "#2166AC", mid = "#FFFFBF", high = "#D53E4F",
                         midpoint = 0.5, na.value = "grey50",
                         name = "Feature\nvalue\n(scaled)") +
  theme_minimal(base_size = 12) +
  labs(
    title = "H3: SHAP Value Distribution (12 predictors)",
    subtitle = "Each dot = one country; colour = feature value (scaled 0-1)",
    x = "SHAP value (impact on gradient steepness prediction)",
    y = NULL
  )
dev.off()
cat("Saved: output/figures/h3_shap_beeswarm.png\n")

# ── 2f. SHAP dependence plots for top 4 features ──
top_features <- shap_importance$feature[1:min(4, nrow(shap_importance))]

png("output/figures/h3_shap_dependence.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (feat in top_features) {
  feat_vals <- X[, feat]
  feat_shap <- shap_matrix[, feat]

  plot(feat_vals, feat_shap,
       pch = 19, col = "#2166AC", cex = 1.2,
       xlab = feat, ylab = "SHAP value",
       main = paste("SHAP dependence:", feat))
  abline(h = 0, lty = 2, col = "grey50")

  extreme_idx <- which(abs(feat_shap) > quantile(abs(feat_shap), 0.75))
  if (length(extreme_idx) > 0) {
    text(feat_vals[extreme_idx], feat_shap[extreme_idx],
         labels = dataset2$country[extreme_idx],
         pos = 3, cex = 0.6, col = "grey30")
  }
}
dev.off()
cat("Saved: output/figures/h3_shap_dependence.png\n")

# ── Save XGBoost results ──
saveRDS(list(
  model = xgb_model,
  params = params,
  best_nrounds = best_nrounds,
  cv_result = cv_result,
  loocv_preds = loocv_preds,
  loocv_rmse = loocv_rmse,
  loocv_r2 = loocv_r2,
  importance = importance,
  shap_values = shap_matrix,
  mean_abs_shap = mean_abs_shap,
  attitudinal_share = attitudinal_total / total_importance,
  institutional_share = institutional_total / total_importance,
  economic_share = economic_total / total_importance,
  regional_share = regional_total / total_importance
), "data/models/h3_xgboost.rds")
cat("\nSaved: data/models/h3_xgboost.rds\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 3: BAYESIAN LINEAR MODELS (6 models, confirmatory inference)
# ══════════════════════════════════════════════════════════════════════════════
#
# Strategy:
#   - Find common complete cases FIRST, then fit all models on same N
#   - Standardise predictors (z-scores) for comparable coefficients
#   - Regularising Student-t(3,0,1) priors
#   - 6 models: institutional, economic, post-socialist, combined,
#     attitudinal (NEW), updated combined (NEW)
#   - WAIC + LOO for model comparison
#   - Convergence diagnostics for all models
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  PART 3: BAYESIAN LINEAR MODELS (6 models)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

# ── 3a. Prepare standardised data on COMMON complete cases ──
# Variables used across ALL 6 models:
#   gender_equal, relig_free, gdp_percap, flfp, post_socialist,
#   ess_secular, ess_gender_egal
# Exclude contraceptive (43% missing) and ess_autonomy (only in XGBoost)

bayes_vars <- c("gradient_steepness",
                "gender_equal", "relig_free",
                "gdp_percap", "flfp",
                "ess_secular", "ess_gender_egal",
                "post_socialist")

bayes_data <- dataset2 |>
  select(country, all_of(bayes_vars))

# Find complete cases across ALL variables
bayes_complete <- bayes_data |>
  filter(complete.cases(pick(everything())))

cat("Complete cases for Bayesian models:", nrow(bayes_complete), "of 21\n")
cat("Dropped:", paste(setdiff(bayes_data$country, bayes_complete$country), collapse = ", "), "\n\n")

# Standardise continuous predictors on the complete-case subset
bayes_complete <- bayes_complete |>
  mutate(across(c(gender_equal, relig_free, gdp_percap, flfp,
                  ess_secular, ess_gender_egal),
                ~as.numeric(scale(.)),
                .names = "{.col}_z"))

cat("=== Bayesian data (z-scored, N =", nrow(bayes_complete), ") ===\n")
print(bayes_complete |> select(country, gradient_steepness, ends_with("_z"), post_socialist) |>
        mutate(across(where(is.numeric), ~round(., 2))))

# ── 3b. Define priors ──
reg_prior <- c(
  prior(student_t(3, 0, 1), class = "b"),
  prior(student_t(3, 0, 1), class = "Intercept"),
  prior(student_t(3, 0, 1), class = "sigma")
)

cat("\n=== Priors: Student-t(3, 0, 1) on all parameters ===\n\n")

# ── 3c. Model 1: Institutional (V-Dem) ──
cat("--- Fitting M1: Institutional (gender_equal + relig_free) ---\n")
m1_institutional <- brm(
  gradient_steepness ~ gender_equal_z + relig_free_z,
  data = bayes_complete, family = gaussian(), prior = reg_prior,
  chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0
)
cat("M1 fitted.\n")
print(summary(m1_institutional))

# ── 3d. Model 2: Economic ──
cat("\n--- Fitting M2: Economic (gdp_percap + flfp) ---\n")
m2_economic <- brm(
  gradient_steepness ~ gdp_percap_z + flfp_z,
  data = bayes_complete, family = gaussian(), prior = reg_prior,
  chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0
)
cat("M2 fitted.\n")
print(summary(m2_economic))

# ── 3e. Model 3: Post-socialist ──
cat("\n--- Fitting M3: Post-socialist ---\n")
m3_postsoc <- brm(
  gradient_steepness ~ post_socialist,
  data = bayes_complete, family = gaussian(), prior = reg_prior,
  chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0
)
cat("M3 fitted.\n")
print(summary(m3_postsoc))

# ── 3f. Model 4: Combined (original — institutional + economic + regional) ──
cat("\n--- Fitting M4: Combined (gender_equal + flfp + post_socialist) ---\n")
m4_combined <- brm(
  gradient_steepness ~ gender_equal_z + flfp_z + post_socialist,
  data = bayes_complete, family = gaussian(), prior = reg_prior,
  chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0
)
cat("M4 fitted.\n")
print(summary(m4_combined))

# ── 3g. Model 5: Attitudinal (ESS) — NEW ──
cat("\n--- Fitting M5: Attitudinal (ess_secular + ess_gender_egal) ---\n")
m5_attitudinal <- brm(
  gradient_steepness ~ ess_secular_z + ess_gender_egal_z,
  data = bayes_complete, family = gaussian(), prior = reg_prior,
  chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0
)
cat("M5 fitted.\n")
print(summary(m5_attitudinal))

# ── 3h. Model 6: Updated combined (best ESS + best economic + regional) — NEW ──
cat("\n--- Fitting M6: Updated combined (ess_secular + gdp_percap + post_socialist) ---\n")
m6_combined_new <- brm(
  gradient_steepness ~ ess_secular_z + gdp_percap_z + post_socialist,
  data = bayes_complete, family = gaussian(), prior = reg_prior,
  chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0
)
cat("M6 fitted.\n")
print(summary(m6_combined_new))


# ── 3i. CONVERGENCE DIAGNOSTICS ──
cat("\n=== CONVERGENCE DIAGNOSTICS ===\n\n")

all_models <- list(
  M1_Institutional = m1_institutional,
  M2_Economic = m2_economic,
  M3_PostSocialist = m3_postsoc,
  M4_Combined = m4_combined,
  M5_Attitudinal = m5_attitudinal,
  M6_CombinedNew = m6_combined_new
)

for (mname in names(all_models)) {
  rhat_vals <- brms::rhat(all_models[[mname]])
  neff_vals <- brms::neff_ratio(all_models[[mname]])
  cat(sprintf("  %-20s Max Rhat = %.4f | Min neff_ratio = %.4f",
              mname, max(rhat_vals, na.rm = TRUE), min(neff_vals, na.rm = TRUE)))

  if (max(rhat_vals, na.rm = TRUE) < 1.01 & min(neff_vals, na.rm = TRUE) > 0.1) {
    cat(" OK\n")
  } else {
    cat(" CHECK\n")
  }
}


# ── 3j. Model comparison (WAIC and LOO) ──
cat("\n=== MODEL COMPARISON (all 6 models, same N =", nrow(bayes_complete), ") ===\n\n")

m1_institutional <- add_criterion(m1_institutional, c("waic", "loo"))
m2_economic      <- add_criterion(m2_economic, c("waic", "loo"))
m3_postsoc       <- add_criterion(m3_postsoc, c("waic", "loo"))
m4_combined      <- add_criterion(m4_combined, c("waic", "loo"))
m5_attitudinal   <- add_criterion(m5_attitudinal, c("waic", "loo"))
m6_combined_new  <- add_criterion(m6_combined_new, c("waic", "loo"))

cat("--- WAIC comparison ---\n")
waic_comp <- loo_compare(m1_institutional, m2_economic, m3_postsoc,
                         m4_combined, m5_attitudinal, m6_combined_new,
                         criterion = "waic")
print(waic_comp)

cat("\n--- LOO comparison ---\n")
loo_comp <- loo_compare(m1_institutional, m2_economic, m3_postsoc,
                        m4_combined, m5_attitudinal, m6_combined_new,
                        criterion = "loo")
print(loo_comp)


# ── 3k. Posterior summaries ──
cat("\n=== POSTERIOR SUMMARIES ===\n\n")

post_m1 <- as_draws_df(m1_institutional)
post_m2 <- as_draws_df(m2_economic)
post_m5 <- as_draws_df(m5_attitudinal)
post_m6 <- as_draws_df(m6_combined_new)

cat("--- M1: Institutional ---\n")
cat("gender_equal_z: median =", round(median(post_m1$b_gender_equal_z), 3),
    " | 95% CI [", round(quantile(post_m1$b_gender_equal_z, 0.025), 3), ",",
    round(quantile(post_m1$b_gender_equal_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m1$b_gender_equal_z < 0), 3), "\n")
cat("relig_free_z:   median =", round(median(post_m1$b_relig_free_z), 3),
    " | 95% CI [", round(quantile(post_m1$b_relig_free_z, 0.025), 3), ",",
    round(quantile(post_m1$b_relig_free_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m1$b_relig_free_z < 0), 3), "\n")

cat("\n--- M2: Economic ---\n")
cat("gdp_percap_z: median =", round(median(post_m2$b_gdp_percap_z), 3),
    " | 95% CI [", round(quantile(post_m2$b_gdp_percap_z, 0.025), 3), ",",
    round(quantile(post_m2$b_gdp_percap_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m2$b_gdp_percap_z < 0), 3), "\n")
cat("flfp_z:       median =", round(median(post_m2$b_flfp_z), 3),
    " | 95% CI [", round(quantile(post_m2$b_flfp_z, 0.025), 3), ",",
    round(quantile(post_m2$b_flfp_z, 0.975), 3), "]",
    " | P(> 0) =", round(mean(post_m2$b_flfp_z > 0), 3), "\n")

cat("\n--- M5: Attitudinal (NEW) ---\n")
cat("ess_secular_z:     median =", round(median(post_m5$b_ess_secular_z), 3),
    " | 95% CI [", round(quantile(post_m5$b_ess_secular_z, 0.025), 3), ",",
    round(quantile(post_m5$b_ess_secular_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m5$b_ess_secular_z < 0), 3), "\n")
cat("ess_gender_egal_z: median =", round(median(post_m5$b_ess_gender_egal_z), 3),
    " | 95% CI [", round(quantile(post_m5$b_ess_gender_egal_z, 0.025), 3), ",",
    round(quantile(post_m5$b_ess_gender_egal_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m5$b_ess_gender_egal_z < 0), 3), "\n")

cat("\n--- M6: Updated Combined (NEW) ---\n")
cat("ess_secular_z: median =", round(median(post_m6$b_ess_secular_z), 3),
    " | 95% CI [", round(quantile(post_m6$b_ess_secular_z, 0.025), 3), ",",
    round(quantile(post_m6$b_ess_secular_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m6$b_ess_secular_z < 0), 3), "\n")
cat("gdp_percap_z:  median =", round(median(post_m6$b_gdp_percap_z), 3),
    " | 95% CI [", round(quantile(post_m6$b_gdp_percap_z, 0.025), 3), ",",
    round(quantile(post_m6$b_gdp_percap_z, 0.975), 3), "]",
    " | P(< 0) =", round(mean(post_m6$b_gdp_percap_z < 0), 3), "\n")
cat("post_socialist: median =", round(median(post_m6$b_post_socialist), 3),
    " | 95% CI [", round(quantile(post_m6$b_post_socialist, 0.025), 3), ",",
    round(quantile(post_m6$b_post_socialist, 0.975), 3), "]",
    " | P(> 0) =", round(mean(post_m6$b_post_socialist > 0), 3), "\n")


# ── 3l. Posterior plots ──
png("output/figures/h3_bayesian_posteriors.png", width = 1100, height = 800, res = 120)

posterior_plot_data <- bind_rows(
  data.frame(model = "M1: Institutional (V-Dem)", param = "gender_equal",
             value = post_m1$b_gender_equal_z, group = "Institutional"),
  data.frame(model = "M1: Institutional (V-Dem)", param = "relig_free",
             value = post_m1$b_relig_free_z, group = "Institutional"),
  data.frame(model = "M2: Economic", param = "gdp_percap",
             value = post_m2$b_gdp_percap_z, group = "Economic"),
  data.frame(model = "M2: Economic", param = "flfp",
             value = post_m2$b_flfp_z, group = "Economic"),
  data.frame(model = "M5: Attitudinal (ESS)", param = "ess_secular",
             value = post_m5$b_ess_secular_z, group = "Attitudinal"),
  data.frame(model = "M5: Attitudinal (ESS)", param = "ess_gender_egal",
             value = post_m5$b_ess_gender_egal_z, group = "Attitudinal")
)

# Order facets meaningfully
posterior_plot_data$model <- factor(posterior_plot_data$model,
  levels = c("M1: Institutional (V-Dem)", "M5: Attitudinal (ESS)", "M2: Economic"))

ggplot(posterior_plot_data, aes(x = value, y = param, fill = group)) +
  stat_halfeye(.width = c(0.66, 0.95), alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = c(
    "Institutional" = "#2166AC",
    "Attitudinal" = "#E66101",
    "Economic" = "#B2182B"
  )) +
  facet_wrap(~model, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 12) +
  labs(
    title = "H3: Bayesian Posterior Distributions (3 Domain Models)",
    subtitle = "Effect on gradient steepness (standardised coefficients)",
    x = "Posterior estimate (standardised)",
    y = NULL, fill = "Domain"
  ) +
  theme(legend.position = "bottom")
dev.off()
cat("\nSaved: output/figures/h3_bayesian_posteriors.png\n")


# ── Save all Bayesian models ──
saveRDS(list(
  m1_institutional = m1_institutional,
  m2_economic = m2_economic,
  m3_postsoc = m3_postsoc,
  m4_combined = m4_combined,
  m5_attitudinal = m5_attitudinal,
  m6_combined_new = m6_combined_new,
  waic_comp = waic_comp,
  loo_comp = loo_comp,
  bayes_data = bayes_complete
), "data/models/h3_bayesian.rds")
cat("Saved: data/models/h3_bayesian.rds\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 4: SUMMARY AND H3 VERDICT
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  H3 SUMMARY (UPDATED WITH ESS)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

cat("RESEARCH QUESTION: Is cross-country variation in the education-fertility\n")
cat("gradient predicted better by cultural indicators than by economic indicators?\n\n")

cat("--- STAGE 1: XGBoost + SHAP (Exploratory, 12 predictors) ---\n")
cat("LOOCV R²:", round(loocv_r2, 3), "\n")
cat("Top predictor:", shap_importance$feature[1], "\n")
cat(sprintf("Attitudinal (ESS):     %.1f%%\n", 100 * attitudinal_total / total_importance))
cat(sprintf("Institutional (V-Dem): %.1f%%\n", 100 * institutional_total / total_importance))
cat(sprintf("Economic:              %.1f%%\n", 100 * economic_total / total_importance))
cat(sprintf("Regional:              %.1f%%\n", 100 * regional_total / total_importance))
cat(sprintf("Cultural combined:     %.1f%% vs Economic: %.1f%%\n\n",
            100 * cultural_combined / total_importance,
            100 * economic_total / total_importance))

cat("--- STAGE 2: Bayesian (Confirmatory, 6 models, N =", nrow(bayes_complete), ") ---\n")
cat("Best model (WAIC):", rownames(waic_comp)[1], "\n")
cat("Best model (LOO):", rownames(loo_comp)[1], "\n")
cat("Does M5 (attitudinal) beat M1 (institutional)?",
    ifelse(which(rownames(waic_comp) == "m5_attitudinal") < which(rownames(waic_comp) == "m1_institutional"),
           "YES", "NO"), "(WAIC)\n")

cat("\n══════════════════════════════════════════════════════════════════════\n")

# ── Methodology log ──
cat("
====================================================================
METHODOLOGY LOG — H3 ENTRIES (UPDATED)
====================================================================

36. H3 Stage 1: XGBoost + SHAP (UPDATED). Gradient-boosted regression of
    gradient_steepness on 12 predictors: 3 attitudinal (ESS), 5 institutional
    (V-Dem), 3 economic (OECD/WB), 1 regional. Conservative hyperparameters
    for N=21 (max_depth=2, lambda=5, alpha=1). LOOCV for honest performance.
    SHAP values via xgboost native predcontrib. Three-way SHAP comparison:
    attitudinal vs institutional vs economic aggregate mean |SHAP|.

37. H3 Stage 2: Bayesian linear models via brms (UPDATED). Six models:
    (1) Institutional (V-Dem): gender_equal + relig_free
    (2) Economic: gdp_percap + flfp
    (3) Post-socialist only
    (4) Combined (original): gender_equal + flfp + post_socialist
    (5) Attitudinal (ESS): ess_secular + ess_gender_egal  [NEW]
    (6) Updated combined: ess_secular + gdp_percap + post_socialist  [NEW]
    All on common complete-case subset. Student-t(3,0,1) priors.
    WAIC and LOO-CV for 6-model comparison. Convergence verified via
    R-hat and neff_ratio for all models.

====================================================================
")

cat("\nDone. H3 analysis complete (updated with ESS).\n")