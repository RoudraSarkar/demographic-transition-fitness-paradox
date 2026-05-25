# ============================================================================
# 07_h3_models.R
# H3 ANALYSIS: Cultural vs Economic Explanations of SES-Fertility Gradient
# MSc Applied Social Data Science, TCD
#
# Tests whether cross-country variation in the education-fertility gradient
# is better predicted by cultural/institutional indicators (V-Dem) or
# economic indicators (GDP, FLFP, contraceptive prevalence).
#
# Three-part approach:
#   PART 1: XGBoost regression + LOOCV (feature discovery)
#   PART 2: SHAP interpretation (variable importance & cultural vs economic)
#   PART 3: Bayesian linear models (confirmatory inference with uncertainty)
#
# Input:  data/derived/dataset2_full.rds (21 countries × 27 variables)
# Output: data/models/h3_xgboost.rds
#         data/models/h3_bayesian.rds
#         output/figures/h3_*.png
# ============================================================================
install.packages("xgboost")
install.packages("brms")
install.packages("tidybayes")
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xgboost)
library(brms)

setwd("/Users/whiz/Desktop/dissertation")

# ── 0. LOAD DATA AND PREPARE ─────────────────────────────────────────────────

dataset2 <- readRDS("data/derived/dataset2_full.rds")

cat("=== Dataset 2 loaded ===\n")
cat("Dimensions:", nrow(dataset2), "×", ncol(dataset2), "\n\n")

# Convert any NaN to NA (from contraceptive rowwise mean)
dataset2 <- dataset2 |>
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# Define predictor groups for H3 horse-race
cultural_vars <- c("libdem", "gender_equal", "relig_free", "polyarchy", "egaldem")
economic_vars <- c("gdp_percap", "flfp", "contraceptive")
regional_vars <- c("post_socialist")

all_predictors <- c(cultural_vars, economic_vars, regional_vars)

# Outcome: gradient_steepness (eTFR_low - eTFR_high)
outcome_var <- "gradient_steepness"

cat("=== Predictor groups ===\n")
cat("Cultural (", length(cultural_vars), "):", paste(cultural_vars, collapse = ", "), "\n")
cat("Economic (", length(economic_vars), "):", paste(economic_vars, collapse = ", "), "\n")
cat("Regional (", length(regional_vars), "):", paste(regional_vars, collapse = ", "), "\n")
cat("Outcome:", outcome_var, "\n\n")

# ── 1. CORRELATION CHECK ─────────────────────────────────────────────────────
# V-Dem indices are likely highly correlated — check before modelling

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
  cat(sprintf("  %-15s r = %+.3f  (|r| = %.3f)\n", v, outcome_cors[v], abs(outcome_cors[v])))
}

# ── Save correlation plot ──
png("output/figures/h3_correlation_matrix.png", width = 800, height = 700, res = 120)
cor_plot_data <- as.data.frame(as.table(cor_matrix))
names(cor_plot_data) <- c("Var1", "Var2", "value")
ggplot(cor_plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "H3: Predictor Correlation Matrix", fill = "r")
dev.off()
cat("\nSaved: output/figures/h3_correlation_matrix.png\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 1: XGBoost REGRESSION (Feature Discovery)
# ══════════════════════════════════════════════════════════════════════════════
#
# N = 21 is very small for tree-based methods. Strategy:
#   - Ultra-conservative hyperparameters (shallow trees, high regularisation)
#   - Leave-one-out cross-validation (LOOCV) for honest performance
#   - Focus on SHAP interpretation, NOT predictive accuracy
#   - XGBoost handles missing values natively (important for contraceptive)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  PART 1: XGBoost REGRESSION — gradient_steepness\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

# Prepare XGBoost matrix
xgb_data <- dataset2 |> select(all_of(c(outcome_var, all_predictors)))

X <- as.matrix(xgb_data |> select(all_of(all_predictors)))
y <- xgb_data[[outcome_var]]

cat("XGBoost matrix: N =", nrow(X), "| p =", ncol(X), "\n")
cat("Missing values in X:", sum(is.na(X)), "(XGBoost handles natively)\n")
cat("Outcome range:", round(range(y), 3), "\n\n")

# ── 1a. Hyperparameter specification ──
# Conservative for N=21: stumps (depth=1-2), few rounds, high regularisation
params <- list(
  objective    = "reg:squarederror",
  max_depth    = 2,          # Very shallow trees (stumps or near-stumps)
  eta          = 0.1,        # Low learning rate
  subsample    = 0.8,        # Row subsampling
  colsample_bytree = 0.8,    # Column subsampling
  lambda       = 5,          # Strong L2 regularisation
  alpha        = 1,          # Some L1 regularisation
  min_child_weight = 3       # Minimum observations per leaf
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
  nfold   = 5,         # 5-fold CV (each fold ~4 countries)
  verbose = FALSE,
  early_stopping_rounds = 20,
  print_every_n = 50
)

best_nrounds <- cv_result$best_iteration
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

# ── 1d. Leave-one-out cross-validation (honest performance) ──
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

# ── 1e. XGBoost built-in feature importance ──
cat("\n=== XGBoost feature importance (gain) ===\n")
importance <- xgb.importance(model = xgb_model)
print(importance)

# ── Save XGBoost model ──
saveRDS(list(
  model = xgb_model,
  params = params,
  best_nrounds = best_nrounds,
  cv_result = cv_result,
  loocv_preds = loocv_preds,
  loocv_rmse = loocv_rmse,
  loocv_r2 = loocv_r2,
  importance = importance
), "data/models/h3_xgboost.rds")
cat("\nSaved: data/models/h3_xgboost.rds\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 2: SHAP INTERPRETATION
# ══════════════════════════════════════════════════════════════════════════════
#
# SHAP (SHapley Additive exPlanations) values decompose each prediction
# into feature contributions. Key outputs:
#   - Global importance ranking (mean |SHAP|)
#   - Cultural vs Economic aggregate importance
#   - SHAP dependence plots for top features
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  PART 2: SHAP INTERPRETATION\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

# ── 2a. Compute SHAP values ──
# xgboost native SHAP via predcontrib
shap_values <- predict(xgb_model, dtrain, predcontrib = TRUE)

# Last column is BIAS — remove it
shap_matrix <- shap_values[, 1:ncol(X)]
colnames(shap_matrix) <- colnames(X)

cat("=== SHAP matrix dimensions ===\n")
cat("Rows:", nrow(shap_matrix), "| Columns:", ncol(shap_matrix), "\n\n")

# ── 2b. Global SHAP importance (mean |SHAP|) ──
mean_abs_shap <- colMeans(abs(shap_matrix))
shap_importance <- data.frame(
  feature = names(mean_abs_shap),
  mean_abs_shap = mean_abs_shap
) |>
  arrange(desc(mean_abs_shap)) |>
  mutate(
    group = case_when(
      feature %in% cultural_vars ~ "Cultural",
      feature %in% economic_vars ~ "Economic",
      feature %in% regional_vars ~ "Regional"
    ),
    rank = row_number()
  )

cat("=== SHAP Feature Importance (mean |SHAP|) ===\n")
print(shap_importance |> mutate(mean_abs_shap = round(mean_abs_shap, 4)))

# ── 2c. Cultural vs Economic aggregate importance ──
cultural_total <- sum(shap_importance$mean_abs_shap[shap_importance$group == "Cultural"])
economic_total <- sum(shap_importance$mean_abs_shap[shap_importance$group == "Economic"])
regional_total <- sum(shap_importance$mean_abs_shap[shap_importance$group == "Regional"])
total_importance <- cultural_total + economic_total + regional_total

cat("\n=== H3 HORSE-RACE: Cultural vs Economic ===\n")
cat(sprintf("Cultural importance:  %.4f (%.1f%%)\n", cultural_total, 100 * cultural_total / total_importance))
cat(sprintf("Economic importance:  %.4f (%.1f%%)\n", economic_total, 100 * economic_total / total_importance))
cat(sprintf("Regional importance:  %.4f (%.1f%%)\n", regional_total, 100 * regional_total / total_importance))
cat("\n")

if (cultural_total > economic_total) {
  cat(">>> Cultural indicators dominate — supports H3 (cultural transmission)\n")
} else {
  cat(">>> Economic indicators dominate — H3 not supported\n")
}

# ── 2d. SHAP importance bar plot ──
png("output/figures/h3_shap_importance.png", width = 900, height = 600, res = 120)
ggplot(shap_importance, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap, fill = group)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Cultural" = "#2166AC", "Economic" = "#B2182B", "Regional" = "#4DAF4A")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "H3: SHAP Feature Importance",
    subtitle = "Mean |SHAP value| — which factors predict gradient steepness?",
    x = NULL, y = "Mean |SHAP value|",
    fill = "Predictor Group"
  ) +
  theme(legend.position = "bottom")
dev.off()
cat("\nSaved: output/figures/h3_shap_importance.png\n")

# ── 2e. SHAP beeswarm/summary plot (manual) ──
# Shows distribution of SHAP values for each feature
shap_long <- as.data.frame(shap_matrix) |>
  mutate(country = dataset2$country) |>
  pivot_longer(-country, names_to = "feature", values_to = "shap_value") |>
  left_join(
    dataset2 |> select(country, all_of(all_predictors)) |>
      pivot_longer(-country, names_to = "feature", values_to = "feature_value"),
    by = c("country", "feature")
  )

# Order features by importance
shap_long <- shap_long |>
  left_join(shap_importance |> select(feature, rank), by = "feature") |>
  mutate(feature = reorder(feature, -rank))

png("output/figures/h3_shap_beeswarm.png", width = 900, height = 600, res = 120)
ggplot(shap_long, aes(x = shap_value, y = feature)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = feature_value), size = 2.5, alpha = 0.8) +
  scale_colour_gradient2(low = "#2166AC", mid = "#FFFFBF", high = "#D53E4F",
                         midpoint = 0.5, na.value = "grey50",
                         name = "Feature\nvalue\n(scaled)") +
  theme_minimal(base_size = 12) +
  labs(
    title = "H3: SHAP Value Distribution",
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
  
  # Add country labels for extreme points
  extreme_idx <- which(abs(feat_shap) > quantile(abs(feat_shap), 0.75))
  if (length(extreme_idx) > 0) {
    text(feat_vals[extreme_idx], feat_shap[extreme_idx],
         labels = dataset2$country[extreme_idx],
         pos = 3, cex = 0.6, col = "grey30")
  }
}
dev.off()
cat("Saved: output/figures/h3_shap_dependence.png\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 3: BAYESIAN LINEAR MODELS (Confirmatory Inference)
# ══════════════════════════════════════════════════════════════════════════════
#
# Uses brms (Bayesian Regression Models using Stan) to estimate effects
# with proper uncertainty quantification. Strategy:
#   - Standardise predictors (z-scores) for comparable coefficients
#   - Regularising Student-t priors to prevent overfitting with N=21
#   - Compare cultural-only vs economic-only vs combined models
#   - WAIC and LOO-CV for model comparison
#   - Select predictors informed by SHAP (Stage 1 → Stage 2 pipeline)
#
# Key constraint: With N=21, models must be VERY parsimonious (2-3 predictors max)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  PART 3: BAYESIAN LINEAR MODELS\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

# ── 3a. Prepare standardised data ──
# Drop contraceptive (43% missing) — use GDP and FLFP for economic
# Select best cultural predictor(s) from SHAP results

# Standardise continuous predictors
bayes_data <- dataset2 |>
  select(country, gradient_steepness, 
         gender_equal, relig_free, libdem, polyarchy, egaldem,
         gdp_percap, flfp,
         post_socialist) |>
  mutate(across(c(gender_equal, relig_free, libdem, polyarchy, egaldem,
                  gdp_percap, flfp),
                ~as.numeric(scale(.)),
                .names = "{.col}_z"))

cat("=== Bayesian data prepared ===\n")
cat("N (complete cases, excl. contraceptive):",
    sum(complete.cases(bayes_data |> select(gradient_steepness, gender_equal_z, gdp_percap_z, flfp_z))),
    "\n\n")

# Check which rows are complete for the main models
complete_idx <- complete.cases(bayes_data |> 
  select(gradient_steepness, gender_equal_z, gdp_percap_z, flfp_z))
cat("Countries with complete data:", sum(complete_idx), "\n")
cat("Missing countries:", paste(bayes_data$country[!complete_idx], collapse = ", "), "\n\n")

# ── 3b. Define priors ──
# Regularising Student-t(3, 0, 1) — allows moderate effects, penalises extremes
# For N=21, regularisation is essential to avoid overfitting
reg_prior <- c(
  prior(student_t(3, 0, 1), class = "b"),       # Coefficients
  prior(student_t(3, 0, 1), class = "Intercept"),# Intercept
  prior(student_t(3, 0, 1), class = "sigma")     # Residual SD
)

cat("=== Priors: Student-t(3, 0, 1) on all parameters ===\n\n")

# ── 3c. Model 1: Cultural-only ──
# Use gender_equal as primary cultural predictor (most directly relevant to
# fertility theory; less correlated with other V-Dem indices than polyarchy/libdem)
cat("--- Fitting Model 1: Cultural-only (gender_equal + relig_free) ---\n")

m1_cultural <- brm(
  gradient_steepness ~ gender_equal_z + relig_free_z,
  data = bayes_data,
  family = gaussian(),
  prior = reg_prior,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 42,
  silent = 2,
  refresh = 0
)

cat("Model 1 fitted.\n")
print(summary(m1_cultural))

# ── 3d. Model 2: Economic-only ──
cat("\n--- Fitting Model 2: Economic-only (gdp_percap + flfp) ---\n")

m2_economic <- brm(
  gradient_steepness ~ gdp_percap_z + flfp_z,
  data = bayes_data,
  family = gaussian(),
  prior = reg_prior,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 42,
  silent = 2,
  refresh = 0
)

cat("Model 2 fitted.\n")
print(summary(m2_economic))

# ── 3e. Model 3: Post-socialist ──
cat("\n--- Fitting Model 3: Post-socialist ---\n")

m3_postsoc <- brm(
  gradient_steepness ~ post_socialist,
  data = bayes_data,
  family = gaussian(),
  prior = reg_prior,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 42,
  silent = 2,
  refresh = 0
)

cat("Model 3 fitted.\n")
print(summary(m3_postsoc))

# ── 3f. Model 4: Combined (SHAP-informed parsimonious) ──
# Combines best cultural + best economic predictor
cat("\n--- Fitting Model 4: Combined (gender_equal + flfp + post_socialist) ---\n")

m4_combined <- brm(
  gradient_steepness ~ gender_equal_z + flfp_z + post_socialist,
  data = bayes_data,
  family = gaussian(),
  prior = reg_prior,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 42,
  silent = 2,
  refresh = 0
)

cat("Model 4 fitted.\n")
print(summary(m4_combined))

# Find complete cases across ALL variables used in any model
bayes_complete <- bayes_data |>
  filter(complete.cases(pick(gradient_steepness, gender_equal_z, relig_free_z,
                             gdp_percap_z, flfp_z, post_socialist)))

cat("Complete cases for all models:", nrow(bayes_complete), "\n")
cat("Dropped:", paste(setdiff(bayes_data$country, bayes_complete$country), collapse = ", "), "\n")

# Refit ALL 4 models on the same subset
cat("\nRefitting M1: Cultural-only...\n")
m1_cultural <- brm(gradient_steepness ~ gender_equal_z + relig_free_z,
                    data = bayes_complete, family = gaussian(), prior = reg_prior,
                    chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0)

cat("Refitting M2: Economic-only...\n")
m2_economic <- brm(gradient_steepness ~ gdp_percap_z + flfp_z,
                    data = bayes_complete, family = gaussian(), prior = reg_prior,
                    chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0)

cat("Refitting M3: Post-socialist...\n")
m3_postsoc <- brm(gradient_steepness ~ post_socialist,
                   data = bayes_complete, family = gaussian(), prior = reg_prior,
                   chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0)

cat("Refitting M4: Combined...\n")
m4_combined <- brm(gradient_steepness ~ gender_equal_z + flfp_z + post_socialist,
                    data = bayes_complete, family = gaussian(), prior = reg_prior,
                    chains = 4, iter = 4000, warmup = 2000, seed = 42, silent = 2, refresh = 0)

cat("All models refitted on N =", nrow(bayes_complete), "\n")

# ── 3g. Model comparison (WAIC and LOO-CV) ──
cat("\n=== MODEL COMPARISON ===\n\n")

# Add WAIC criterion
m1_cultural <- add_criterion(m1_cultural, "waic")
m2_economic <- add_criterion(m2_economic, "waic")
m3_postsoc  <- add_criterion(m3_postsoc, "waic")
m4_combined <- add_criterion(m4_combined, "waic")

# Add LOO criterion
m1_cultural <- add_criterion(m1_cultural, "loo")
m2_economic <- add_criterion(m2_economic, "loo")
m3_postsoc  <- add_criterion(m3_postsoc, "loo")
m4_combined <- add_criterion(m4_combined, "loo")

cat("--- WAIC comparison ---\n")
waic_comp <- loo_compare(m1_cultural, m2_economic, m3_postsoc, m4_combined, criterion = "waic")
print(waic_comp)

cat("\n--- LOO comparison ---\n")
loo_comp <- loo_compare(m1_cultural, m2_economic, m3_postsoc, m4_combined, criterion = "loo")
print(loo_comp)

# ── 3h. Extract and compare posteriors ──
cat("\n=== POSTERIOR SUMMARIES ===\n\n")

# Extract posterior draws for key parameters
post_m1 <- as_draws_df(m1_cultural)
post_m2 <- as_draws_df(m2_economic)
post_m4 <- as_draws_df(m4_combined)

cat("--- Cultural model (M1) ---\n")
cat("gender_equal_z: median =", round(median(post_m1$b_gender_equal_z), 3),
    " | 95% CI [", round(quantile(post_m1$b_gender_equal_z, 0.025), 3), ",",
    round(quantile(post_m1$b_gender_equal_z, 0.975), 3), "]\n")
cat("relig_free_z:   median =", round(median(post_m1$b_relig_free_z), 3),
    " | 95% CI [", round(quantile(post_m1$b_relig_free_z, 0.025), 3), ",",
    round(quantile(post_m1$b_relig_free_z, 0.975), 3), "]\n")

cat("\n--- Economic model (M2) ---\n")
cat("gdp_percap_z: median =", round(median(post_m2$b_gdp_percap_z), 3),
    " | 95% CI [", round(quantile(post_m2$b_gdp_percap_z, 0.025), 3), ",",
    round(quantile(post_m2$b_gdp_percap_z, 0.975), 3), "]\n")
cat("flfp_z:       median =", round(median(post_m2$b_flfp_z), 3),
    " | 95% CI [", round(quantile(post_m2$b_flfp_z, 0.025), 3), ",",
    round(quantile(post_m2$b_flfp_z, 0.975), 3), "]\n")

# Probability of direction (% of posterior on same side as median)
cat("\n--- Probability of direction ---\n")
cat("P(gender_equal_z < 0):", round(mean(post_m1$b_gender_equal_z < 0), 3), "\n")
cat("P(relig_free_z < 0):", round(mean(post_m1$b_relig_free_z < 0), 3), "\n")
cat("P(gdp_percap_z < 0):", round(mean(post_m2$b_gdp_percap_z < 0), 3), "\n")
cat("P(flfp_z < 0):", round(mean(post_m2$b_flfp_z < 0), 3), "\n")

# ── 3i. Posterior coefficient plot ──
png("output/figures/h3_bayesian_posteriors.png", width = 1000, height = 600, res = 120)

# Combine posteriors for plotting
posterior_plot_data <- bind_rows(
  data.frame(
    model = "M1: Cultural",
    param = "gender_equal",
    value = post_m1$b_gender_equal_z,
    group = "Cultural"
  ),
  data.frame(
    model = "M1: Cultural",
    param = "relig_free",
    value = post_m1$b_relig_free_z,
    group = "Cultural"
  ),
  data.frame(
    model = "M2: Economic",
    param = "gdp_percap",
    value = post_m2$b_gdp_percap_z,
    group = "Economic"
  ),
  data.frame(
    model = "M2: Economic",
    param = "flfp",
    value = post_m2$b_flfp_z,
    group = "Economic"
  )
)

ggplot(posterior_plot_data, aes(x = value, y = param, fill = group)) +
  stat_halfeye(.width = c(0.66, 0.95), alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = c("Cultural" = "#2166AC", "Economic" = "#B2182B")) +
  facet_wrap(~model, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 12) +
  labs(
    title = "H3: Bayesian Posterior Distributions",
    subtitle = "Effect on gradient steepness (standardised coefficients)",
    x = "Posterior estimate (standardised)",
    y = NULL,
    fill = "Predictor Group"
  ) +
  theme(legend.position = "bottom")
dev.off()
cat("\nSaved: output/figures/h3_bayesian_posteriors.png\n")

# ── Save Bayesian models ──
saveRDS(list(
  m1_cultural = m1_cultural,
  m2_economic = m2_economic,
  m3_postsoc  = m3_postsoc,
  m4_combined = m4_combined,
  waic_comp = waic_comp,
  loo_comp = loo_comp,
  bayes_data = bayes_data
), "data/models/h3_bayesian.rds")
cat("Saved: data/models/h3_bayesian.rds\n")


# ══════════════════════════════════════════════════════════════════════════════
# PART 4: SUMMARY AND H3 VERDICT
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("  H3 SUMMARY\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

cat("RESEARCH QUESTION: Is cross-country variation in the education-fertility\n")
cat("gradient predicted better by cultural/institutional indicators than by\n")
cat("economic indicators?\n\n")

cat("--- STAGE 1: XGBoost + SHAP (Exploratory) ---\n")
cat("LOOCV R²:", round(loocv_r2, 3), "\n")
cat("Cultural aggregate SHAP:", round(cultural_total, 4),
    sprintf("(%.1f%%)", 100 * cultural_total / total_importance), "\n")
cat("Economic aggregate SHAP:", round(economic_total, 4),
    sprintf("(%.1f%%)", 100 * economic_total / total_importance), "\n")
cat("Top predictor:", shap_importance$feature[1], "\n\n")

cat("--- STAGE 2: Bayesian (Confirmatory) ---\n")
cat("Best model (WAIC):", rownames(waic_comp)[1], "\n")
cat("Best model (LOO):", rownames(loo_comp)[1], "\n\n")

cat("══════════════════════════════════════════════════════════════════════\n\n")

# ── Methodology log ──
cat("
====================================================================
METHODOLOGY LOG — H3 ENTRIES
====================================================================

36. H3 Stage 1: XGBoost + SHAP. Gradient-boosted regression of 
    gradient_steepness on 9 predictors (5 cultural, 3 economic, 
    1 regional). Conservative hyperparameters for N=21 
    (max_depth=2, lambda=5, alpha=1). LOOCV for honest performance. 
    SHAP values computed via xgboost native predcontrib. Cultural 
    vs economic importance compared via aggregate mean |SHAP|.

37. H3 Stage 2: Bayesian linear models via brms. Four models compared:
    (1) Cultural-only (gender_equal + relig_free),
    (2) Economic-only (gdp_percap + flfp),
    (3) Post-socialist only,
    (4) Combined (gender_equal + flfp + post_socialist).
    Regularising Student-t(3,0,1) priors on all parameters.
    WAIC and LOO-CV for model comparison. Contraceptive prevalence
    excluded from Bayesian models due to 43% missingness.

====================================================================
")

cat("\nDone. H3 analysis complete.\n")





