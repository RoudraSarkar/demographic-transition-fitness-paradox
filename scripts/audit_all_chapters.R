# ============================================================================
# audit_all_chapters.R
# THE FOLLOWING SCRIPT WAS USED TO RECTIFY ERRORS/MAKE UNFORSEEN CHANGES/INSPECT INFORMATION 


library(dplyr)
library(tidyr)

setwd("/Users/whiz/Desktop/dissertation")

hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
sub <- function(x) cat("\n--- ", x, " ---\n", sep = "")
claim <- function(what, tex) cat(sprintf("  [tex claims: %s]  %s\n", tex, what))

safe <- function(label, expr) {
  out <- try(force(expr), silent = TRUE)
  if (inherits(out, "try-error")) {
    cat("\n  !! COULD NOT VERIFY:", label, "\n")
    cat("     ", gsub("\n", " ", as.character(out)), "\n")
    return(invisible(NULL))
  }
  invisible(out)
}

load_rds <- function(path) {
  if (!file.exists(path)) stop("missing file: ", path)
  readRDS(path)
}

cat("AUDIT OF ALL REPORTED NUMBERS\n")
cat("Run date:", format(Sys.time()), "\n")
cat("R version:", R.version.string, "\n")

# ============================================================================
hdr("CHAPTER 3 — DATA AND METHODOLOGY")
# ============================================================================

safe("3.2 country sample", {
  asfr <- load_rds("data/derived/asfr_data.rds")
  etfr <- load_rds("data/derived/etfr_data.rds")
  sub("3.2 / Table 3.1 — panel dimensions")
  claim(sprintf("asfr_data rows = %d", nrow(asfr)), "—")
  claim(sprintf("etfr_data rows = %d", nrow(etfr)), "933 observations")
  claim(sprintf("countries = %d", n_distinct(etfr$country)), "21")
  claim(sprintf("year range = %d–%d", min(etfr$year), max(etfr$year)), "2007–2024")
  claim(sprintf("age groups = %s", paste(sort(unique(asfr$age_group)), collapse = ", ")), "seven, 15–49")
  cat("\n  Countries in panel:\n")
  print(sort(unique(etfr$country)))
  cat("\n  Year coverage by country (the panel is unbalanced — check Sweden):\n")
  print(as.data.frame(etfr |> group_by(country) |>
    summarise(n_years = n_distinct(year), first = min(year), last = max(year),
              .groups = "drop") |> arrange(n_years)))
})

safe("3.3 predictor coverage", {
  d2 <- load_rds("data/derived/dataset2_full.rds")
  sub("3.3 — Dataset 2 predictor coverage")
  claim(sprintf("dataset2_full rows = %d, cols = %d", nrow(d2), ncol(d2)), "21 countries")
  cat("\n  Non-missing count per column (tex claims: OECD 18–19 of 21; World Bank 12 of 21; ESS 20 of 21):\n")
  print(data.frame(
    variable = names(d2),
    n_nonmissing = sapply(d2, function(x) sum(!is.na(x))),
    row.names = NULL
  ))
  cat("\n  Complete cases across all predictors (tex claims N = 18 after dropping\n")
  cat("  North Macedonia, Serbia, Türkiye):\n")
  cc <- d2[complete.cases(d2), ]
  cat("    complete-case N =", nrow(cc), "\n")
  cat("    dropped:", paste(setdiff(d2$country, cc$country), collapse = ", "), "\n")

  sub("3.3 — V-Dem collinearity")
  cat("  tex claims: 'three of these correlate above r = 0.98'\n")
  vdem <- c("libdem", "gender_equal", "relig_free", "polyarchy", "egaldem")
  vdem <- vdem[vdem %in% names(d2)]
  if (length(vdem) > 1) {
    cm <- cor(d2[vdem], use = "pairwise.complete.obs")
    print(round(cm, 3))
    offdiag <- cm[upper.tri(cm)]
    cat("\n    pairs with |r| > 0.98:", sum(abs(offdiag) > 0.98), "\n")
    cat("    max off-diagonal |r| :", round(max(abs(offdiag)), 4), "\n")
  }
})

safe("3.4 BCS70 descriptives", {
  h4 <- load_rds("data/derived/h4_analytical_data.rds")
  sub("3.4 — BCS70 analytical sample")
  claim(sprintf("total N = %d", nrow(h4)), "8,318")
  if ("female" %in% names(h4)) {
    claim(sprintf("women = %d, men = %d", sum(h4$female == 1, na.rm = TRUE),
                  sum(h4$female == 0, na.rm = TRUE)), "4,445 women, 3,873 men")
  }
  if ("d" %in% names(h4)) {
    cat("\n  Treatment distribution (tex claims 35.6% at NVQ 4):\n")
    print(round(100 * prop.table(table(h4$d)), 1))
  }
  if ("y" %in% names(h4)) {
    cat("\n  Childlessness (tex claims 21.7% childless at age 42):\n")
    cat("    % with y == 0:", round(100 * mean(h4$y == 0, na.rm = TRUE), 1), "\n")
  }
})

# ============================================================================
hdr("CHAPTER 4 — DESCRIPTIVE RESULTS  [previously found STALE — recheck]")
# ============================================================================

safe("Table 4.1", {
  etfr <- load_rds("data/derived/etfr_data.rds")
  sub("Table 4.1 — pooled descriptives by tier")
  cat("  tex NOW claims (after correction): low 1.85 [0.36,3.39] sd 0.48\n")
  cat("                                     medium 1.36 [0.84,2.24] sd 0.22\n")
  cat("                                     high 1.51 [0.48,2.32] sd 0.27\n\n")
  print(as.data.frame(etfr |>
    mutate(education = factor(education, levels = c("low", "medium", "high"))) |>
    group_by(education) |>
    summarise(mean = round(mean(etfr, na.rm = TRUE), 3),
              sd   = round(sd(etfr, na.rm = TRUE), 3),
              min  = round(min(etfr, na.rm = TRUE), 3),
              max  = round(max(etfr, na.rm = TRUE), 3),
              n    = n(), .groups = "drop")))
})

safe("4.2 gradient steepness", {
  gs <- load_rds("data/derived/gradient_shape.rds")
  sub("4.2 — steepness range")
  st <- gs |> mutate(steepness = round(low - high, 3)) |> arrange(desc(steepness))
  claim(sprintf("max = %.3f (%s)", st$steepness[1], st$country[1]), "+1.322 Slovakia")
  claim(sprintf("min = %.3f (%s)", st$steepness[nrow(st)], st$country[nrow(st)]), "-0.462 Denmark")
  cat("\n  Countries with negative steepness (tex now claims 3: Denmark, Norway, Poland):\n")
  print(as.data.frame(st |> filter(steepness < 0) |> select(country, low, high, steepness)))
  cat("\n  Full ranking (cross-check against Appendix A):\n")
  print(as.data.frame(st |> select(country, low, medium, high, steepness, share_low)))
})

safe("4.3 low-tier share", {
  gs <- load_rds("data/derived/gradient_shape.rds")
  sub("4.3 — low-education share range")
  claim(sprintf("min = %.2f%% (%s)", min(gs$share_low), gs$country[which.min(gs$share_low)]),
        "under 5% Poland")
  claim(sprintf("max = %.2f%% (%s)", max(gs$share_low), gs$country[which.max(gs$share_low)]),
        "over 56% Türkiye")
  cat("\n  Sorted shares (tex claims a cluster below 11% and a remainder above 12.8%):\n")
  print(round(sort(gs$share_low), 2))
})

safe("Table 4.2 shape classification", {
  gs <- load_rds("data/derived/gradient_shape.rds")
  sub("Table 4.2 — shape counts")
  cat("  tex claims: monotonic 5, inverted_bottom 2, composition 8, broad 6\n\n")
  print(table(gs$shape))
  cat("\n  Membership:\n")
  for (s in sort(unique(gs$shape))) {
    cat("   ", s, ":", paste(sort(gs$country[gs$shape == s]), collapse = ", "), "\n")
  }
  sub("4.4 — marginal cases")
  cat("  tex names Poland (low-med 0.01), Greece and Estonia (med-high 0.03):\n")
  print(as.data.frame(gs |>
    mutate(gap_low_med  = round(low - medium, 3),
           gap_high_med = round(high - medium, 3),
           min_gap = round(pmin(abs(low - medium), abs(high - medium)), 3)) |>
    select(country, low, medium, high, gap_low_med, gap_high_med, min_gap) |>
    arrange(min_gap) |> head(6)))
})

# ============================================================================
hdr("CHAPTER 5 — H1 AND H2")
# ============================================================================

safe("Tables 5.1 / 5.2 / 5.3", {
  h1 <- load_rds("data/models/h1_models.rds")
  sub("Table 5.1 — M1 baseline")
  cat("  tex claims: low +0.289 (0.052) p<0.001 [0.181,0.397]\n")
  cat("              high +0.102 (0.041) p=0.022 [0.017,0.188]; within R2 = 0.294\n\n")
  print(broom::tidy(h1$m1, conf.int = TRUE))
  cat("  within R2:", round(fixest::r2(h1$m1, "wr2"), 4), "\n")

  sub("Table 5.1 — strict H1 contrast")
  cat("  tex claims: -0.187 (0.051) p=0.002\n\n")
  print(broom::tidy(h1$m1_ref_low, conf.int = TRUE) |>
          filter(grepl("high", term)))

  sub("Table 5.2 — country x education interactions  [NOT PREVIOUSLY VERIFIED]")
  cat("  tex claims: Slovakia low +0.430 | Spain med +0.343 low +0.368\n")
  cat("              Latvia -0.159/+0.176 | Greece -0.103/+0.170\n")
  cat("              Romania med -0.577 low -0.203 | Denmark -0.255/-0.510\n")
  cat("              Türkiye +0.152/+0.177 | within R2 = 0.593\n\n")
  m2t <- broom::tidy(h1$m2)
  print(as.data.frame(m2t |>
    filter(grepl("Slovakia|Spain|Latvia|Greece|Romania|Denmark|rkiye", term)) |>
    mutate(estimate = round(estimate, 3)) |>
    select(term, estimate)))
  cat("\n  M2 within R2:", round(fixest::r2(h1$m2, "wr2"), 4), "\n")

  sub("Table 5.3 — robustness battery  [ROWS 2-5 NOT PREVIOUSLY VERIFIED]")
  cat("  tex claims:\n")
  cat("    Balanced (8 countries)  N=432  low +0.379 (0.078) p=0.002 | high +0.143 (0.059) p=0.045\n")
  cat("    Excluding 2020-2021     N=840  low +0.278 (0.051) p<0.001 | high +0.097 (0.042) p=0.033\n")
  cat("    Excluding U-shape comp. N=561  low +0.244 (0.059) p=0.001 | high +0.089 (0.069) p=0.216\n")
  cat("    Population-weighted     N=933  low +0.242 (0.078) p=0.006 | high -0.012 (0.083) p=0.890\n\n")
  for (nm in c("m1_balanced", "m1_no_covid", "m1_no_type_a", "m1_weighted")) {
    if (!is.null(h1[[nm]])) {
      cat("  ", nm, " (N =", nobs(h1[[nm]]), "):\n", sep = "")
      print(as.data.frame(broom::tidy(h1[[nm]]) |>
        mutate(across(where(is.numeric), ~round(.x, 4)))))
      cat("\n")
    }
  }
})

safe("Table 5.4 quadratic", {
  h2 <- load_rds("data/models/h2_models.rds")
  sub("Table 5.4 — linear vs quadratic")
  cat("  tex claims: linear b1 -0.093 (0.025) p=0.002, within R2 0.119, AIC -213.0, BIC -24.3\n")
  cat("              quad b1 -0.484 (0.088), b2 +0.195 (0.039), within R2 0.294,\n")
  cat("                   AIC -417.0, BIC -223.5, curve min 1.24\n\n")
  print(broom::tidy(h2$m_linear))
  cat("  linear within R2:", round(fixest::r2(h2$m_linear, "wr2"), 4),
      "| AIC:", round(AIC(h2$m_linear), 1), "| BIC:", round(BIC(h2$m_linear), 1), "\n\n")
  print(broom::tidy(h2$m_quad))
  cat("  quad within R2:", round(fixest::r2(h2$m_quad, "wr2"), 4),
      "| AIC:", round(AIC(h2$m_quad), 1), "| BIC:", round(BIC(h2$m_quad), 1), "\n")
  b1 <- coef(h2$m_quad)["educ_num"]; b2 <- coef(h2$m_quad)["educ_num_sq"]
  cat("  curve minimum:", round(-b1 / (2 * b2), 3), "\n")
  cat("  Wald F on b2 (tex claims F(1,20) = 24.8, p = 7.16e-05):\n")
  print(fixest::wald(h2$m_quad, "educ_num_sq"))
  cat("\n  Predicted relative eTFR (tex claims 1.00 / 0.75 / 0.83):\n")
  for (e in 0:2) cat("    educ =", e, ":", round(exp(b1 * e + b2 * e^2), 4), "\n")
})

safe("Table 5.5 quantile bootstrap", {
  qb <- load_rds("data/models/h2_quantile_boot.rds")
  sub("Table 5.5 — quantile regression with clustered bootstrap SEs")
  cat("  tex claims low  +0.093(.078) +0.140(.061) +0.205(.053) +0.236(.059) +0.280(.062)\n")
  cat("             med  -0.119(.065) -0.129(.048) -0.102(.036) -0.091(.042) -0.064(.045)\n")
  cat("             p    low .233 .021 <.001 <.001 <.001 | med .065 .007 .005 .032 .154\n\n")
  print(as.data.frame(qb$boot_clustered |>
    mutate(across(c(estimate, std_error, p_value), ~round(.x, 4))) |>
    select(tau, term, estimate, std_error, p_value)))
  cat("\n  SE inflation from clustering (tex claims 2.5-3.1x):\n")
  cmp <- qb$boot_clustered |> select(tau, term, se_cl = std_error) |>
    left_join(qb$boot_unclustered |> select(tau, term, se_un = std_error),
              by = c("tau", "term")) |>
    mutate(ratio = round(se_cl / se_un, 2))
  print(as.data.frame(cmp))
})

safe("Table 5.7 age-floor sweep", {
  af <- load_rds("data/derived/agefloor_sweep.rds")
  sub("Table 5.7 — age-floor sweep")
  cat("  tex claims 15-49: +0.289 / +0.102 / -0.187 / b2 +0.195 / min 1.24\n")
  cat("             20-49: +0.317 / +0.160 / -0.157 / b2 +0.238 / min 1.16\n")
  cat("             25-49: +0.016 / +0.217 / +0.201 / b2 +0.116 / min 0.57\n\n")
  print(af$sweep_tab)
  cat("\n  Tier means by floor (tex: 1.85/1.36/1.51; 1.80/1.28/1.51; 1.15/1.10/1.36):\n")
  print(as.data.frame(af$tier_means))
  cat("\n  Composition (Appendix B Table B.1 — tex: 15-19 = 82.0/18.0/0.0; 20-24 = 14.0/70.8/15.2):\n")
  print(as.data.frame(af$edu_comp))
  cat("\n  Contribution (Appendix B Table B.2 — tex: low 3.2/35.0/30.4/19.8/9.2/2.3/0.1):\n")
  print(as.data.frame(af$contrib |> select(education, age_group, pct_of_tier) |>
    pivot_wider(names_from = age_group, values_from = pct_of_tier)))
})

safe("Table 5.4b CR2", {
  cr <- load_rds("data/models/h1_cr2.rds")
  sub("Section 5.3.1 — CR2")
  cat("  tex claims: CR2 SEs 0.051/0.041, df 18.7, p 0.021; strict -0.187 p=0.0015\n")
  cat("              CR0 0.050/0.040; fixest 0.052/0.041; balanced df exactly 7.0\n\n")
  print(cr$comparison)
  cat("\n"); print(cr$strict_cmp)
  cat("\n"); print(cr$cr2_table)
})

# ============================================================================
hdr("CHAPTER 6 — H3 AND H4  [CHAPTER 6 H3 NEVER VERIFIED — HIGHEST RISK]")
# ============================================================================

safe("Table 6.1 SHAP", {
  xg <- load_rds("data/models/h3_xgboost.rds")
  sub("6.1 / Table 6.1 — XGBoost and SHAP")
  cat("  tex claims: 13 boosting rounds; LOOCV R2 = -0.086\n")
  cat("              relig_free 0.0263 (44.9%), contraceptive 0.0130 (22.2%),\n")
  cat("              gdp_percap 0.0107 (18.3%), polyarchy 0.0031 (5.3%),\n")
  cat("              libdem 0.0023 (3.9%), ess_secular 0.0022 (3.8%),\n")
  cat("              ess_autonomy 0.0009 (1.5%), five predictors at 0.0000\n")
  cat("              shares: attitudinal 5.4%, institutional 54.1%, economic 40.5%\n\n")
  cat("  best_nrounds :", xg$best_nrounds, "\n")
  cat("  LOOCV R2     :", round(xg$loocv_r2, 4), "\n")
  cat("  LOOCV RMSE   :", round(xg$loocv_rmse, 4), "\n\n")
  cat("  mean |SHAP|:\n")
  print(round(sort(xg$mean_abs_shap, decreasing = TRUE), 4))
  cat("\n  as % of total:\n")
  print(round(100 * sort(xg$mean_abs_shap, decreasing = TRUE) / sum(xg$mean_abs_shap), 1))
  cat("\n  block shares:\n")
  cat("    attitudinal  :", round(100 * xg$attitudinal_share, 1), "%\n")
  cat("    institutional:", round(100 * xg$institutional_share, 1), "%\n")
  cat("    economic     :", round(100 * xg$economic_share, 1), "%\n")
  cat("    regional     :", round(100 * xg$regional_share, 1), "%\n")
  cat("\n  tex also claims: 'reclassifying contraceptive shifts cultural to 81.7%\n")
  cat("  and economic to 18.3%' — check this arithmetic against the shares above.\n")
})

safe("Tables 6.2 / 6.3 Bayesian", {
  bh <- load_rds("data/models/h3_bayesian.rds")
  sub("6.2 / Table 6.2 — WAIC comparison")
  cat("  tex claims: M2 economic 0.0 | M3 postsoc -1.1 (0.5) | M5 attitudinal -1.5 (1.1)\n")
  cat("              M1 institutional -1.9 (1.0) | M6 -2.1 (1.2) | M4 -3.1 (0.6)\n")
  cat("              N = 18; max Rhat = 1.003\n\n")
  print(bh$waic_comp)
  cat("\n  LOO comparison (tex claims identical ordering):\n")
  print(bh$loo_comp)
  cat("\n  Bayesian data N:", nrow(bh$bayes_data), "\n")

  sub("Convergence — tex claims max Rhat = 1.003")
  for (nm in c("m1_institutional", "m2_economic", "m3_postsoc",
               "m4_combined", "m5_attitudinal", "m6_combined_new")) {
    if (!is.null(bh[[nm]])) {
      rh <- max(brms::rhat(bh[[nm]]), na.rm = TRUE)
      cat(sprintf("    %-18s max Rhat = %.4f\n", nm, rh))
    }
  }

  sub("Table 6.3 — posterior summaries")
  cat("  tex claims: gdp_percap (M2) median -0.27 sd 0.15 [-0.57,+0.02] P(<0)=0.966\n")
  cat("              flfp (M2) +0.16 0.15 [-0.13,+0.46] P(>0)=0.873\n")
  cat("              ess_gender_egal (M5) +0.15 0.12 [-0.08,+0.39] P(>0)=0.909\n")
  cat("              ess_secular (M5) +0.04 0.12 [-0.19,+0.27] P(>0)=0.648\n")
  cat("              relig_free (M1) -0.12 0.12 [-0.35,+0.12] P(<0)=0.853\n")
  cat("              gender_equal (M1) +0.01 0.12 [-0.23,+0.25] P(>0)=0.531\n\n")
  for (nm in c("m2_economic", "m5_attitudinal", "m1_institutional")) {
    if (!is.null(bh[[nm]])) {
      cat("\n  ", nm, ":\n", sep = "")
      ps <- posterior::as_draws_df(bh[[nm]])
      bcols <- grep("^b_", names(ps), value = TRUE)
      bcols <- setdiff(bcols, "b_Intercept")
      for (b in bcols) {
        x <- ps[[b]]
        cat(sprintf("    %-24s median %+.3f  sd %.3f  [%+.3f, %+.3f]  P(<0) = %.3f  P(>0) = %.3f\n",
                    b, median(x), sd(x),
                    quantile(x, 0.025), quantile(x, 0.975),
                    mean(x < 0), mean(x > 0)))
      }
    }
  }
})

safe("Tables 6.4 / 6.5 / 6.6 H4", {
  h4 <- load_rds("data/models/h4_dml_results.rds")
  sub("Tables 6.4-6.6 — H4 DML  [these were regenerated after the seed fix]")
  cat("  tex claims: women -0.133 (0.028) N=1461 p<0.001\n")
  cat("              men   -0.047 (0.033) N=1081 p=0.148\n")
  cat("              pooled -0.110 (0.022) N=2542 p<0.001\n")
  cat("              mediation women M0/M1/M2/M3 = -0.133/-0.130/-0.049/-0.082\n")
  cat("              pathways women: att 2.2%, econ 61.2%, partner -25.0%, direct 61.6%\n\n")
  print(names(h4))
  for (nm in names(h4)) {
    if (is.data.frame(h4[[nm]])) {
      cat("\n  ", nm, ":\n", sep = "")
      print(as.data.frame(h4[[nm]]))
    }
  }
})

safe("Figure 6.4 raw gradient", {
  h4d <- load_rds("data/derived/h4_analytical_data.rds")
  sub("Figure 6.4 / 6.4 text — unadjusted BCS70 gradient")
  cat("  tex claims: women 2.12 at lowest tier to 1.57 at NVQ4+\n")
  cat("              women flat between NVQ3 (~1.59) and NVQ4+ (~1.58)\n\n")
  if (all(c("d", "y", "female") %in% names(h4d))) {
    print(as.data.frame(h4d |>
      filter(!is.na(d), !is.na(y), !is.na(female)) |>
      group_by(female, d) |>
      summarise(mean_children = round(mean(y, na.rm = TRUE), 3),
                n = n(), .groups = "drop") |>
      mutate(sex = ifelse(female == 1, "women", "men")) |>
      select(sex, nvq = d, mean_children, n)))
  }
})

# ============================================================================
hdr("SUMMARY")
# ============================================================================
cat("
Anything printed above that disagrees with main.tex is a stale number.

Priority order for checking:
  1. CHAPTER 6 H3  — Tables 6.1, 6.2, 6.3. Never verified. Same drafting period
                     as the stale Chapter 4 tables. Highest risk.
  2. Table 5.2     — the seven country interactions. Never verified.
  3. Table 5.3     — rows 2-5. Never verified.
  4. Section 3.3   — the 'r > 0.98' collinearity claim; predictor coverage counts.
  5. Section 3.4   — 35.6% at NVQ4; 21.7% childless.
  6. Figure 6.4    — 2.12 / 1.57 / 1.59 / 1.58.

Already verified and reproducing correctly (no need to recheck):
  Table 4.1 (after correction), Table 4.2 membership, Table 5.1, Table 5.4,
  Table 5.5, Table 5.7, Section 5.3.1 (CR2), Tables 6.4-6.6 (post-seed).
")

library(fixest); library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")

# ---- [2] where is the H4 data? ----
cat("\n--- H4 rds files ---\n")
print(list.files("data", pattern = "h4.*\\.rds$", recursive = TRUE, full.names = TRUE))

# ---- [3] which 18 countries in bayes_data? ----
bh <- readRDS("data/models/h3_bayesian.rds")
cat("\n--- bayes_data countries (n =", nrow(bh$bayes_data), ") ---\n")
print(sort(bh$bayes_data$country))
cat("\ndropped vs full 21:\n")
print(setdiff(sort(readRDS("data/derived/gradient_shape.rds")$country),
              sort(bh$bayes_data$country)))

# ---- [1] what did m1_weighted actually fit? ----
h1 <- readRDS("data/models/h1_models.rds")
cat("\n--- m1_weighted call ---\n")
print(h1$m1_weighted$call)
cat("\n--- etfr_data columns (need the weight variable) ---\n")
print(names(readRDS("data/derived/etfr_data.rds")))






library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")
h4 <- readRDS("data/processed/h4_analytical_data.rds")

cat("\n--- structure ---\n")
for (nm in names(h4)) {
  x <- h4[[nm]]
  cat(sprintf("  %-13s class=%-22s %s\n", nm, paste(class(x), collapse="/"),
      if (is.null(dim(x))) paste0("len ", length(x)) else paste(dim(x), collapse=" x ")))
}

cat("\n--- descriptives ---\n"); print(h4$descriptives)

cat("\n--- Ns  [tex: analysis 8,318 | women 4,445 | men 3,873] ---\n")
for (nm in c("df_full","df_analysis","df_women","df_men"))
  cat(sprintf("  %-13s %s\n", nm, nrow(h4[[nm]])))

da <- h4$df_analysis
cat("\n--- df_analysis columns ---\n"); print(names(da))

pick <- function(df, cands) { h <- cands[cands %in% names(df)]; if (length(h)) h[1] else NA_character_ }
cd <- pick(da, c("d","nvq","nvq_level","highest_nvq","edu_nvq","treat","treatment","education"))
cy <- pick(da, c("y","n_children","children","total_children","nkids","num_children","fertility"))
cf <- pick(da, c("female","sex","is_female"))
cat("\n  detected -> treatment:", cd, "| outcome:", cy, "| sex:", cf, "\n")

if (!is.na(cd)) {
  cat("\n  Treatment distribution [tex: 35.6% at NVQ 4]:\n")
  print(round(100 * prop.table(table(da[[cd]])), 1))
}
if (!is.na(cy)) {
  cat("\n  Childlessness [tex: 21.7%]:  % zero =",
      round(100 * mean(da[[cy]] == 0, na.rm = TRUE), 1), "\n")
}
if (!any(is.na(c(cd, cy, cf)))) {
  cat("\n--- Figure 6.4  [tex: women 2.12 low -> 1.57 NVQ4+; NVQ3 ~1.59, NVQ4+ ~1.58] ---\n")
  print(as.data.frame(da |>
    filter(!is.na(.data[[cd]]), !is.na(.data[[cy]]), !is.na(.data[[cf]])) |>
    group_by(sex = ifelse(.data[[cf]] == 1, "women", "men"), nvq = .data[[cd]]) |>
    summarise(mean_children = round(mean(.data[[cy]], na.rm = TRUE), 3),
              n = n(), .groups = "drop")))
}







library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")
ok <- function(lbl, expr) tryCatch(expr, error = function(e)
  cat("  !! ", lbl, ": ", conditionMessage(e), "\n", sep = ""))

# ============================================================
# [1] ASFR completeness — the 44-cell question
# ============================================================
cat("\n=== [1] ASFR PANEL COMPLETENESS ===\n")
ok("asfr", {
  asfr <- readRDS("data/derived/asfr_data.rds")
  etfr <- readRDS("data/derived/etfr_data.rds")
  cat("  asfr columns:\n"); print(names(asfr))
  cat("\n  asfr rows =", nrow(asfr), " | expected if complete = 6531 (311 x 3 x 7)\n")
  cat("  shortfall =", 6531 - nrow(asfr), "\n")

  cat("\n  n_age_groups in etfr_data [tex assumes 7 throughout]:\n")
  print(table(etfr$n_age_groups))
  cat("\n  sum(n_age_groups) =", sum(etfr$n_age_groups),
      " vs nrow(asfr) =", nrow(asfr),
      if (sum(etfr$n_age_groups) == nrow(asfr)) " -> RECONCILES\n" else " -> MISMATCH\n")

  short <- etfr |> filter(n_age_groups < 7)
  cat("\n  cells with <7 age groups:", nrow(short), "\n")
  if (nrow(short)) {
    print(as.data.frame(short |> count(country, n_age_groups)))
    cat("\n  their eTFR vs full-coverage cells, by tier:\n")
    print(as.data.frame(etfr |> group_by(education, complete = n_age_groups == 7) |>
      summarise(n = n(), mean_etfr = round(mean(etfr), 3), .groups = "drop")))
  }
})

# ============================================================
# [2] "ten pre-treatment confounders"
# ============================================================
cat("\n=== [2] CONFOUNDER COUNT  [tex: ten] ===\n")
ok("dml", {
  dml <- readRDS("data/models/h4_dml_results.rds")
  mt <- dml$models_total
  cat("  models_total names:\n"); print(names(mt))
  m <- mt[[1]]
  cat("\n  class:", paste(class(m), collapse = "/"), "\n")
  for (f in c("x_cols", "y_col", "d_cols", "n_folds", "n_rep")) {
    v <- tryCatch(m[[f]], error = function(e) NULL)
    if (!is.null(v)) cat(sprintf("  %-9s (%d): %s\n", f, length(v), paste(v, collapse = ", ")))
  }
})

# ============================================================
# [3] Method parameters  §3.6 / §3.7
# ============================================================
cat("\n=== [3] METHOD PARAMETERS ===\n")
ok("xgb", {
  xg <- readRDS("data/models/h3_xgboost.rds")
  cat("  h3_xgboost names:\n"); print(names(xg))
  p <- tryCatch(xg$model$params, error = function(e) NULL)
  if (is.null(p)) p <- tryCatch(xg$params, error = function(e) NULL)
  cat("\n  tex: depth 2, eta 0.1, lambda 5, alpha 1, min_child_weight 3\n")
  if (!is.null(p)) print(unlist(p)) else cat("  (params not stored — check xg structure)\n")
})
ok("brms", {
  bh <- readRDS("data/models/h3_bayesian.rds")
  b <- bh$m2_economic
  cat("\n  tex: 4 chains x 4,000 iterations; Student-t(3,0,1) priors\n")
  s <- b$fit@sim
  cat("  chains =", s$chains, "| iter =", s$iter, "| warmup =", s$warmup, "\n")
  cat("\n  priors:\n"); print(brms::prior_summary(b))
})

# ============================================================
# [4] Shape rule — does the stated rule reproduce Table 4.2?
# ============================================================
cat("\n=== [4] CLASSIFICATION RULE ===\n")
ok("shape", {
  g <- readRDS("data/derived/gradient_shape.rds")
  cat("  gradient_shape columns:\n"); print(names(g))
  chk <- g |>
    mutate(
      medium_lowest = etf_med_lowest <- (etfr_medium < etfr_low & etfr_medium < etfr_high),
      rule_as_written = ifelse(etfr_high > etfr_medium,
                               ifelse(share_low < 12, "j_curve_composition", "j_curve_broad"),
                               "(not a U)")) |>
    select(country, etfr_low, etfr_medium, etfr_high, share_low,
           medium_lowest, shape, rule_as_written)
  print(as.data.frame(chk))
  cat("\n  rows where the rule as written in 3.2/4.4 disagrees with the stored shape:\n")
  print(as.data.frame(chk |> filter(rule_as_written != shape, rule_as_written != "(not a U)")))
})

# ============================================================
# [5] Raw Eurostat — 3.2 sample construction (probe only)
# ============================================================
cat("\n=== [5] RAW FILES  [tex: 35 -> 21; twelve 'All ISCED'; BiH, Montenegro] ===\n")
ok("raw", print(list.files("data/raw", recursive = TRUE)))

library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")

cat("\n=== [4] CLASSIFICATION RULE ===\n")
g <- readRDS("data/derived/gradient_shape.rds")
cat("  gradient_shape columns:\n"); print(names(g))

chk <- g |>
  mutate(
    medium_lowest   = medium < low & medium < high,
    high_gt_medium  = high > medium,
    # the rule exactly as 3.2 / 4.4 state it: high > medium, then split on share_low
    rule_as_written = ifelse(high > medium,
                             ifelse(share_low < 12, "j_curve_composition", "j_curve_broad"),
                             "(rule silent)")
  ) |>
  select(country, low, medium, high, share_low, medium_lowest, shape, rule_as_written) |>
  arrange(shape, country)

print(as.data.frame(chk))

cat("\n  --- countries where the rule as written disagrees with the stored shape ---\n")
bad <- chk |> filter(rule_as_written != shape)
if (nrow(bad)) print(as.data.frame(bad)) else cat("  none\n")

cat("\n  --- stored shape vs medium_lowest (the condition 3.2 omits) ---\n")
print(table(shape = chk$shape, medium_lowest = chk$medium_lowest))








library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")
asfr <- readRDS("data/derived/asfr_data.rds")
etfr <- readRDS("data/derived/etfr_data.rds")

cat("\n--- [A] which age group is missing from the 44 short cells? ---\n")
short <- etfr |> filter(n_age_groups < 7) |> select(country, year, education)
print(asfr |> semi_join(short, by = c("country","year","education")) |>
        count(age_group) |> as.data.frame())

cat("\n--- [B] high tier at 15-19: empty, or near-zero denominators? ---\n")
print(asfr |> filter(education == "high", age_group == "15-19") |>
  summarise(n_cells = n(), pop_min = min(population), pop_med = median(population),
            births_zero = sum(births == 0), births_max = max(births),
            asfr_med = round(median(asfr), 4), asfr_max = round(max(asfr), 4)) |> as.data.frame())

cat("\n--- 10 largest high-tier 15-19 ASFRs ---\n")
print(asfr |> filter(education == "high", age_group == "15-19") |>
  arrange(desc(asfr)) |> select(country, year, births, population, asfr) |>
  head(10) |> as.data.frame())

cat("\n--- high tier ASFR by age group, for scale ---\n")
print(asfr |> filter(education == "high") |> group_by(age_group) |>
  summarise(n = n(), med_pop = round(median(population)), med_asfr = round(median(asfr), 4),
            max_asfr = round(max(asfr), 4), .groups = "drop") |> as.data.frame())

cat("\n--- [C] confounder count  [tex: ten] ---\n")
m <- readRDS("data/models/h4_dml_results.rds")$models_total$women
xc <- tryCatch(m$data$x_cols, error = function(e) NULL)
if (is.null(xc)) { cat("  m$data fields:\n"); print(names(m$data)) } else
  cat("  x_cols (", length(xc), "):\n  ", paste(xc, collapse = ", "), "\n", sep = "")

cat("\n--- [D] which BCS70 files does script 08 actually read? ---\n")
src <- readLines("scripts/08_h4data_bcs70.R", warn = FALSE)
writeLines(grep("UKDA|\\.dta", src, value = TRUE))

library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")
asfr <- readRDS("data/derived/asfr_data.rds")
af   <- readRDS("data/derived/agefloor_sweep.rds")

cat("\n--- [i] NA audit ---\n")
print(asfr |> group_by(education, age_group) |>
  summarise(n = n(), na_pop = sum(is.na(population)), na_asfr = sum(is.na(asfr)),
            zero_births = sum(births == 0, na.rm = TRUE), .groups = "drop") |>
  filter(na_pop > 0 | na_asfr > 0) |> as.data.frame())
cat("\n  total NA asfr rows:", sum(is.na(asfr$asfr)), "\n")

cat("\n--- [ii] recompute B.2: ratio of means vs mean of ratios ---\n")
tot <- asfr |> group_by(country, year, education) |>
  summarise(etfr = 5 * sum(asfr, na.rm = TRUE), .groups = "drop")
byag <- asfr |> group_by(country, year, education, age_group) |>
  summarise(part = 5 * sum(asfr, na.rm = TRUE), .groups = "drop") |>
  left_join(tot, by = c("country","year","education"))

cat("\n  ratio of means (what 5.7's tier means imply):\n")
print(byag |> group_by(education, age_group) |>
  summarise(pct = round(100 * sum(part) / sum(etfr), 1), .groups = "drop") |>
  tidyr::pivot_wider(names_from = age_group, values_from = pct) |> as.data.frame())

cat("\n  mean of ratios (suspected source of the 2.2%):\n")
print(byag |> filter(etfr > 0) |> mutate(r = 100 * part / etfr) |>
  group_by(education, age_group) |>
  summarise(pct = round(mean(r, na.rm = TRUE), 1), .groups = "drop") |>
  tidyr::pivot_wider(names_from = age_group, values_from = pct) |> as.data.frame())

cat("\n--- [iii] what script 10 stored ---\n")
print(as.data.frame(af$contrib))
cat("\n  agefloor_sweep objects:\n"); print(names(af))

library(dplyr); library(haven)
setwd("/Users/whiz/Desktop/dissertation")
h4 <- readRDS("data/processed/h4_analytical_data.rds")

cat("df_full N =", nrow(h4$df_full), "\n")
cat("df_analysis N =", nrow(h4$df_analysis), "\n\n")

# what makes df_full -> df_analysis? the filter that drops ~1,522
cat("rows in df_full missing each key var:\n")
for (v in c("y","d","female","sex_raw")) {
  if (v %in% names(h4$df_full))
    cat(sprintf("  %-8s missing: %d\n", v, sum(is.na(h4$df_full[[v]]))))
}

# 9,840 vs the ~17,000 birth cohort: which sweep's respondents define df_full?
# the age-51 main file is the most recent sweep; check its N
m51 <- read_dta("data/raw/bcs70/UKDA-9347-stata/stata13/bcs11_age51_main.dta", n_max = 0)
cat("\nage-51 file columns:", ncol(m51), "\n")
cat("\n--- how script 08 builds df_full (the join/filter lines) ---\n")
src <- readLines("scripts/08_h4data_bcs70.R", warn = FALSE)
writeLines(grep("df_full|filter|drop_na|inner_join|left_join|semi_join|nrow", src, value = TRUE)[1:25])

grep -n "relevel\|m1_weighted\|etfr_weighted\|ref = " scripts/03_h1_models.R









library(dplyr); library(haven)
setwd("/Users/whiz/Desktop/dissertation")
h4 <- readRDS("data/processed/h4_analytical_data.rds")

cat("df_full N =", nrow(h4$df_full), "\n")
cat("df_analysis N =", nrow(h4$df_analysis), "\n\n")

cat("rows in df_full missing each key var:\n")
for (v in c("y","d","female","sex_raw")) {
  if (v %in% names(h4$df_full))
    cat(sprintf("  %-8s missing: %d\n", v, sum(is.na(h4$df_full[[v]]))))
}
cat("\ncomplete on all three (y, d, female):",
    sum(complete.cases(h4$df_full[, intersect(c("y","d","female"), names(h4$df_full))])), "\n")

cat("\n--- how script 08 builds df_full ---\n")
src <- readLines("scripts/08_h4data_bcs70.R", warn = FALSE)
hits <- grep("df_full|df_analysis|filter|drop_na|_join|nrow|17000|17,000", src, value = TRUE)
writeLines(head(hits, 30))


src <- readLines("scripts/03_h1_models.R", warn = FALSE)
grep("relevel|m1_weighted|etfr_weighted|ref = ", src, value = TRUE)


src <- readLines("scripts/03_h1_models.R", warn = FALSE)
i <- grep("etfr_weighted <- etfr_data", src)
writeLines(src[i:(i+12)])

library(dplyr)
etfr_weighted <- etfr_data |>
  mutate(log_etfr = log(etfr),
         education = relevel(factor(education), ref = "medium")) |>
  left_join(pop_weights, by = c("country", "year", "education"))


library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")

src <- readLines("scripts/06b_ess_cultural_predictors.R", warn = FALSE)
cat("=== lines mentioning gender ===\n")
writeLines(grep("gender|gndr|egal|job|reverse|recode|scale|mean", src,
                value = TRUE, ignore.case = TRUE))

d2 <- readRDS("data/derived/dataset2_full.rds")
cat("\n=== ess_gender_egal by country, sorted ===\n")
print(as.data.frame(arrange(select(d2, country, ess_gender_egal), ess_gender_egal)))
}





library(dplyr)
d2 <- readRDS("data/derived/dataset2_full.rds")

# Does the CURRENT (wrong) variable have Nordics low? (we know yes)
# The fix must produce the REVERSE ranking. Since the composite is on a 1-5 scale,
# the corrected value for each country is simply (6 - current), because
# ess_gender_egal = mean of two 1-5 items, and 6-x inverts a 1-5 mean symmetrically.
d2 |>
  transmute(country,
            current      = ess_gender_egal,
            corrected    = 6 - ess_gender_egal) |>
  arrange(corrected) |>
  as.data.frame() |>
  print()


library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")

# Back up first
file.copy("data/derived/dataset2_full.rds",
          "data/derived/dataset2_full_preESSfix.rds", overwrite = FALSE)

# Apply the correction
d2 <- readRDS("data/derived/dataset2_full.rds")
d2$ess_gender_egal <- 6 - d2$ess_gender_egal
saveRDS(d2, "data/derived/dataset2_full.rds")

# Confirm it took
cat("Sweden should now be HIGH, Türkiye LOW:\n")
print(as.data.frame(arrange(select(d2, country, ess_gender_egal), ess_gender_egal)))


library(dplyr)
setwd("/Users/whiz/Desktop/dissertation")
h4 <- readRDS("data/processed/h4_analytical_data.rds")
da <- h4$df_women   # or df_analysis filtered to women

# What's the common complete-case sample across ALL mediators?
mediators <- c("m_trad_gender","m_profamily","m_log_earn","m_partner")
confounders <- c("soc_birth","mage","mage_fb","parity","birth_order",
                 "cog_10","soc_10","inc_10","soc_16","malaise_16")
keep <- complete.cases(da[, c("y","d", confounders, mediators)])
cat("common-sample N (women):", sum(keep), "  [expect ~812]\n")

# How is non-employment coded in log earnings?
cat("\nm_log_earn: n missing =", sum(is.na(da$m_log_earn)),
    "| n zero =", sum(da$m_log_earn == 0, na.rm = TRUE),
    "| min =", min(da$m_log_earn, na.rm = TRUE), "\n")
cat("Of those missing m_log_earn, mean d and mean y:\n")
print(da |> mutate(miss = is.na(m_log_earn)) |> group_by(miss) |>
      summarise(n = n(), mean_d = mean(d, na.rm=TRUE), mean_y = mean(y, na.rm=TRUE)))

# ── COMMON-SAMPLE H4 ROBUSTNESS: all stages on the SAME N ──────────────────
med_att  <- c("m_trad_gender", "m_profamily")
med_econ <- "m_log_earn"
med_part <- "m_partner"
all_med  <- c(med_att, med_econ, med_part)

x_women <- select_confounders(df_women, X_CANDIDATES)

# Build ONE common sample: complete on outcome, treatment, confounders, ALL mediators
common_vars <- c(Y_VAR, D_VAR, x_women, all_med)
df_common <- as.data.frame(df_women)[complete.cases(as.data.frame(df_women)[, common_vars]), ]
cat("\n=== COMMON SAMPLE N =", nrow(df_common), "===\n\n")

cs_M0 <- fit_dml(df_common, Y_VAR, D_VAR, x_women,
                 label = "CS M0 (total)")
cs_M1 <- fit_dml(df_common, Y_VAR, D_VAR, c(x_women, med_att),
                 label = "CS M1 (+attitudes)")
cs_M2 <- fit_dml(df_common, Y_VAR, D_VAR, c(x_women, med_att, med_econ),
                 label = "CS M2 (+earnings)")
cs_M3 <- fit_dml(df_common, Y_VAR, D_VAR, c(x_women, med_att, med_econ, med_part),
                 label = "CS M3 (+partner)")

cat("\n=== COMMON-SAMPLE SEQUENCE (all N =", nrow(df_common), ") ===\n")
cs_tab <- data.frame(
  Stage = c("M0 total", "M1 +attitudes", "M2 +earnings", "M3 +partner"),
  N     = c(cs_M0$n, cs_M1$n, cs_M2$n, cs_M3$n),
  theta = round(c(cs_M0$coef, cs_M1$coef, cs_M2$coef, cs_M3$coef), 4),
  SE    = round(c(cs_M0$se, cs_M1$se, cs_M2$se, cs_M3$se), 4),
  p     = round(c(cs_M0$pval, cs_M1$pval, cs_M2$pval, cs_M3$pval), 4)
)
print(cs_tab)

cat("\n=== PATHWAY SHARES (common sample) ===\n")
tot <- cs_M0$coef
cat(sprintf("  Attitudinal : %+.4f  (%.1f%%)\n", cs_M0$coef - cs_M1$coef,
            100 * (cs_M0$coef - cs_M1$coef) / tot))
cat(sprintf("  Economic    : %+.4f  (%.1f%%)\n", cs_M1$coef - cs_M2$coef,
            100 * (cs_M1$coef - cs_M2$coef) / tot))
cat(sprintf("  Partnership : %+.4f  (%.1f%%)\n", cs_M2$coef - cs_M3$coef,
            100 * (cs_M2$coef - cs_M3$coef) / tot))
cat(sprintf("  Direct      : %+.4f  (%.1f%%)\n", cs_M3$coef,
            100 * cs_M3$coef / tot))

cat("\n=== COMPOSITION vs MEDIATION ===\n")
cat(sprintf("  Full-sample M0 (N=1461): -0.1327\n"))
cat(sprintf("  Common-sample M0 (N=%d): %+.4f\n", cs_M0$n, cs_M0$coef))
cat(sprintf("  Difference attributable to composition: %+.4f\n", cs_M0$coef - (-0.1327)))

