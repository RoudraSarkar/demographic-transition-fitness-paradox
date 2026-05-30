# =============================================================================
# audit_chapter3.R
# Reads saved model objects and datasets to extract every fact
# that Chapter 3 needs to be accurate. Run in your dissertation directory.
# =============================================================================

library(dplyr)
library(fixest)
setwd("~/Desktop/dissertation")

cat("\n")
cat("================================================================\n")
cat("  CHAPTER 3 AUDIT — DATA AND METHODOLOGY FACT-CHECK\n")
cat("================================================================\n")

# ─── SECTION A: DATASET 1 ────────────────────────────────────────────────────

cat("\n\n=== A. DATASET 1 (etfr_data) ===\n")
etfr <- readRDS("data/derived/etfr_data.rds")
cat("Rows:           ", nrow(etfr), "\n")
cat("Countries:      ", n_distinct(etfr$country), "\n")
cat("Year range:     ", paste(range(etfr$year), collapse="–"), "\n")
cat("Education tiers:", paste(sort(unique(etfr$education)), collapse=", "), "\n")
cat("eTFR min:       ", round(min(etfr$etfr), 4), "\n")
cat("eTFR max:       ", round(max(etfr$etfr), 4), "\n")
cat("log(eTFR) min:  ", round(log(min(etfr$etfr)), 3), "\n")
cat("log(eTFR) max:  ", round(log(max(etfr$etfr)), 3), "\n")
cat("Columns:        ", paste(names(etfr), collapse=", "), "\n")
cat("n_age_groups values: ", paste(sort(unique(etfr$n_age_groups)), collapse=", "), "\n")

cat("\n--- Year coverage by country ---\n")
coverage <- etfr |>
  group_by(country) |>
  summarise(n_years = n_distinct(year), first = min(year), last = max(year)) |>
  arrange(n_years)
print(coverage, n = Inf)
cat("\nCountries with 13-18 years: ", sum(coverage$n_years >= 13 & coverage$n_years <= 18), "\n")
cat("Countries with <13 years:  ", sum(coverage$n_years < 13), "\n")
cat("\nFull country list:\n")
cat(paste(sort(unique(etfr$country)), collapse=", "), "\n")

# ─── SECTION B: GRADIENT SHAPE CLASSIFICATION ────────────────────────────────

cat("\n\n=== B. GRADIENT SHAPE CLASSIFICATION ===\n")
d2 <- readRDS("data/derived/dataset2_full.rds")

cat("\n--- Shape distribution ---\n")
print(table(d2$shape, useNA = "ifany"))

cat("\n--- Countries by shape ---\n")
for (s in sort(unique(d2$shape))) {
  countries <- d2 |> filter(shape == s) |> pull(country) |> sort()
  cat(sprintf("  %-25s (%d): %s\n", s, length(countries), paste(countries, collapse=", ")))
}

cat("\n--- U-shape indicator (medium < both low and high) ---\n")
cat("U-shape countries:    ", sum(d2$u_shape == 1), "\n")
cat("Non-U-shape countries:", sum(d2$u_shape == 0), "\n")

cat("\n--- share_low distribution ---\n")
cat("Min:    ", round(min(d2$share_low), 1), "\n")
cat("Max:    ", round(max(d2$share_low), 1), "\n")
cat("Median: ", round(median(d2$share_low), 1), "\n")
cat("Countries with share_low < 12%:\n")
low_share <- d2 |> filter(share_low < 12) |> select(country, share_low, shape) |> arrange(share_low)
print(low_share, n = Inf)

# ─── SECTION C: DATASET 2 ────────────────────────────────────────────────────

cat("\n\n=== C. DATASET 2 (country-level) ===\n")
cat("Rows:    ", nrow(d2), "\n")
cat("Columns: ", ncol(d2), "\n")
cat("Variables: ", paste(names(d2), collapse=", "), "\n")
cat("\n--- Missingness ---\n")
miss <- colSums(is.na(d2) | sapply(d2, function(x) is.nan(as.numeric(x))))
print(miss[miss > 0])

cat("\n--- Post-socialist countries ---\n")
post_soc <- d2 |> filter(post_socialist == 1) |> pull(country) |> sort()
cat("Count:", length(post_soc), "\n")
cat("Countries:", paste(post_soc, collapse=", "), "\n")

cat("\n--- Countries with missing GDP ---\n")
cat(paste(d2$country[is.na(d2$gdp_percap)], collapse=", "), "\n")
cat("\n--- Countries with missing FLFP ---\n")
cat(paste(d2$country[is.na(d2$flfp)], collapse=", "), "\n")
cat("\n--- Countries with missing/NaN contraceptive ---\n")
contra_miss <- d2$country[is.na(d2$contraceptive) | is.nan(d2$contraceptive)]
cat(paste(contra_miss, collapse=", "), "\n")
cat("Count:", length(contra_miss), "\n")

# ─── SECTION D: DATASET 3 (BCS70) ────────────────────────────────────────────

cat("\n\n=== D. DATASET 3 (BCS70) ===\n")
bcs <- readRDS("data/processed/h4_analytical_data.rds")
df <- bcs$df_analysis
cat("Total N:   ", nrow(df), "\n")
cat("Women:     ", nrow(bcs$df_women), "\n")
cat("Men:       ", nrow(bcs$df_men), "\n")

cat("\n--- Treatment (d) distribution ---\n")
d_tab <- table(df$d)
d_pct <- round(100 * prop.table(d_tab), 1)
for (i in seq_along(d_tab)) {
  cat(sprintf("  NVQ %s: %5d (%5.1f%%)\n", names(d_tab)[i], d_tab[i], d_pct[i]))
}

cat("\n--- Outcome (y) distribution ---\n")
y_tab <- table(df$y)
cat("Childless (y=0): ", y_tab["0"], " (", round(100*y_tab["0"]/nrow(df), 1), "%)\n", sep="")
cat("Mean children:   ", round(mean(df$y), 2), "\n")
cat("Max children:    ", max(df$y), "\n")

cat("\n--- All variable names in df_analysis ---\n")
cat(paste(names(df), collapse=", "), "\n")

cat("\n--- Coverage of ALL variables ---\n")
for (v in names(df)) {
  pct <- round(100 * mean(!is.na(df[[v]])), 1)
  if (pct < 100) {
    cat(sprintf("  %-25s : %5.1f%% non-missing\n", v, pct))
  }
}

cat("\n--- Age 51 fertility variables in df_analysis? ---\n")
age51_vars <- grep("bd11|age51|nchild", names(df), value = TRUE, ignore.case = TRUE)
if (length(age51_vars) > 0) {
  cat("Found: ", paste(age51_vars, collapse=", "), "\n")
} else {
  cat("No age-51 fertility variables found in df_analysis\n")
}

# ─── SECTION E: H1 MODELS ────────────────────────────────────────────────────

cat("\n\n=== E. H1 MODEL RESULTS ===\n")
h1_files <- list.files("data/models", pattern = "h1|m1", full.names = TRUE)
cat("Saved H1 model files:\n")
cat(paste(h1_files, collapse="\n"), "\n")

for (f in h1_files) {
  cat("\n--- Loading:", basename(f), "---\n")
  tryCatch({
    if (grepl("\\.csv$", f, ignore.case = TRUE)) {
      print(read.csv(f))
    } else {
      obj <- readRDS(f)
      if (inherits(obj, "fixest")) {
        cat("Type: fixest\n")
        cat("N obs:", nobs(obj), "\n")
        print(summary(obj))
      } else if (is.list(obj)) {
        cat("Type: list with elements:", paste(names(obj), collapse=", "), "\n")
        for (nm in names(obj)) {
          if (inherits(obj[[nm]], "fixest")) {
            cat("\n  >>", nm, "<<\n")
            cat("  N obs:", nobs(obj[[nm]]), "\n")
            print(coeftable(obj[[nm]]))
          }
        }
      } else {
        cat("Type:", class(obj), "\n")
        str(obj, max.level = 1)
      }
    }
  }, error = function(e) cat("  Error loading:", e$message, "\n"))
}

# ─── SECTION F: H2 MODELS ────────────────────────────────────────────────────

cat("\n\n=== F. H2 MODEL RESULTS ===\n")
h2_files <- list.files("data/models", pattern = "h2|m2|quad", full.names = TRUE)
cat("Saved H2 model files:\n")
cat(paste(h2_files, collapse="\n"), "\n")

for (f in h2_files) {
  cat("\n--- Loading:", basename(f), "---\n")
  tryCatch({
    if (grepl("\\.csv$", f, ignore.case = TRUE)) {
      print(read.csv(f))
    } else {
      obj <- readRDS(f)
      if (inherits(obj, "fixest")) {
        print(summary(obj))
      } else if (is.list(obj)) {
        cat("Type: list with elements:", paste(names(obj), collapse=", "), "\n")
        for (nm in names(obj)) {
          if (inherits(obj[[nm]], "fixest") || inherits(obj[[nm]], "rq")) {
            cat("\n  >>", nm, "<<\n")
            tryCatch(print(summary(obj[[nm]])), error = function(e) cat("  Error:", e$message, "\n"))
          }
        }
      } else {
        cat("Type:", class(obj), "\n")
      }
    }
  }, error = function(e) cat("  Error loading:", e$message, "\n"))
}

# ─── SECTION G: H3 MODELS ────────────────────────────────────────────────────

cat("\n\n=== G. H3 MODEL RESULTS ===\n")
h3_files <- list.files("data/models", pattern = "h3|xgb|bayes|shap", full.names = TRUE)
cat("Saved H3 model files:\n")
cat(paste(h3_files, collapse="\n"), "\n")

for (f in h3_files) {
  cat("\n--- Loading:", basename(f), "---\n")
  tryCatch({
    if (grepl("\\.csv$", f, ignore.case = TRUE)) {
      print(read.csv(f))
    } else {
      obj <- readRDS(f)
      if (inherits(obj, "brmsfit")) {
        cat("Type: brmsfit\n")
        cat("Formula:", as.character(obj$formula$formula), "\n")
        cat("N obs:", nobs(obj), "\n")
        rhat_vals <- brms::rhat(obj)
        cat("Max Rhat:", round(max(rhat_vals, na.rm=TRUE), 4), "\n")
        neff_vals <- brms::neff_ratio(obj)
        cat("Min neff_ratio:", round(min(neff_vals, na.rm=TRUE), 3), "\n")
        print(summary(obj)$fixed)
      } else if (inherits(obj, "xgb.Booster")) {
        cat("Type: xgboost model\n")
        cat("Features:", paste(obj$feature_names, collapse=", "), "\n")
        cat("Num boosting rounds:", obj$niter, "\n")
      } else if (is.list(obj)) {
        cat("Type: list with elements:", paste(names(obj), collapse=", "), "\n")
        if ("loocv_r2" %in% names(obj)) cat("LOOCV R2:", round(obj$loocv_r2, 4), "\n")
        if ("shap_values" %in% names(obj)) cat("SHAP matrix dims:", paste(dim(obj$shap_values), collapse=" x "), "\n")
        if ("mean_abs_shap" %in% names(obj)) {
          cat("Mean |SHAP| by feature:\n")
          print(round(sort(obj$mean_abs_shap, decreasing=TRUE), 4))
        }
        if ("cultural_share" %in% names(obj)) cat("Cultural SHAP share:", round(obj$cultural_share, 3), "\n")
        if ("economic_share" %in% names(obj)) cat("Economic SHAP share:", round(obj$economic_share, 3), "\n")
      } else {
        cat("Type:", paste(class(obj), collapse=", "), "\n")
      }
    }
  }, error = function(e) cat("  Error loading:", e$message, "\n"))
}

# ─── SECTION H: H4 DML RESULTS ───────────────────────────────────────────────

cat("\n\n=== H. H4 DML RESULTS ===\n")
h4_files <- list.files("data/models", pattern = "h4|dml|mediation", full.names = TRUE)
cat("Saved H4 model files:\n")
cat(paste(h4_files, collapse="\n"), "\n")

for (f in h4_files) {
  cat("\n--- Loading:", basename(f), "---\n")
  tryCatch({
    if (grepl("\\.csv$", f, ignore.case = TRUE)) {
      print(read.csv(f))
    } else {
      obj <- readRDS(f)
      if (is.list(obj)) {
        cat("Type: list with elements:", paste(names(obj), collapse=", "), "\n")
        for (nm in names(obj)) {
          if (grepl("theta|coef|result|total|pooled|women|men|mediation", nm, ignore.case = TRUE)) {
            cat("\n  >>", nm, "<<\n")
            if (is.numeric(obj[[nm]])) {
              print(round(obj[[nm]], 4))
            } else if (is.data.frame(obj[[nm]])) {
              print(obj[[nm]])
            } else {
              tryCatch(print(obj[[nm]]), error = function(e) cat("  ", class(obj[[nm]]), "\n"))
            }
          }
        }
      } else {
        cat("Type:", paste(class(obj), collapse=", "), "\n")
        tryCatch(print(summary(obj)), error = function(e) cat("  Cannot summarise\n"))
      }
    }
  }, error = function(e) cat("  Error loading:", e$message, "\n"))
}

# ─── SECTION I: ALL SAVED MODEL FILES ────────────────────────────────────────

cat("\n\n=== I. ALL SAVED MODEL FILES ===\n")
all_models <- list.files("data/models", recursive = TRUE, full.names = TRUE)
cat("Total saved model files:", length(all_models), "\n")
for (f in sort(all_models)) {
  info <- file.info(f)
  cat(sprintf("  %-50s  %8.1f KB  %s\n", basename(f), info$size/1024, format(info$mtime, "%Y-%m-%d %H:%M")))
}

# ─── SECTION J: H1 ROBUSTNESS — RE-DERIVE FROM DATA ─────────────────────────

cat("\n\n=== J. H1 ROBUSTNESS — RE-DERIVE FROM DATA ===\n")
etfr$log_etfr <- log(etfr$etfr)

full_cov <- etfr |> group_by(country) |> summarise(ny = n_distinct(year)) |> filter(ny == max(ny)) |> pull(country)
cat("Balanced subsample countries (", length(full_cov), "):", paste(full_cov, collapse=", "), "\n")

type_a <- d2 |> filter(shape == "j_curve_composition") |> pull(country) |> sort()
cat("Type A composition countries (", length(type_a), "):", paste(type_a, collapse=", "), "\n")

type_b <- d2 |> filter(shape == "j_curve_broad") |> pull(country) |> sort()
cat("Type B broad U countries (", length(type_b), "):", paste(type_b, collapse=", "), "\n")

mono <- d2 |> filter(shape == "monotonic_negative") |> pull(country) |> sort()
cat("Monotonic negative (", length(mono), "):", paste(mono, collapse=", "), "\n")

inv <- d2 |> filter(shape == "inverted_bottom") |> pull(country) |> sort()
cat("Inverted bottom (", length(inv), "):", paste(inv, collapse=", "), "\n")

cat("\n--- Population-weighted model saved? ---\n")
pop_files <- list.files("data/models", pattern = "weight|pop", full.names = TRUE)
if (length(pop_files) > 0) {
  cat("Found:", paste(pop_files, collapse=", "), "\n")
} else {
  cat("No population-weighted model file found in data/models/\n")
}

cat("\n--- Wild bootstrap results saved? ---\n")
boot_files <- list.files("data/models", pattern = "boot|wild|cluster", full.names = TRUE)
if (length(boot_files) > 0) {
  cat("Found:", paste(boot_files, collapse=", "), "\n")
} else {
  cat("No bootstrap results file found in data/models/\n")
}

cat("\n\n================================================================\n")
cat("  AUDIT COMPLETE — paste this entire output back into chat\n")
cat("================================================================\n")
