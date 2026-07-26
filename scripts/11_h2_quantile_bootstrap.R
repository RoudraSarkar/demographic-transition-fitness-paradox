# ============================================================================
# 11_h2_quantile_bootstrap.R
# H2 — Quantile regression with bootstrap standard errors
#
# Fixes fix-list item 4.1. The dissertation (§5.5) currently states that
# "standard errors [are] unavailable in quantreg". That is not correct:
# quantreg::summary.rq() supports several inference methods, including
# se = "boot", and passes further arguments through to quantreg::boot.rq().
# A reader who knows the package will notice the claim is wrong.
#
# This script re-runs the Table 5.5 quantile regressions and attaches
# bootstrap SEs, confidence intervals, and p-values.
#
# CLUSTERING. boot.rq() accepts a `cluster` argument, implemented for the
# weighted xy bootstrap (bsmethod = "wxy"). Because the panel has only 21
# countries and observations within a country are not independent, the
# cluster bootstrap is the appropriate default here. The script also reports
# the unclustered xy bootstrap for comparison — if the two diverge sharply,
# within-country dependence matters and the clustered version is the one to
# report.
#
# REFERENCE LEVEL. rq() is called on factor(education) with no explicit level
# ordering, so R uses alphabetical order and "high" becomes the reference.
# This matches Table 5.5 as published (reference: high). The script asserts
# this rather than assuming it.
#
# Inputs:  data/derived/etfr_data.rds
# Outputs: data/models/h2_quantile_boot.rds
#          output/figures/h2_quantile_boot.png
# Depends: 02_descriptive_dataset1.R
# ============================================================================

library(dplyr)
library(tidyr)
library(quantreg)
library(ggplot2)

setwd("/Users/whiz/Desktop/dissertation")
set.seed(42)

R_BOOT   <- 1000                              # bootstrap replications
QUANTILES <- c(0.10, 0.25, 0.50, 0.75, 0.90)  # as in Table 5.5

etfr_data <- readRDS("data/derived/etfr_data.rds") |>
  mutate(log_etfr = log(etfr))

cat("=== Input ===\n")
cat("etfr_data:", nrow(etfr_data), "rows,",
    n_distinct(etfr_data$country), "countries\n")
cat("Bootstrap replications:", R_BOOT, "\n\n")

# Confirm the reference level is "high", matching the published table.
edu_levels <- levels(factor(etfr_data$education))
cat("factor(education) levels:", paste(edu_levels, collapse = ", "), "\n")
cat("Reference level (first):", edu_levels[1], "\n")
if (edu_levels[1] != "high") {
  warning("Reference level is not 'high'. Table 5.5 reports contrasts vs high. ",
          "Check before using these numbers.")
}
cat("\n")

# ── 1. Fit quantile regressions ──────────────────────────────────────────────
# Same specification as 04_h2_models.R: country and year enter as dummies
# because rq() has no fixed-effects syntax.

fml <- log_etfr ~ factor(education) + factor(country) + factor(year)

fits <- lapply(QUANTILES, function(tau) {
  cat("Fitting tau =", tau, "...\n")
  rq(fml, data = etfr_data, tau = tau)
})
names(fits) <- paste0("tau_", QUANTILES)

# ── 2. Bootstrap standard errors ─────────────────────────────────────────────
# Two variants. The clustered one is the headline; the unclustered one is a
# comparison that shows how much within-country dependence matters.

boot_summary <- function(fit, tau, clustered) {
  s <- try({
    if (clustered) {
      summary(fit, se = "boot", bsmethod = "wxy",
              cluster = etfr_data$country, R = R_BOOT)
    } else {
      summary(fit, se = "boot", bsmethod = "xy", R = R_BOOT)
    }
  }, silent = TRUE)

  if (inherits(s, "try-error")) {
    cat("  WARNING: bootstrap failed at tau =", tau,
        if (clustered) "(clustered)" else "(unclustered)", "\n")
    cat("  ", as.character(s), "\n")
    return(NULL)
  }

  ct <- coef(s)
  keep <- grepl("^factor\\(education\\)", rownames(ct))
  data.frame(
    tau       = tau,
    term      = sub("factor\\(education\\)", "", rownames(ct)[keep]),
    estimate  = ct[keep, "Value"],
    std_error = ct[keep, "Std. Error"],
    t_value   = ct[keep, "t value"],
    p_value   = ct[keep, "Pr(>|t|)"],
    clustered = clustered,
    row.names = NULL
  )
}

cat("\n=== Bootstrapping (clustered by country, bsmethod = wxy) ===\n")
boot_cl <- lapply(seq_along(QUANTILES), function(i) {
  cat("  tau =", QUANTILES[i], "\n")
  boot_summary(fits[[i]], QUANTILES[i], clustered = TRUE)
}) |> bind_rows()

cat("\n=== Bootstrapping (unclustered, bsmethod = xy) ===\n")
boot_un <- lapply(seq_along(QUANTILES), function(i) {
  cat("  tau =", QUANTILES[i], "\n")
  boot_summary(fits[[i]], QUANTILES[i], clustered = FALSE)
}) |> bind_rows()

stopifnot(nrow(boot_cl) > 0)

boot_cl <- boot_cl |>
  mutate(
    ci_lo = estimate - 1.96 * std_error,
    ci_hi = estimate + 1.96 * std_error
  )

# ── 3. Table 5.5, with SEs ───────────────────────────────────────────────────

cat("\n\n=== TABLE 5.5 (REVISED) — clustered bootstrap SEs ===\n")
cat("Reference: high. Bootstrap R =", R_BOOT, ", clustered by country.\n\n")

tab55 <- boot_cl |>
  mutate(
    cell = sprintf("%+.3f (%.3f)", estimate, std_error),
    term = factor(term, levels = c("low", "medium"))
  ) |>
  select(term, tau, cell) |>
  pivot_wider(names_from = tau, values_from = cell,
              names_prefix = "tau_") |>
  arrange(term)

print(as.data.frame(tab55))

cat("\n--- Same table, p-values ---\n")
tab55_p <- boot_cl |>
  mutate(term = factor(term, levels = c("low", "medium"))) |>
  select(term, tau, p_value) |>
  mutate(p_value = round(p_value, 4)) |>
  pivot_wider(names_from = tau, values_from = p_value, names_prefix = "tau_") |>
  arrange(term)
print(as.data.frame(tab55_p))

cat("\n--- Full detail (clustered) ---\n")
print(as.data.frame(
  boot_cl |>
    mutate(across(c(estimate, std_error, ci_lo, ci_hi), ~round(.x, 4)),
           p_value = round(p_value, 4)) |>
    select(tau, term, estimate, std_error, ci_lo, ci_hi, p_value)
))

# ── 4. Does clustering matter? ───────────────────────────────────────────────

if (nrow(boot_un) > 0) {
  cat("\n\n=== Clustered vs unclustered bootstrap SEs ===\n")
  cat("If the ratio is far above 1, within-country dependence matters and the\n")
  cat("clustered SEs are the ones to report.\n\n")
  cmp <- boot_cl |>
    select(tau, term, se_clustered = std_error) |>
    left_join(boot_un |> select(tau, term, se_unclustered = std_error),
              by = c("tau", "term")) |>
    mutate(ratio = round(se_clustered / se_unclustered, 2),
           across(c(se_clustered, se_unclustered), ~round(.x, 4)))
  print(as.data.frame(cmp))
}

# ── 5. Is the U-shape significant at every quantile? ─────────────────────────
# The dissertation's claim is that the U-shape holds across the whole
# conditional distribution. With SEs we can now test that rather than assert it.
# Reference is high, so:
#   low  > 0 means low-educated fertility exceeds high-educated
#   medium < 0 means medium-educated fertility is below high-educated
# The U-shape requires medium below BOTH, i.e. medium coefficient < 0 AND
# medium coefficient < low coefficient.

cat("\n\n=== U-shape significance by quantile ===\n")
verdict <- boot_cl |>
  select(tau, term, estimate, p_value) |>
  pivot_wider(names_from = term, values_from = c(estimate, p_value)) |>
  mutate(
    medium_below_high      = estimate_medium < 0,
    medium_below_high_sig  = estimate_medium < 0 & p_value_medium < 0.05,
    low_above_high         = estimate_low > 0,
    low_above_high_sig     = estimate_low > 0 & p_value_low < 0.05,
    u_shape_holds          = estimate_medium < 0 & estimate_medium < estimate_low
  )
print(as.data.frame(verdict |>
        mutate(across(starts_with("estimate"), ~round(.x, 3)),
               across(starts_with("p_value"),  ~round(.x, 4)))))

cat("\nQuantiles where medium is significantly below high:",
    sum(verdict$medium_below_high_sig), "of", nrow(verdict), "\n")
cat("Quantiles where low is significantly above high:",
    sum(verdict$low_above_high_sig), "of", nrow(verdict), "\n")

# ── 6. Figure ────────────────────────────────────────────────────────────────

p_q <- boot_cl |>
  mutate(term = factor(term, levels = c("low", "medium"),
                       labels = c("Low vs high", "Medium vs high"))) |>
  ggplot(aes(x = tau, y = estimate, colour = term, fill = term)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Low vs high" = "#d73027",
                                 "Medium vs high" = "#4575b4")) +
  scale_fill_manual(values = c("Low vs high" = "#d73027",
                               "Medium vs high" = "#4575b4")) +
  labs(
    title    = "Education contrasts across the conditional distribution of log eTFR",
    subtitle = paste0("Quantile regression, reference = high. Ribbons are 95% CIs from a ",
                      "clustered bootstrap (R = ", R_BOOT, ")."),
    x = expression(paste("Quantile (", tau, ")")),
    y = "Coefficient (log eTFR)",
    colour = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/h2_quantile_boot.png", p_q, width = 8, height = 5, dpi = 300)
cat("\nFigure saved to output/figures/h2_quantile_boot.png\n")

# ── 7. Save ──────────────────────────────────────────────────────────────────

dir.create("data/models", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(fits = fits, boot_clustered = boot_cl, boot_unclustered = boot_un,
       tab55 = tab55, verdict = verdict, R = R_BOOT),
  "data/models/h2_quantile_boot.rds"
)
cat("Saved to data/models/h2_quantile_boot.rds\n")



