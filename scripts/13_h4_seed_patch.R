# ============================================================================
# 13_h4_seed_patch.R
# H4 — Making the DML estimates reproducible and internally consistent
#
# Fixes fix-list items 2.5 and 2.6.
#
# THE PROBLEM
# -----------
# 08_h4dml_bcs70.r calls set.seed(20250525) ONCE, at line 41, near the top of
# the script. But fit_dml() is called many times afterwards:
#
#     res_w  <- fit_dml(df_w_cc, ...)          # total effect, women
#     res_m  <- fit_dml(df_m_cc, ...)          # total effect, men
#     res_p  <- fit_dml(df_p_cc, ...)          # total effect, pooled
#     ... then run_mediation() calls fit_dml() four more times per sex
#
# Each call draws random numbers (for cross-fitting fold assignment and for
# the random forests), which advances the RNG state. So every call starts
# from a different position in the random stream.
#
# The consequence is visible in the dissertation. In run_mediation(), the M0
# stage uses:
#
#     x_augmented <- unique(c(x_vars, character(0)))   # == x_vars
#
# which is EXACTLY the same data and the same covariates as the total-effect
# call. Same estimand, same inputs. Yet:
#
#     Table 6.4 (women, total)  : theta = -0.145
#     Table 6.5 (women, M0)     : theta = -0.142
#     Table 6.4 (men, total)    : theta = -0.060
#     Table 6.5 (men, M0)       : theta = -0.052
#
# The dissertation currently explains this as "stochastic variation in the
# cross-fitting partitions", which is honest and true. But it is avoidable,
# and two different numbers for the same quantity invites the question of
# what else moved.
#
# THE FIX
# -------
# Seed INSIDE fit_dml() rather than once at the top. Then any two calls with
# identical inputs produce identical output, because both start from the same
# RNG state. This is a two-line change.
#
# WHY NOT JUST INCREASE n_rep?
# More repetitions would shrink the discrepancy but never eliminate it, and
# the run is already 5 folds x 3 reps x 14 model fits. Seeding is exact and free.
#
# WHAT THIS DOES NOT FIX
# ----------------------
# Seeding makes the results reproducible, not more correct. The M0-vs-total
# gap of 0.003 (women) and 0.008 (men) is a fair indication of the Monte Carlo
# noise in any single DML estimate, and that noise does not disappear just
# because the seed is fixed. It is worth stating the n_rep = 3 median in the
# methods and not over-reading the third decimal place.
# ============================================================================

verify_seed_fix <- function() {

  needed <- c("fit_dml", "df_w_cc", "x_women", "Y_VAR", "D_VAR",
              "res_w", "med_women")
  missing <- needed[!vapply(needed, exists, logical(1))]
  if (length(missing) > 0) {
    stop("Not found in the environment: ", paste(missing, collapse = ", "),
         "\nSource the patched 08_h4dml_bcs70.r first, in this session.")
  }

  cat("=== TEST 1: is fit_dml deterministic? ===\n")
  cat("Running the women's total effect twice with identical inputs.\n")
  cat("If the seed patch worked, the two must agree to every decimal place.\n\n")

  a <- fit_dml(df_w_cc, Y_VAR, D_VAR, x_women, label = "verify A")
  b <- fit_dml(df_w_cc, Y_VAR, D_VAR, x_women, label = "verify B")

  cat(sprintf("\n  Run A: theta = %.6f, SE = %.6f\n", a$coef, a$se))
  cat(sprintf("  Run B: theta = %.6f, SE = %.6f\n", b$coef, b$se))
  cat(sprintf("  Difference in theta: %.2e\n", abs(a$coef - b$coef)))

  det_ok <- isTRUE(all.equal(a$coef, b$coef, tolerance = 1e-10))
  cat("  VERDICT:", if (det_ok) "PASS - deterministic" else
                    "FAIL - still varying between calls", "\n")

  cat("\n=== TEST 2: does the total effect match mediation M0? ===\n")
  cat("These use the same data and the same covariates, so they must agree.\n\n")

  m0_theta <- med_women$theta[1]

  cat(sprintf("  Total effect (Table 6.4) : %.6f\n", res_w$coef))
  cat(sprintf("  Mediation M0 (Table 6.5) : %.6f\n", m0_theta))
  cat(sprintf("  Difference               : %.2e\n", abs(res_w$coef - m0_theta)))

  match_ok <- isTRUE(all.equal(res_w$coef, m0_theta, tolerance = 1e-8))
  cat("  VERDICT:", if (match_ok) "PASS - tables now agree" else
                    "FAIL - still inconsistent", "\n")

  if (!match_ok) {
    cat("\n  If TEST 1 passed but TEST 2 failed, the two calls are not actually\n")
    cat("  receiving identical inputs. Check whether select_confounders() returns\n")
    cat("  the same vector for both, and whether prepare_dml_data() drops the same\n")
    cat("  rows. Compare:\n")
    cat("    setdiff(x_women, unique(c(x_women, character(0))))\n")
    cat("    nrow(df_w_cc)  vs  the M0 row count printed above\n")
  }

  cat("\n=== TEST 3: p-value consistency ===\n")
  cat("Item 2.5: with identical theta, SE and N, the p-value must be identical.\n\n")

  z_total <- res_w$coef / res_w$se
  p_total <- 2 * pnorm(-abs(z_total))
  cat(sprintf("  Women total: theta = %.4f, SE = %.4f, z = %.3f, p = %.4f\n",
              res_w$coef, res_w$se, z_total, p_total))
  cat(sprintf("  Reported p : %.4f\n", res_w$pval))
  cat("  These should agree to rounding.\n")

  invisible(list(det_ok = det_ok, match_ok = match_ok,
                 a = a, b = b, total = res_w$coef, m0 = m0_theta))
}


