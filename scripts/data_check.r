# =============================================================================
# inspect_datasets.R
# Run this in your dissertation project to print the structure of all
# three analytical datasets. Paste the output back into chat if you want
# Claude to verify variable names against the methodology write-up.
# =============================================================================

library(dplyr)
setwd("~/Desktop/dissertation")

cat("\n=========================================================\n")
cat(" DATASET 1: Education-Stratified Period Fertility (H1, H2)\n")
cat("=========================================================\n")

etfr_data <- readRDS("data/derived/etfr_data.rds")
cat("Rows:        ", nrow(etfr_data), "\n")
cat("Countries:   ", n_distinct(etfr_data$country), "\n")
cat("Year range:  ", paste(range(etfr_data$year), collapse = "-"), "\n")
cat("Education levels: ", paste(unique(etfr_data$education), collapse = ", "), "\n")
cat("\n--- Structure ---\n")
str(etfr_data)
cat("\n--- Year coverage by country (unbalanced panel) ---\n")
print(etfr_data |>
        group_by(country) |>
        summarise(n_years = n_distinct(year),
                  first_year = min(year),
                  last_year  = max(year)) |>
        arrange(n_years), n = Inf)
cat("\n--- Summary statistics ---\n")
print(summary(etfr_data$etfr))

cat("\n=========================================================\n")
cat(" DATASET 2: Country-Level Outcomes + Predictors (H3)\n")
cat("=========================================================\n")

dataset2 <- readRDS("data/derived/dataset2_full.rds")
cat("Rows:    ", nrow(dataset2), "\n")
cat("Columns: ", ncol(dataset2), "\n")
cat("\n--- Variable names ---\n")
print(names(dataset2))
cat("\n--- Structure ---\n")
str(dataset2)
cat("\n--- Missing values ---\n")
print(colSums(is.na(dataset2)))

cat("\n=========================================================\n")
cat(" DATASET 3: BCS70 Individual-Level (H4)\n")
cat("=========================================================\n")

bcs <- readRDS("data/processed/h4_analytical_data.rds")
df_analysis <- bcs$df_analysis
cat("Total analytical sample: ", nrow(df_analysis), "\n")
cat("Women: ", nrow(bcs$df_women), "\n")
cat("Men:   ", nrow(bcs$df_men), "\n")
cat("\n--- Key analytical variables ---\n")
key_vars <- c("y", "d", "female",
              "soc_birth", "mage", "mage_fb", "parity", "birth_order",
              "cog_10", "soc_10", "inc_10",
              "cog_16", "soc_16", "malaise_16",
              "m_trad_gender", "m_profamily",
              "m_earnings", "m_log_earn", "m_partner")
key_vars_avail <- key_vars[key_vars %in% names(df_analysis)]
cat("Available:", paste(key_vars_avail, collapse = ", "), "\n")
cat("\n--- Coverage of key variables ---\n")
for (v in key_vars_avail) {
  cat(sprintf("  %-20s : %6.1f%% non-missing\n",
              v, 100 * mean(!is.na(df_analysis[[v]]))))
}
cat("\n--- Treatment distribution ---\n")
print(table(df_analysis$d, useNA = "ifany"))
cat("\n--- Outcome distribution ---\n")
print(table(df_analysis$y, useNA = "ifany"))
