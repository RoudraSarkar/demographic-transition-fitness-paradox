# =============================================================================
# 08_h4data_bcs70.R
# H4: BCS70 Data Processing - Education and Fertility
# FRESH START - Simple, robust, diagnostic-heavy approach
# =============================================================================

cat("\n=== H4 BCS70 Data Processing ===\n")
cat("Starting fresh...\n\n")

# =============================================================================
# 0. SETUP
# =============================================================================

# Clear environment completely
rm(list = ls())
gc()

# Set seed
set.seed(19700404)

# Load packages (install if needed)
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

suppressPackageStartupMessages({
  pacman::p_load(
    haven,      # Read .dta files
    dplyr,      # Data manipulation
    tidyr,      # Tidying data
    ggplot2     # Plotting
  )
})

# Ensure dplyr is on top
library(dplyr, warn.conflicts = FALSE)

# Set paths
ROOT <- "~/Desktop/dissertation"
DATA_ROOT <- file.path(ROOT, "data")
BCS_ROOT <- file.path(DATA_ROOT, "raw/bcs70")
OUT_DATA <- file.path(DATA_ROOT, "processed")
OUT_FIGS <- file.path(ROOT, "output/figures")

# Create output directories
dir.create(OUT_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIGS, recursive = TRUE, showWarnings = FALSE)

cat("Paths set:\n")
cat("  BCS root:", BCS_ROOT, "\n")
cat("  Output data:", OUT_DATA, "\n")
cat("  Output figs:", OUT_FIGS, "\n\n")

# =============================================================================
# 1. LOAD RAW DATA FILES
# =============================================================================

cat("--- LOADING RAW DATA ---\n")

# Helper: Load and report
load_and_report <- function(path, name) {
  cat(sprintf("Loading %-20s ... ", name))
  df <- haven::read_dta(path)
  cat(sprintf("%6d rows, %4d cols\n", nrow(df), ncol(df)))
  return(df)
}

# ---- Birth (confounders) ----
birth_der_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-2666-stata/stata/stata13/bcs1derived.dta"),
  "birth_derived"
)

birth_main_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-2666-stata/stata/stata13/bcs7072a.dta"),
  "birth_main"
)

# ---- Age 10 (childhood SES + cognition) ----
age10_der_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-3723-stata/stata/stata13/bcs3derived.dta"),
  "age10_derived"
)

# ---- Age 16 (adolescent cognition) ----
age16_der_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-3535-stata/stata/stata13/bcs4derived.dta"),
  "age16_derived"
)

age16_arith_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-6095-stata/stata/stata13/bcs70_16-year_arithmetic_data.dta"),
  "age16_arithmetic"
)

# ---- Age 30 (TREATMENT) ----
age30_der_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-5558-stata/stata/stata13_se/bcs6derived.dta"),
  "age30_derived"
)

age30_main_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-5558-stata/stata/stata13_se/bcs2000.dta"),
  "age30_main"
)

# ---- Age 42 (OUTCOME) ----
age42_der_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-7473-stata/stata/stata13/bcs70_2012_derived.dta"),
  "age42_derived"
)

age42_flat_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-7473-stata/stata/stata13/bcs70_2012_flatfile.dta"),
  "age42_flatfile"
)

# ---- Age 51 (sex + robustness) ----
age51_main_raw <- load_and_report(
  file.path(BCS_ROOT, "UKDA-9347-stata/stata/stata13/bcs11_age51_main.dta"),
  "age51_main"
)

cat("\nAll files loaded successfully.\n\n")

# =============================================================================
# 2. STANDARDIZE AND CLEAN
# =============================================================================

cat("--- STANDARDIZING DATA ---\n")

# Helper: Clean BCS70 data
clean_bcs <- function(df, name) {
  # Lowercase all names
  names(df) <- tolower(names(df))
  
  # ZAP LABELS FIRST (critical - prevents conversion errors)
  df <- haven::zap_labels(df)
  
  # Fix bcsid BEFORE doing anything else
  if ("bcsid" %in% names(df)) {
    # If bcsid is numeric, convert to character
    # If already character, keep as is
    if (is.numeric(df$bcsid)) {
      df$bcsid <- as.character(round(df$bcsid))
    } else {
      df$bcsid <- as.character(df$bcsid)
    }
  }
  
  # Convert BCS70 missing codes to NA
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::where(is.numeric),
    ~dplyr::if_else(.x %in% c(-1, -7, -8, -9), NA_real_, .x)
  ))
  
  # Remove any duplicate bcsids (keep first)
  n_before <- nrow(df)
  df <- dplyr::distinct(df, bcsid, .keep_all = TRUE)
  n_after <- nrow(df)
  
  if (n_before != n_after) {
    cat(sprintf("  %-20s: removed %d duplicates\n", name, n_before - n_after))
  }
  
  return(df)
}

# Clean all datasets
birth_der <- clean_bcs(birth_der_raw, "birth_derived")
birth_main <- clean_bcs(birth_main_raw, "birth_main")
age10_der <- clean_bcs(age10_der_raw, "age10_derived")
age16_der <- clean_bcs(age16_der_raw, "age16_derived")
age16_arith <- clean_bcs(age16_arith_raw, "age16_arithmetic")
age30_der <- clean_bcs(age30_der_raw, "age30_derived")
age30_main <- clean_bcs(age30_main_raw, "age30_main")
age42_der <- clean_bcs(age42_der_raw, "age42_derived")
age42_flat <- clean_bcs(age42_flat_raw, "age42_flatfile")
age51_main <- clean_bcs(age51_main_raw, "age51_main")

cat("All datasets standardized.\n\n")

# =============================================================================
# 3. EXTRACT VARIABLES
# =============================================================================

cat("--- EXTRACTING VARIABLES ---\n")

# ---- Birth ----
birth_vars <- birth_der |>
  dplyr::select(
    bcsid,
    bd1psoc,    # Social class
    bd1mage,    # Mother's age
    bd1agefb,   # Mother's age at first birth
    bd1cntry,   # Country
    bd1regn     # Region
  ) |>
  dplyr::left_join(
    birth_main |> dplyr::select(bcsid, a0166),  # Parity
    by = "bcsid"
  )

cat(sprintf("Birth variables: %d cases\n", nrow(birth_vars)))

# ---- Age 10 ----
age10_vars <- age10_der |>
  dplyr::select(
    bcsid,
    bd3read,    # Reading
    bd3maths,   # Maths
    bd3psoc,    # Social class
    bd3inc      # Income
  )

cat(sprintf("Age 10 variables: %d cases\n", nrow(age10_vars)))

# ---- Age 16 ----
age16_vars <- age16_der |>
  dplyr::select(
    bcsid,
    bd4read,    # Vocabulary
    bd4psoc,    # Social class
    bd4mal      # Malaise
  ) |>
  dplyr::left_join(
    age16_arith |> dplyr::select(bcsid, mathscore),
    by = "bcsid"
  )

cat(sprintf("Age 16 variables: %d cases\n", nrow(age16_vars)))

# ---- Age 30 (TREATMENT) ----
age30_vars <- age30_der |>
  dplyr::select(
    bcsid,
    hinvq00     # NVQ level (TREATMENT)
  )

# Add attitudes if available
att_vars <- c("wm1", "wm2", "wm3", "wm4", "wm5",
              "c1", "c2", "c3", "c4")
att_vars_avail <- intersect(att_vars, names(age30_main))

if (length(att_vars_avail) > 0) {
  age30_att <- age30_main |>
    dplyr::select(bcsid, dplyr::all_of(att_vars_avail))
  
  age30_vars <- age30_vars |>
    dplyr::left_join(age30_att, by = "bcsid")
}

cat(sprintf("Age 30 variables: %d cases, %d attitude vars\n", 
            nrow(age30_vars), length(att_vars_avail)))

# ---- Age 42 (OUTCOME) ----
age42_vars <- age42_der |>
  dplyr::select(
    bcsid,
    bd9totce,   # Total children (OUTCOME)
    bd9hnvq,    # NVQ level
    bd9ms,      # Marital status
    bd9partp    # Has partner
  )

# Add economic vars
econ_vars <- c("b9groa", "b9grow", "b9cns8", "b9ten", "b9cmsex")
econ_vars_avail <- intersect(econ_vars, names(age42_flat))

if (length(econ_vars_avail) > 0) {
  age42_econ <- age42_flat |>
    dplyr::select(bcsid, dplyr::all_of(econ_vars_avail))
  
  age42_vars <- age42_vars |>
    dplyr::left_join(age42_econ, by = "bcsid")
}

cat(sprintf("Age 42 variables: %d cases, %d econ vars\n",
            nrow(age42_vars), length(econ_vars_avail)))

# ---- Age 51 (sex) ----
age51_vars <- age51_main |>
  dplyr::select(
    bcsid,
    b11sex,     # Sex
    bd11nochh   # Children in household
  )

cat(sprintf("Age 51 variables: %d cases\n", nrow(age51_vars)))

cat("\nAll variables extracted.\n\n")

# =============================================================================
# 4. MERGE TO ANALYTICAL DATASET
# =============================================================================

cat("--- MERGING TO ANALYTICAL DATASET ---\n")

# Start with age 42 (has the outcome we care about)
df <- age42_vars

cat(sprintf("Starting with age 42: %d cases\n", nrow(df)))

# Add each sweep incrementally
df <- df |>
  dplyr::left_join(age30_vars, by = "bcsid") |>
  dplyr::left_join(age51_vars, by = "bcsid") |>
  dplyr::left_join(birth_vars, by = "bcsid") |>
  dplyr::left_join(age10_vars, by = "bcsid") |>
  dplyr::left_join(age16_vars, by = "bcsid")

cat(sprintf("After all merges: %d cases, %d variables\n", nrow(df), ncol(df)))

# Report key variable availability
cat("\nKey variable coverage:\n")
cat(sprintf("  Outcome (bd9totce):  %6d / %d (%.1f%%)\n", 
            sum(!is.na(df$bd9totce)), nrow(df),
            100 * mean(!is.na(df$bd9totce))))
cat(sprintf("  Treatment (hinvq00): %6d / %d (%.1f%%)\n",
            sum(!is.na(df$hinvq00)), nrow(df),
            100 * mean(!is.na(df$hinvq00))))
cat(sprintf("  Sex (b11sex):        %6d / %d (%.1f%%)\n",
            sum(!is.na(df$b11sex)), nrow(df),
            100 * mean(!is.na(df$b11sex))))

cat("\n")

# =============================================================================
# 5. CONSTRUCT ANALYTICAL VARIABLES
# =============================================================================

cat("--- CONSTRUCTING ANALYTICAL VARIABLES ---\n")

# Create analysis variables
df <- df |>
  dplyr::mutate(
    # ---- Core variables ----
    # Outcome: total children at age 42
    y = bd9totce,
    
    # Treatment: NVQ level at age 30 (0-4, collapse 5 into 4)
    d = dplyr::if_else(hinvq00 == 5, 4, hinvq00),
    
    # Sex: from age 51 (best coverage) or age 42 as backup
    sex_raw = dplyr::coalesce(b11sex, b9cmsex),
    female = dplyr::if_else(sex_raw == 2, 1L, 0L),
    
    # ---- Confounders ----
    # Social class (reverse code so higher = higher SES)
    soc_birth = 6L - as.integer(bd1psoc),
    soc_10 = 6L - as.integer(bd3psoc),
    soc_16 = 6L - as.integer(bd4psoc),
    
    # Mother's characteristics
    mage = bd1mage,
    mage_fb = bd1agefb,
    parity = a0166,
    birth_order = parity + 1L,
    
    # Cognition at age 10 (standardized)
    read_10_z = as.numeric(scale(bd3read)),
    math_10_z = as.numeric(scale(bd3maths)),
    cog_10 = (read_10_z + math_10_z) / 2,
    
    # Cognition at age 16 (standardized)
    vocab_16_z = as.numeric(scale(bd4read)),
    math_16_z = as.numeric(scale(mathscore)),
    cog_16 = (vocab_16_z + math_16_z) / 2,
    
    # Family background
    inc_10 = bd3inc,
    malaise_16 = bd4mal
  )

cat("Core variables constructed.\n")

# Check for mediator variables (only if they exist)
has_wm <- all(c("wm1", "wm2", "wm3") %in% names(df))
has_c <- all(c("c1", "c3") %in% names(df))

if (has_wm) {
  df <- df |>
    dplyr::mutate(
      m_trad_gender = rowMeans(cbind(wm1, wm2, wm3), na.rm = TRUE)
    )
  cat("Added mediator: traditional gender attitudes\n")
}

if (has_c) {
  df <- df |>
    dplyr::mutate(
      m_profamily = rowMeans(cbind(c1, c3), na.rm = TRUE)
    )
  cat("Added mediator: profamily attitudes\n")
}

if ("b9groa" %in% names(df)) {
  df <- df |>
    dplyr::mutate(
      m_earnings = b9groa,
      m_log_earn = log(b9groa + 1)
    )
  cat("Added mediator: earnings\n")
}

if ("bd9partp" %in% names(df)) {
  df <- df |>
    dplyr::mutate(
      m_partner = dplyr::if_else(bd9partp == 1, 1L, 0L)
    )
  cat("Added mediator: has partner\n")
}

cat("\n")

# =============================================================================
# 6. CREATE ANALYTICAL SAMPLE
# =============================================================================

cat("--- CREATING ANALYTICAL SAMPLE ---\n")

# Define analytical sample: non-missing y, d, female
df_analysis <- df |>
  dplyr::filter(
    !is.na(y),
    !is.na(d),
    !is.na(female)
  )

cat(sprintf("Analytical sample: %d cases (%.1f%% of merged data)\n",
            nrow(df_analysis),
            100 * nrow(df_analysis) / nrow(df)))

cat(sprintf("  Women: %d (%.1f%%)\n",
            sum(df_analysis$female == 1),
            100 * mean(df_analysis$female == 1)))

cat(sprintf("  Men:   %d (%.1f%%)\n",
            sum(df_analysis$female == 0),
            100 * mean(df_analysis$female == 0)))

cat("\n")

# Split by sex
df_women <- df_analysis |> dplyr::filter(female == 1)
df_men <- df_analysis |> dplyr::filter(female == 0)

cat(sprintf("Women sample: %d cases\n", nrow(df_women)))
cat(sprintf("Men sample:   %d cases\n", nrow(df_men)))

cat("\n")

# =============================================================================
# 7. DESCRIPTIVE STATISTICS
# =============================================================================

cat("--- DESCRIPTIVE STATISTICS ---\n\n")

# Overall summary
desc_overall <- df_analysis |>
  dplyr::summarise(
    n = dplyr::n(),
    y_mean = mean(y, na.rm = TRUE),
    y_sd = sd(y, na.rm = TRUE),
    d_mean = mean(d, na.rm = TRUE),
    d_sd = sd(d, na.rm = TRUE)
  )

cat("Overall sample:\n")
print(desc_overall)
cat("\n")

# By sex
desc_by_sex <- df_analysis |>
  dplyr::group_by(female) |>
  dplyr::summarise(
    n = dplyr::n(),
    y_mean = mean(y, na.rm = TRUE),
    y_sd = sd(y, na.rm = TRUE),
    d_mean = mean(d, na.rm = TRUE),
    d_sd = sd(d, na.rm = TRUE),
    .groups = "drop"
  )

cat("By sex:\n")
print(desc_by_sex)
cat("\n")

# Education-fertility crosstab
cat("Mean fertility by education level and sex:\n")
educ_fert <- df_analysis |>
  dplyr::group_by(d, female) |>
  dplyr::summarise(
    n = dplyr::n(),
    y_mean = mean(y, na.rm = TRUE),
    y_se = sd(y, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  )
print(educ_fert)
cat("\n")

# =============================================================================
# 8. VISUALIZE RAW GRADIENT
# =============================================================================

cat("--- CREATING VISUALIZATION ---\n")

# Plot education-fertility gradient
p_gradient <- educ_fert |>
  dplyr::mutate(sex_label = ifelse(female == 1, "Women", "Men")) |>
  ggplot(aes(x = d, y = y_mean, color = sex_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_errorbar(aes(ymin = y_mean - 1.96 * y_se,
                    ymax = y_mean + 1.96 * y_se),
                width = 0.1, alpha = 0.5) +
  scale_color_manual(
    values = c("Women" = "#2C7BB6", "Men" = "#D7191C"),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = 0:4,
    labels = c("None", "NVQ1", "NVQ2", "NVQ3", "NVQ4+")
  ) +
  labs(
    title = "Education-Fertility Gradient (Unadjusted)",
    subtitle = "BCS70 cohort, completed fertility at age 42",
    x = "Highest NVQ qualification at age 30",
    y = "Mean number of children",
    size = "N",
    caption = "Error bars: 95% confidence intervals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave(
  filename = file.path(OUT_FIGS, "h4_raw_gradient.png"),
  plot = p_gradient,
  width = 10, height = 6, dpi = 300
)

cat(sprintf("Saved: %s\n\n", file.path(OUT_FIGS, "h4_raw_gradient.png")))

# =============================================================================
# 9. SAVE ANALYTICAL DATASETS
# =============================================================================

cat("--- SAVING OUTPUTS ---\n")

# Save analytical data
saveRDS(
  object = list(
    df_full = df,
    df_analysis = df_analysis,
    df_women = df_women,
    df_men = df_men,
    descriptives = list(
      overall = desc_overall,
      by_sex = desc_by_sex,
      educ_fert = educ_fert
    )
  ),
  file = file.path(OUT_DATA, "h4_analytical_data.rds")
)

cat(sprintf("Saved: %s\n", file.path(OUT_DATA, "h4_analytical_data.rds")))

# Also save as CSV for inspection
readr::write_csv(
  df_analysis,
  file = file.path(OUT_DATA, "h4_analytical_data.csv")
)

cat(sprintf("Saved: %s\n", file.path(OUT_DATA, "h4_analytical_data.csv")))

cat("\n")

# =============================================================================
# 10. SUMMARY
# =============================================================================

cat("=== PROCESSING COMPLETE ===\n\n")

cat("Summary:\n")
cat(sprintf("  Total cases merged:     %d\n", nrow(df)))
cat(sprintf("  Analytical sample:      %d\n", nrow(df_analysis)))
cat(sprintf("    Women:                %d\n", nrow(df_women)))
cat(sprintf("    Men:                  %d\n", nrow(df_men)))
cat("\n")
cat("Next steps:\n")
cat("  1. Run DML total effect models (separate script)\n")
cat("  2. Add mediation decomposition\n")
cat("  3. Robustness checks\n\n")

cat("Session info:\n")
print(sessionInfo())

# =============================================================================
# END
# =============================================================================

