# ============================================================================
# 06_dataset2_predictors.R
# DATASET 2 (H3) — PART 2: Load and process country-level predictors
# MSc Applied Social Data Science, TCD
#
# Loads raw cultural, economic, and demographic predictors from external
# sources, harmonizes country names, computes panel-period means, and
# merges with outcomes from Phase 1.
#
# Inputs:  data/raw/V-Dem-CY-FullOthers-v16_csv/V-Dem-CY-Full+Others-v16.csv
#          data/raw/ESS_data/Datafile-subset.csv
#          data/raw/GDP per capita annual/GDP Per Capita Annual.csv
#          data/raw/female labour force participation/female_labourforce_participation.csv
#          data/raw/contraceptive prevelancerate/API_SP.DYN.CONU.ZS_DS2_en_csv_v2_2894.csv
#          data/derived/dataset2_outcomes.rds (from Phase 1)
# Outputs: data/derived/dataset2_full.rds
# ============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(countrycode)  # install.packages("countrycode") if needed

setwd("/Users/whiz/Desktop/dissertation")

# ── 0. Load outcomes from Phase 1 ───────────────────────────────────────────

outcomes <- readRDS("data/derived/dataset2_outcomes.rds")

cat("=== Phase 1 outcomes loaded ===\n")
cat("Countries:", nrow(outcomes), "\n")
print(outcomes$country)

# ── 1. Load V-Dem (cultural indicators) ──────────────────────────────────────

vdem_raw <- read_csv("data/raw/V-Dem-CY-FullOthers-v16_csv/V-Dem-CY-Full+Others-v16.csv",
                     show_col_types = FALSE)

cat("\n=== V-Dem loaded ===\n")
cat("Rows:", nrow(vdem_raw), "| Columns:", ncol(vdem_raw), "\n")

# Filter to our 21 countries and 2007-2024 period, extract key indicators
vdem <- vdem_raw |>
  filter(year >= 2007, year <= 2024) |>
  select(
    country_name,
    year,
    # Cultural/institutional indicators
    v2x_libdem,      # Liberal democracy index
    v2x_gender,      # Gender equality index
    v2clrelig,       # Freedom of religion
    v2x_polyarchy,   # Electoral democracy index
    v2x_egaldem      # Egalitarian democracy index
  )

# Compute country-level means across 2007-2024
vdem_country <- vdem |>
  group_by(country_name) |>
  summarise(
    libdem       = mean(v2x_libdem, na.rm = TRUE),
    gender_equal = mean(v2x_gender, na.rm = TRUE),
    relig_free   = mean(v2clrelig, na.rm = TRUE),
    polyarchy    = mean(v2x_polyarchy, na.rm = TRUE),
    egaldem      = mean(v2x_egaldem, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== V-Dem countries found ===\n")
print(vdem_country$country_name)

# ── 2. ESS skipped (file too large, V-Dem covers cultural indicators) ────────

cat("\n=== ESS skipped ===\n")
cat("V-Dem gender equality and liberal democracy indices used instead.\n")
ess_country <- NULL

# ── 3. Load GDP per capita (OECD long format) ────────────────────────────────

gdp_raw <- read_csv("data/raw/GDP per capita annual/GDP Per Capita Annual.csv",
                    show_col_types = FALSE)

cat("\n=== GDP loaded ===\n")
cat("Rows:", nrow(gdp_raw), "| Columns:", ncol(gdp_raw), "\n")

# Filter to 2007-2024, compute country means
gdp_country <- gdp_raw |>
  filter(TIME_PERIOD >= 2007, TIME_PERIOD <= 2024) |>
  mutate(OBS_VALUE = as.numeric(OBS_VALUE)) |>
  group_by(`Reference area`) |>
  summarise(gdp_percap = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop") |>
  rename(country_name = `Reference area`)

cat("\n=== GDP countries (first 20) ===\n")
print(head(gdp_country$country_name, 20))

# ── 4. Load Female Labor Force Participation (OECD long format) ──────────────

flfp_raw <- read_csv("data/raw/female labour force participation/female_labourforce_participation.csv",
                     show_col_types = FALSE)

cat("\n=== FLFP loaded ===\n")
cat("Rows:", nrow(flfp_raw), "| Columns:", ncol(flfp_raw), "\n")

# Check if it has the same structure as GDP
if ("TIME_PERIOD" %in% names(flfp_raw) & "OBS_VALUE" %in% names(flfp_raw)) {
  flfp_country <- flfp_raw |>
    filter(TIME_PERIOD >= 2007, TIME_PERIOD <= 2024) |>
    mutate(OBS_VALUE = as.numeric(OBS_VALUE)) |>
    group_by(`Reference area`) |>
    summarise(flfp = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop") |>
    rename(country_name = `Reference area`)
  
  cat("\n=== FLFP countries (first 20) ===\n")
  print(head(flfp_country$country_name, 20))
} else {
  cat("\nNote: FLFP has different structure. Skipping for now.\n")
  flfp_country <- NULL
}

# ── 5. Load Contraceptive Prevalence ─────────────────────────────────────────

contra_raw <- read_csv("data/raw/contraceptive prevelancerate/API_SP.DYN.CONU.ZS_DS2_en_csv_v2_2894.csv",
                       skip = 4,  # World Bank CSVs have header rows
                       show_col_types = FALSE)

cat("\n=== Contraceptive prevalence loaded ===\n")
cat("Rows:", nrow(contra_raw), "| Columns:", ncol(contra_raw), "\n")

# Compute 2007-2024 mean
contra_years_present <- gdp_years[gdp_years %in% names(contra_raw)]

if (length(contra_years_present) > 0) {
  contra_country <- contra_raw |>
    select(`Country Name`, all_of(contra_years_present)) |>
    mutate(across(all_of(contra_years_present), as.numeric)) |>
    rowwise() |>
    mutate(contraceptive = mean(c_across(all_of(contra_years_present)), na.rm = TRUE)) |>
    ungroup() |>
    select(`Country Name`, contraceptive) |>
    rename(country_name = `Country Name`)
  
  cat("\n=== Contraceptive countries ===\n")
  print(contra_country$country_name)
} else {
  cat("\nNote: Contraceptive year columns not found. Check CSV structure.\n")
  contra_country <- NULL
}

# ── 6. Harmonize country names ───────────────────────────────────────────────
# Use countrycode package to standardize to ISO3 or consistent names

# Create a mapping from our 21 countries to standard names
country_mapping <- data.frame(
  country = outcomes$country,
  country_std = countrycode(outcomes$country, 
                            origin = "country.name", 
                            destination = "country.name",
                            warn = FALSE)
)

cat("\n=== Country name harmonization ===\n")
print(country_mapping)

# Apply harmonization to predictor datasets
vdem_country <- vdem_country |>
  mutate(country_std = countrycode(country_name,
                                   origin = "country.name",
                                   destination = "country.name",
                                   warn = FALSE))

if (!is.null(gdp_country)) {
  gdp_country <- gdp_country |>
    mutate(country_std = countrycode(country_name,
                                     origin = "country.name",
                                     destination = "country.name",
                                     warn = FALSE))
}

if (!is.null(flfp_country)) {
  flfp_country <- flfp_country |>
    mutate(country_std = countrycode(country_name,
                                     origin = "country.name",
                                     destination = "country.name",
                                     warn = FALSE))
}

if (!is.null(contra_country)) {
  contra_country <- contra_country |>
    mutate(country_std = countrycode(country_name,
                                     origin = "country.name",
                                     destination = "country.name",
                                     warn = FALSE))
}

# ── 7. Merge predictors with outcomes ────────────────────────────────────────

# Start with outcomes, add harmonized country names
dataset2 <- outcomes |>
  left_join(country_mapping, by = "country")

# Merge V-Dem
dataset2 <- dataset2 |>
  left_join(vdem_country |> select(country_std, libdem, gender_equal, relig_free, polyarchy, egaldem),
            by = "country_std")

# Merge GDP
if (!is.null(gdp_country)) {
  dataset2 <- dataset2 |>
    left_join(gdp_country |> select(country_std, gdp_percap),
              by = "country_std")
}

# Merge FLFP
if (!is.null(flfp_country)) {
  dataset2 <- dataset2 |>
    left_join(flfp_country |> select(country_std, flfp),
              by = "country_std")
}

# Merge contraceptive prevalence
if (!is.null(contra_country)) {
  dataset2 <- dataset2 |>
    left_join(contra_country |> select(country_std, contraceptive),
              by = "country_std")
}

# ── 8. Add regional/historical indicators ────────────────────────────────────

dataset2 <- dataset2 |>
  mutate(
    # Post-socialist indicator
    post_socialist = as.integer(country %in% c(
      "Croatia", "Czechia", "Estonia", "Hungary", "Latvia", "Poland",
      "Romania", "Serbia", "Slovakia", "Slovenia", "North Macedonia"
    )),
    
    # Geographic region
    region = case_when(
      country %in% c("Denmark", "Finland", "Norway", "Sweden") ~ "Nordic",
      country %in% c("Austria", "Belgium") ~ "Western",
      country %in% c("Greece", "Portugal", "Spain") ~ "Southern",
      country %in% c("Croatia", "Czechia", "Estonia", "Hungary", "Latvia",
                     "Poland", "Slovakia", "Slovenia") ~ "Eastern",
      country %in% c("North Macedonia", "Romania", "Serbia") ~ "Balkan",
      country == "Türkiye" ~ "Other",
      TRUE ~ NA_character_
    )
  )

# ── 9. Summary and save ───────────────────────────────────────────────────────

cat("\n\n=== DATASET 2 FULL (outcomes + predictors) ===\n")
print(dataset2, n = Inf)

cat("\n=== Missing value summary ===\n")
missing_summary <- dataset2 |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

print(missing_summary)

cat("\n=== Dataset dimensions ===\n")
cat("Rows:", nrow(dataset2), "| Columns:", ncol(dataset2), "\n")

# Save
saveRDS(dataset2, "data/derived/dataset2_full.rds")

cat("\nSaved to data/derived/dataset2_full.rds\n")

# ── 10. Methodology log ───────────────────────────────────────────────────────

cat("
====================================================================
METHODOLOGY LOG — NEW ENTRY FROM DATASET 2 BUILD
====================================================================

35. Dataset 2 construction (Part 2: Predictors). Country-level predictors
    merged with outcomes from Part 1. N = 21 countries. Predictors:
    
    CULTURAL (V-Dem 2007–2024 means):
    - libdem: Liberal democracy index
    - gender_equal: Gender equality index
    - relig_free: Freedom of religion
    - polyarchy: Electoral democracy index
    - egaldem: Egalitarian democracy index
    
    ECONOMIC (World Bank/OECD 2007–2024 means):
    - gdp_percap: GDP per capita (constant USD)
    - flfp: Female labor force participation rate
    - contraceptive: Contraceptive prevalence rate
    
    REGIONAL/HISTORICAL:
    - post_socialist: Binary (1 = Eastern European post-socialist)
    - region: Categorical (Nordic, Western, Southern, Eastern, Balkan)
    
    Missing data handled via multiple imputation in H3 Stage 1 (XGBoost
    handles missingness natively; Bayesian Stage 2 uses informative priors).
    
====================================================================
")

cat("\nDone. Dataset 2 ready for H3 modelling.\n")
