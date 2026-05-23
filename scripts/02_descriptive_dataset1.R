library(dplyr)
library(tidyr)

# ============================================
# DATASET 1: AGE-SPECIFIC FERTILITY RATES
# BY COUNTRY × YEAR × AGE GROUP × EDUCATION
# ============================================

# Load source files
fe   <- read.csv("data/raw/fertility by education /fertility by education .csv")
ea   <- read.csv("data/raw/educational attainment /educational attainment .csv")
pjan <- read.csv("data/raw/demo_pjangroup/demo_pjangroup.csv")

# Final 22-country sample
final_countries <- c(
  "Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia",
  "Finland", "Greece", "Hungary", "Latvia", "Montenegro", "North Macedonia",
  "Norway", "Poland", "Portugal", "Romania", "Serbia", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Türkiye"
)

# Three ISCED categories
isced_keep <- c(
  "Less than primary, primary and lower secondary education (levels 0-2)",
  "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)",
  "Tertiary education (levels 5-8)"
)

# Helper: map full ISCED label to short code
classify_edu <- function(x) case_when(
  grepl("levels 0-2", x) ~ "low",
  grepl("levels 3 and 4", x) ~ "medium",
  grepl("levels 5-8", x) ~ "high"
)

# ============================================
# 1. NUMERATOR: births by 5-year age group × education
#    (aggregate single-year ages to 5-year groups for consistency)
# ============================================

single_year_ages <- paste(15:49, "years")

fe_births <- fe |>
  filter(
    geo %in% final_countries,
    age %in% single_year_ages,
    isced11 %in% isced_keep
  ) |>
  mutate(
    age_num   = as.integer(gsub(" years?", "", age)),
    age_group = paste0(5L * (age_num %/% 5L), "-", 5L * (age_num %/% 5L) + 4L),
    education = classify_edu(isced11)
  ) |>
  group_by(country = geo, year = TIME_PERIOD, age_group, education) |>
  summarise(births = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop")

# ============================================
# 2. TOTAL FEMALE POPULATION by 5-year age group
# ============================================

pjan_ages <- c(
  "From 15 to 19 years", "From 20 to 24 years", "From 25 to 29 years",
  "From 30 to 34 years", "From 35 to 39 years", "From 40 to 44 years",
  "From 45 to 49 years"
)

pjan_total <- pjan |>
  filter(
    geo %in% final_countries,
    sex == "Females",
    age %in% pjan_ages
  ) |>
  mutate(age_group = sub("From (\\d+) to (\\d+) years", "\\1-\\2", age)) |>
  select(country = geo, year = TIME_PERIOD, age_group, total_pop = OBS_VALUE)

# ============================================
# 3. EDUCATION SHARE OF WOMEN by age group
#    edat_lfse_03 has 5-year groups for 15-34 and 10-year groups
#    for 35-44 and 45-54 — apply 10-year shares to both 5-year subgroups
# ============================================

ea_pct <- ea |>
  filter(
    geo %in% final_countries,
    sex == "Females",
    isced11 %in% isced_keep,
    age %in% c(
      "From 15 to 19 years", "From 20 to 24 years", "From 25 to 29 years",
      "From 30 to 34 years", "From 35 to 44 years", "From 45 to 54 years"
    )
  ) |>
  mutate(education = classify_edu(isced11)) |>
  select(country = geo, year = TIME_PERIOD, age, education,
         edu_pct = OBS_VALUE)

# Map ea age categories to our 5-year age groups
ea_pct_5yr <- ea_pct |>
  mutate(target = case_when(
    age == "From 15 to 19 years" ~ list("15-19"),
    age == "From 20 to 24 years" ~ list("20-24"),
    age == "From 25 to 29 years" ~ list("25-29"),
    age == "From 30 to 34 years" ~ list("30-34"),
    age == "From 35 to 44 years" ~ list(c("35-39", "40-44")),
    age == "From 45 to 54 years" ~ list("45-49")
  )) |>
  unnest(target) |>
  select(country, year, age_group = target, education, edu_pct)

# ============================================
# 4. EDUCATION-SPECIFIC POPULATION = TOTAL × SHARE
# ============================================

pop_edu <- pjan_total |>
  inner_join(ea_pct_5yr, by = c("country", "year", "age_group")) |>
  mutate(population = total_pop * edu_pct / 100) |>
  select(country, year, age_group, education, population)

# ============================================
# 5. ASFR = BIRTHS / POPULATION
# ============================================

asfr_data <- fe_births |>
  inner_join(pop_edu, by = c("country", "year", "age_group", "education")) |>
  mutate(asfr = births / population) |>
  select(country, year, age_group, education, births, population, asfr) |>
  arrange(country, year, education, age_group)

# ============================================
# 6. DIAGNOSTICS + SAVE
# ============================================

cat("=== ASFR dataset built ===\n")
cat("  Rows:", nrow(asfr_data), "\n")
cat("  Countries:", length(unique(asfr_data$country)), "\n")
cat("  Year range:", paste(range(asfr_data$year), collapse = "-"), "\n")
cat("  Age groups:", paste(sort(unique(asfr_data$age_group)), collapse = ", "), "\n")
cat("  Education levels:", paste(sort(unique(asfr_data$education)), collapse = ", "), "\n\n")

cat("Sample (Austria, 2020):\n")
print(asfr_data |> filter(country == "Austria", year == 2020))

saveRDS(asfr_data, "data/derived/asfr_data.rds")
cat("\nSaved to data/derived/asfr_data.rds\n")
