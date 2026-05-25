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

# Which countries actually made it into the final dataset?
cat("Countries in asfr_data:\n")
print(sort(unique(asfr_data$country)))

# Compare against intended set
final_countries <- c(
  "Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia",
  "Finland", "Greece", "Hungary", "Latvia", "Montenegro", "North Macedonia",
  "Norway", "Poland", "Portugal", "Romania", "Serbia", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Türkiye"
)
cat("\nMissing country:\n")
print(setdiff(final_countries, unique(asfr_data$country)))

# Where did Austria get lost? Check each step
cat("\nAustria presence at each step:\n")
cat("  fe_births:", nrow(fe_births |> filter(country == "Austria")), "rows\n")
cat("  pjan_total:", nrow(pjan_total |> filter(country == "Austria")), "rows\n")
cat("  ea_pct_5yr:", nrow(ea_pct_5yr |> filter(country == "Austria")), "rows\n")
cat("  pop_edu:", nrow(pop_edu |> filter(country == "Austria")), "rows\n")

# Where did Montenegro get lost?
cat("Montenegro presence at each step:\n")
cat("  fe_births:", nrow(fe_births |> filter(country == "Montenegro")), "rows\n")
cat("  pjan_total:", nrow(pjan_total |> filter(country == "Montenegro")), "rows\n")
cat("  ea_pct_5yr:", nrow(ea_pct_5yr |> filter(country == "Montenegro")), "rows\n")
cat("  pop_edu:", nrow(pop_edu |> filter(country == "Montenegro")), "rows\n")

# What years does Austria actually have?
cat("\nAustria year coverage in asfr_data:\n")
print(sort(unique(asfr_data$year[asfr_data$country == "Austria"])))

# ── Why Montenegro vanishes at the final join ─────────────────────────────────
mfb <- fe_births |> filter(country == "Montenegro")
mpe <- pop_edu   |> filter(country == "Montenegro")

cat("Years in fe_births Montenegro :", paste(sort(unique(mfb$year)), collapse=", "), "\n")
cat("Years in pop_edu   Montenegro :", paste(sort(unique(mpe$year)), collapse=", "), "\n")
cat("Year overlap                  :", length(intersect(mfb$year, mpe$year)), "\n\n")

cat("Age groups fe_births          :", paste(sort(unique(mfb$age_group)), collapse=", "), "\n")
cat("Age groups pop_edu            :", paste(sort(unique(mpe$age_group)), collapse=", "), "\n\n")

cat("Education fe_births           :", paste(sort(unique(mfb$education)), collapse=", "), "\n")
cat("Education pop_edu             :", paste(sort(unique(mpe$education)), collapse=", "), "\n\n")

cat("Inner-join result             :", 
    nrow(inner_join(mfb, mpe, by = c("country","year","age_group","education"))), 
    "rows\n")

# ── Year coverage audit across ALL countries (the bigger issue) ──────────────
cat("\nYear coverage by country in asfr_data:\n")
asfr_data |>
  group_by(country) |>
  summarise(
    n_years   = n_distinct(year),
    first_yr  = min(year),
    last_yr   = max(year),
    .groups   = "drop"
  ) |>
  arrange(n_years) |>
  print(n = Inf)

# ── Verify Austria's truncation is a data issue, not a join bug ──────────────
cat("Austria years in each source:\n")
cat("  fe_births :", paste(sort(unique(fe_births$year[fe_births$country == "Austria"])), collapse=", "), "\n")
cat("  pjan_total:", paste(sort(unique(pjan_total$year[pjan_total$country == "Austria"])), collapse=", "), "\n")
cat("  ea_pct_5yr:", paste(sort(unique(ea_pct_5yr$year[ea_pct_5yr$country == "Austria"])), collapse=", "), "\n")


# ── Drop Montenegro and rebuild cleanly ──────────────────────────────────────
# (Already effectively done — Montenegro is already absent from asfr_data,
#  but make the exclusion explicit for documentation.)

final_countries_v2 <- setdiff(final_countries, "Montenegro")
asfr_data <- asfr_data |> filter(country %in% final_countries_v2)

# ── Compute period eTFR by country × year × education ────────────────────────
etfr_data <- asfr_data |>
  group_by(country, year, education) |>
  summarise(
    n_age_groups = n(),
    etfr         = 5 * sum(asfr, na.rm = TRUE),
    .groups      = "drop"
  )

# ── Plausibility check ───────────────────────────────────────────────────────
cat("Coverage: should be 7 age groups per cell\n")
print(table(etfr_data$n_age_groups))

cat("\neTFR distribution by education:\n")
etfr_data |>
  group_by(education) |>
  summarise(
    min  = round(min(etfr,  na.rm = TRUE), 3),
    p25  = round(quantile(etfr, 0.25, na.rm = TRUE), 3),
    mean = round(mean(etfr, na.rm = TRUE), 3),
    p75  = round(quantile(etfr, 0.75, na.rm = TRUE), 3),
    max  = round(max(etfr,  na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  print()

# Expected: low ~1.8–2.5, medium ~1.4–1.9, high ~1.2–1.7
cat("\nImplausible eTFR rows (>4 or <0.3):\n")
print(etfr_data |> filter(etfr > 4 | etfr < 0.3))

# ── Save both datasets ───────────────────────────────────────────────────────
saveRDS(asfr_data, "data/derived/asfr_data.rds")
saveRDS(etfr_data, "data/derived/etfr_data.rds")
cat("\nasfr_data rows:", nrow(asfr_data), "\n")
cat("etfr_data rows:", nrow(etfr_data), "\n")

# ── Gradient over time: when did high overtake medium? ──────────────────────
gradient_by_year <- etfr_data |>
  group_by(year, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), 
            n_countries = n_distinct(country),
            .groups = "drop") |>
  pivot_wider(names_from = education, values_from = c(mean_etfr, n_countries)) |>
  mutate(
    gap_low_med   = round(mean_etfr_low    - mean_etfr_medium, 3),
    gap_med_high  = round(mean_etfr_medium - mean_etfr_high,   3),
    monotonic_neg = mean_etfr_low > mean_etfr_medium & mean_etfr_medium > mean_etfr_high
  ) |>
  select(year, low = mean_etfr_low, medium = mean_etfr_medium, high = mean_etfr_high,
         gap_low_med, gap_med_high, monotonic_neg, n = n_countries_low) |>
  arrange(year)

print(gradient_by_year, n = Inf)

# ── Gradient by country: which countries show the J-curve? ──────────────────
gradient_by_country <- etfr_data |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_etfr) |>
  mutate(
    gap_med_high  = round(medium - high, 3),
    monotonic_neg = low > medium & medium > high,
    j_curve       = low > high & high > medium
  ) |>
  arrange(gap_med_high)

print(gradient_by_country, n = Inf)

# ── Sanity: is medium-low really driven by the long-panel Eastern bloc? ─────
cat("\nCountries where J-curve pattern holds (high > medium):\n")
print(gradient_by_country |> filter(j_curve) |> pull(country))

cat("\nCountries where monotonic negative gradient holds (low > med > high):\n")
print(gradient_by_country |> filter(monotonic_neg) |> pull(country))


# ── 1. Check whether "low" eTFR tracks share of population in "low" tier ─────
# If "low" is a small, selected group (e.g., Roma in Eastern Europe),
# we'd expect HIGH eTFR in low to coincide with SMALL share of population in low

share_by_country <- ea_pct_5yr |>
  filter(age_group %in% c("25-29", "30-34", "35-39")) |>
  group_by(country, education) |>
  summarise(mean_share_pct = mean(edu_pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_share_pct, 
              names_prefix = "share_")

gradient_with_shares <- gradient_by_country |>
  left_join(share_by_country, by = "country") |>
  select(country, low, medium, high, share_low, share_medium, share_high, 
         j_curve, monotonic_neg) |>
  arrange(share_low)

print(gradient_with_shares, n = Inf)

# ── 2. Visualise the gradient shape per country ──────────────────────────────
# Quick sanity check that the pattern looks like what the numbers suggest

library(ggplot2)

etfr_data |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  mutate(education = factor(education, levels = c("low", "medium", "high"))) |>
  ggplot(aes(x = education, y = mean_etfr, group = country)) +
  geom_line(colour = "grey60") +
  geom_point() +
  facet_wrap(~ country, ncol = 5) +
  labs(title = "Education-fertility gradient by country (mean across 2007-2024)",
       y = "Period eTFR", x = NULL) +
  theme_minimal()

ggsave("output/figures/gradient_by_country.png", width = 12, height = 8)

# ── 3. Quick robustness: gradient excluding 2020-2021 (pandemic shock) ──────
gradient_no_covid <- etfr_data |>
  filter(!year %in% c(2020, 2021)) |>
  group_by(country, education) |>
  summarise(mean_etfr = mean(etfr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_etfr) |>
  mutate(j_curve = low > high & high > medium) |>
  arrange(country)

cat("Countries with J-curve excluding pandemic years:\n")
print(gradient_no_covid |> filter(j_curve) |> pull(country))
