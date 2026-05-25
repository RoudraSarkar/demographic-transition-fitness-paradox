library(dplyr)

# --- Define what we want to keep ---

# 35 usable countries (drop aggregates)
aggregates <- c(
  "Euro area – 20 countries (2023-2025)",
  "Euro area – 21 countries (from 2026)",
  "European Union - 27 countries (from 2020)",
  "European Free Trade Association"
)
countries <- setdiff(common, aggregates)

# Five-year age groups we want (for fertility)
age_groups_5yr <- c(
  "From 15 to 19 years",
  "From 20 to 24 years",
  "From 25 to 29 years",
  "From 30 to 34 years",
  "From 35 to 39 years",
  "From 40 to 44 years",
  "From 45 to 49 years"
)

# Three meaningful ISCED categories
isced_keep <- c(
  "Less than primary, primary and lower secondary education (levels 0-2)",
  "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)",
  "Tertiary education (levels 5-8)"
)

# --- Filter the fertility (numerator) file ---

fe_clean <- fe |>
  filter(
    geo %in% countries,
    age %in% age_groups_5yr,
    isced11 %in% isced_keep
  ) |>
  select(geo, age, isced11, TIME_PERIOD, OBS_VALUE) |>
  rename(country = geo, year = TIME_PERIOD, births = OBS_VALUE)

# --- Filter the population (denominator) file ---
# For population we have to use 10-year bands at ages 35+, so we keep what's available

ea_age_groups <- c(
  "From 15 to 19 years",
  "From 20 to 24 years",
  "From 25 to 29 years",
  "From 30 to 34 years",
  "From 35 to 44 years",   # 10-year band — will need splitting later
  "From 45 to 54 years"    # 10-year band — will need splitting later
)

ea_clean <- ea |>
  filter(
    geo %in% countries,
    sex == "Females",
    age %in% ea_age_groups,
    isced11 %in% isced_keep
  ) |>
  select(geo, age, isced11, TIME_PERIOD, OBS_VALUE) |>
  rename(country = geo, year = TIME_PERIOD, pct_population = OBS_VALUE)

# --- Inspect what we have ---

cat("Filtered fertility data:\n")
cat("  Rows:", nrow(fe_clean), "\n")
cat("  Countries:", length(unique(fe_clean$country)), "\n")
cat("  Years:", paste(range(fe_clean$year), collapse = "-"), "\n")
cat("  ISCED levels:", length(unique(fe_clean$isced11)), "\n\n")

cat("Filtered population data:\n")
cat("  Rows:", nrow(ea_clean), "\n")
cat("  Countries:", length(unique(ea_clean$country)), "\n")
cat("  Years:", paste(range(ea_clean$year), collapse = "-"), "\n")
cat("  ISCED levels:", length(unique(ea_clean$isced11)), "\n")

# --- Save cleaned intermediates ---

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
saveRDS(fe_clean, "data/derived/fe_clean.rds")
saveRDS(ea_clean, "data/derived/ea_clean.rds")

cat("\nSaved to data/derived/\n")


# Which countries got dropped between the original fe and fe_clean?
dropped <- setdiff(common, c(unique(fe_clean$country), aggregates))
cat("Countries dropped from fertility analysis (", length(dropped), "):\n")
print(dropped)

# For one dropped country, what age categories DO they have in the original?
sample_country <- dropped[1]
cat("\nFor", sample_country, ", available age × ISCED combinations:\n")
fe |>
  filter(geo == sample_country, isced11 %in% isced_keep) |>
  count(age) |>
  print(n = 100)

sample_country <- "Austria"
cat("For", sample_country, ", available age × ISCED combinations:\n")
result <- fe |>
  filter(geo == sample_country, isced11 %in% isced_keep) |>
  count(age)
print(as.data.frame(result))


single_year_ages <- paste(15:49, "years")

coverage_check <- fe |>
  filter(
    geo %in% countries,
    isced11 %in% isced_keep,
    age %in% single_year_ages
  ) |>
  group_by(geo) |>
  summarise(n_single_year_obs = n(), .groups = "drop") |>
  arrange(n_single_year_obs)

print(as.data.frame(coverage_check))

still_missing <- c("Bulgaria", "Switzerland", "Cyprus", "Germany", "France", 
                   "Ireland", "Iceland", "Italy", "Lithuania", "Luxembourg", 
                   "Malta", "Netherlands")

# Check ALL ISCED categories (not just the three we filtered for) 
# to see what these countries actually have
for (country in still_missing) {
  cat("\n===", country, "===\n")
  ages_isced <- fe |>
    filter(geo == country) |>
    count(isced11)
  print(as.data.frame(ages_isced))
}
