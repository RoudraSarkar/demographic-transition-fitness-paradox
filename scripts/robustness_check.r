library(dplyr)
library(fixest)
setwd("~/Desktop/dissertation")

asfr <- readRDS("data/derived/asfr_data.rds")

# Compute total female population per country-year-education cell
pop_weights <- asfr |>
  group_by(country, year, education) |>
  summarise(pop_weight = sum(population, na.rm = TRUE), .groups = "drop")

# Join into etfr_data
etfr <- readRDS("data/derived/etfr_data.rds") |>
  mutate(log_etfr = log(etfr)) |>
  left_join(pop_weights, by = c("country", "year", "education"))

cat("Weights joined — missing:", sum(is.na(etfr$pop_weight)), "\n")

# Run weighted model
m1_weighted <- feols(
  log_etfr ~ education | country + year,
  data    = etfr,
  weights = ~pop_weight,
  cluster = "country"
)

cat("\n=== M1 (population-weighted) ===\n")
summary(m1_weighted)

# Compare side by side with unweighted
m1_unweighted <- feols(
  log_etfr ~ education | country + year,
  data    = etfr,
  cluster = "country"
)

cat("\n=== M1 (unweighted — for comparison) ===\n")
summary(m1_unweighted)
