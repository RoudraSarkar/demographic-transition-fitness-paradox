# ============================================================================
# 10_diagnostic_agefloor.R
# DIAGNOSTIC — Where does the education-fertility gradient come from?
#
# Context: 09_robustness_age2549.R found that restricting the eTFR to ages
# 25-49 REVERSES the strict H1 contrast (high vs low) from -0.187 to +0.201.
# The entire negative gradient lives in ages 15-24. This script asks *why*,
# and whether the reversal is an artefact of how education is measured at
# young ages or a real feature of the fertility process.
#
# Two competing readings:
#   (A) ARTEFACT. At 15-19 nearly every woman is classified "low" because she
#       has not yet completed upper secondary, and almost none is "high"
#       because a tertiary degree is not attainable at 17. eTFR_low therefore
#       absorbs the whole teenage fertility rate while eTFR_high mechanically
#       receives nothing from those ages. That is construction, not behaviour.
#   (B) MECHANISM. Early childbearing IS the channel through which education
#       depresses fertility (Skirbekk's enrolment effect). Dropping 15-24
#       removes the causal pathway by construction.
#
# The decisive evidence is Section 3: the education composition of women BY
# AGE GROUP. If the low tier holds ~90%+ of women at 15-19 and the high tier
# ~0%, reading (A) is strongly supported for that age band specifically.
#
# ============================================================================

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)

setwd("/Users/whiz/Desktop/dissertation")
set.seed(42)

asfr_data <- readRDS("data/derived/asfr_data.rds")
etfr_full <- readRDS("data/derived/etfr_data.rds")

all_ages <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
stopifnot(all(all_ages %in% unique(asfr_data$age_group)))

cat("=== Inputs ===\n")
cat("asfr_data:", nrow(asfr_data), "rows,", n_distinct(asfr_data$country), "countries\n\n")

# ── 1. AGE-FLOOR SWEEP ───────────────────────────────────────────────────────
# Rebuild the eTFR three times, raising the age floor each time, and re-run
# the H1 tests. This localises the reversal: if it is driven by 15-19 alone,
# the 20-49 panel should look much like the 15-49 panel.

build_etfr <- function(floor_age) {
  keep <- all_ages[as.integer(substr(all_ages, 1, 2)) >= floor_age]
  asfr_data |>
    filter(age_group %in% keep) |>
    group_by(country, year, education) |>
    summarise(etfr = 5 * sum(asfr, na.rm = TRUE), .groups = "drop") |>
    mutate(
      log_etfr  = log(etfr),
      education = factor(education, levels = c("medium", "low", "high"))
    )
}

fit_all <- function(d) {
  m_ref_med <- feols(log_etfr ~ education | country + year,
                     data = d, cluster = "country")
  m_ref_low <- feols(log_etfr ~ education | country + year,
                     data = d |> mutate(education = relevel(education, ref = "low")),
                     cluster = "country")
  dq <- d |>
    mutate(educ_num = case_when(education == "low" ~ 0,
                                education == "medium" ~ 1,
                                education == "high" ~ 2),
           educ_num_sq = educ_num^2)
  m_quad <- feols(log_etfr ~ educ_num + educ_num_sq | country + year,
                  data = dq, cluster = "country")
  list(ref_med = m_ref_med, ref_low = m_ref_low, quad = m_quad)
}

floors <- c(15, 20, 25)
panels <- setNames(lapply(floors, build_etfr), paste0("floor_", floors))
models <- lapply(panels, fit_all)

sweep_tab <- lapply(seq_along(floors), function(i) {
  f  <- floors[i]
  m  <- models[[i]]
  tm <- broom::tidy(m$ref_med)
  tl <- broom::tidy(m$ref_low)
  tq <- broom::tidy(m$quad)
  b1 <- tq$estimate[tq$term == "educ_num"]
  b2 <- tq$estimate[tq$term == "educ_num_sq"]
  mn <- if (b2 > 0) -b1 / (2 * b2) else NA_real_
  data.frame(
    age_range   = paste0(f, "-49"),
    n_age_grps  = length(all_ages) - (f - 15) / 5,
    N           = nobs(m$ref_med),
    low_vs_med  = round(tm$estimate[tm$term == "educationlow"], 3),
    low_p       = round(tm$p.value[tm$term == "educationlow"], 3),
    high_vs_med = round(tm$estimate[tm$term == "educationhigh"], 3),
    high_p      = round(tm$p.value[tm$term == "educationhigh"], 3),
    strict_H1   = round(tl$estimate[tl$term == "educationhigh"], 3),
    strict_p    = round(tl$p.value[tl$term == "educationhigh"], 3),
    beta2       = round(b2, 3),
    beta2_p     = round(tq$p.value[tq$term == "educ_num_sq"], 3),
    curve_min   = round(mn, 3),
    row.names   = NULL
  )
}) |> bind_rows()

cat("=== SECTION 1: AGE-FLOOR SWEEP ===\n")
cat("strict_H1 is the high-vs-low contrast: negative = H1 holds.\n")
cat("Watch for the sign flip and identify which floor causes it.\n\n")
print(sweep_tab)

cat("\n--- Sign of strict H1 by floor ---\n")
for (i in seq_len(nrow(sweep_tab))) {
  cat(sprintf("  %-7s : %+.3f (p = %.3f)  %s\n",
              sweep_tab$age_range[i], sweep_tab$strict_H1[i], sweep_tab$strict_p[i],
              ifelse(sweep_tab$strict_H1[i] < 0, "H1 holds", "H1 REVERSED")))
}

# Tier means at each floor
tier_means <- lapply(seq_along(floors), function(i) {
  panels[[i]] |>
    group_by(education) |>
    summarise(mean_etfr = round(mean(etfr, na.rm = TRUE), 3), .groups = "drop") |>
    mutate(age_range = paste0(floors[i], "-49"))
}) |> bind_rows() |>
  pivot_wider(names_from = education, values_from = mean_etfr) |>
  select(age_range, low, medium, high) |>
  mutate(
    gap_low_high   = round(low - high, 3),
    medium_lowest  = medium < low & medium < high
  )

cat("\n--- Pooled tier means by age floor ---\n")
print(tier_means)

# ── 2. WHERE DOES EACH TIER'S FERTILITY LIVE? ────────────────────────────────
# Decompose each tier's eTFR by age group. Shows directly how much of the
# low tier's total is contributed by ages 15-24.

contrib <- asfr_data |>
  group_by(education, age_group) |>
  summarise(mean_asfr = mean(asfr, na.rm = TRUE), .groups = "drop") |>
  group_by(education) |>
  mutate(
    etfr_contrib = 5 * mean_asfr,
    pct_of_tier  = round(100 * etfr_contrib / sum(etfr_contrib), 1)
  ) |>
  ungroup() |>
  select(education, age_group, etfr_contrib, pct_of_tier) |>
  mutate(education = factor(education, levels = c("low", "medium", "high"))) |>
  arrange(education, age_group)

cat("\n\n=== SECTION 2: eTFR CONTRIBUTION BY AGE GROUP ===\n")
cat("etfr_contrib = 5 * mean ASFR for that cell; pct_of_tier = share of that tier's eTFR.\n\n")
print(contrib |>
        select(education, age_group, pct_of_tier) |>
        pivot_wider(names_from = age_group, values_from = pct_of_tier),
      n = Inf)

young_share <- contrib |>
  mutate(young = age_group %in% c("15-19", "20-24")) |>
  group_by(education, young) |>
  summarise(pct = sum(pct_of_tier), .groups = "drop") |>
  filter(young) |>
  select(education, pct_from_15_24 = pct)

cat("\n--- Share of each tier's eTFR coming from ages 15-24 ---\n")
print(young_share)

# ── 3. THE DECISIVE TEST: EDUCATION COMPOSITION BY AGE GROUP ─────────────────
# For each age group, what fraction of women sit in each education tier?
# This is what adjudicates between the artefact and mechanism readings.
#
# If at 15-19 the low tier holds the overwhelming majority of women and the
# high tier holds almost none, then eTFR_low at that age is not measuring the
# fertility of women who will END UP low-educated. It is measuring the
# fertility of teenagers, nearly all of whom are provisionally classified low.

edu_comp <- asfr_data |>
  group_by(country, year, age_group, education) |>
  summarise(pop = sum(population, na.rm = TRUE), .groups = "drop") |>
  group_by(country, year, age_group) |>
  mutate(share = 100 * pop / sum(pop, na.rm = TRUE)) |>
  group_by(age_group, education) |>
  summarise(mean_share = round(mean(share, na.rm = TRUE), 1), .groups = "drop") |>
  pivot_wider(names_from = education, values_from = mean_share) |>
  select(age_group, low, medium, high) |>
  arrange(age_group)

cat("\n\n=== SECTION 3: EDUCATION COMPOSITION OF WOMEN BY AGE GROUP ===\n")
cat("Mean % of women in each tier, by age group, across all country-years.\n")
cat("THIS IS THE DECISIVE DIAGNOSTIC.\n\n")
print(edu_comp, n = Inf)

cat("\n--- Interpretation guide ---\n")
young_low  <- edu_comp$low[edu_comp$age_group == "15-19"]
young_high <- edu_comp$high[edu_comp$age_group == "15-19"]
mid_low    <- edu_comp$low[edu_comp$age_group == "30-34"]
cat(sprintf("  At 15-19: %.1f%% of women classified low, %.1f%% high\n", young_low, young_high))
cat(sprintf("  At 30-34: %.1f%% of women classified low\n", mid_low))
cat(sprintf("  Ratio of low-share at 15-19 vs 30-34: %.1fx\n", young_low / mid_low))
if (young_low > 70 && young_high < 5) {
  cat("\n  => STRONG support for the ARTEFACT reading at 15-19: the low tier at\n")
  cat("     this age is essentially 'all teenagers', not 'women who will end up\n")
  cat("     low-educated'. The high tier is near-empty by construction.\n")
} else {
  cat("\n  => The composition at 15-19 is less extreme than the artefact reading\n")
  cat("     assumes. Inspect before concluding.\n")
}

# Same table by country (heterogeneity check)
edu_comp_country <- asfr_data |>
  filter(age_group %in% c("15-19", "20-24")) |>
  group_by(country, year, age_group, education) |>
  summarise(pop = sum(population, na.rm = TRUE), .groups = "drop") |>
  group_by(country, year, age_group) |>
  mutate(share = 100 * pop / sum(pop, na.rm = TRUE)) |>
  group_by(country, age_group, education) |>
  summarise(mean_share = round(mean(share, na.rm = TRUE), 1), .groups = "drop") |>
  filter(education == "low") |>
  select(country, age_group, low_share = mean_share) |>
  pivot_wider(names_from = age_group, values_from = low_share,
              names_prefix = "low_share_") |>
  arrange(desc(`low_share_15-19`))

cat("\n--- Low-tier share at young ages, by country ---\n")
print(edu_comp_country, n = Inf)

# ── 4. SWEDEN ANOMALY CHECK ──────────────────────────────────────────────────
# The 25-49 run returned Sweden at 1.54 / 1.54 / 1.54 -- identical to three
# decimals across all tiers. That is implausible and may indicate a data issue.

cat("\n\n=== SECTION 4: SWEDEN ANOMALY CHECK ===\n")

swe_full <- etfr_full |> filter(country == "Sweden")
cat("Sweden, full panel, eTFR by education and year:\n")
print(swe_full |>
        select(year, education, etfr) |>
        pivot_wider(names_from = education, values_from = etfr) |>
        arrange(year), n = Inf)

swe_asfr <- asfr_data |>
  filter(country == "Sweden") |>
  group_by(age_group, education) |>
  summarise(mean_asfr = round(mean(asfr, na.rm = TRUE), 4),
            mean_pop  = round(mean(population, na.rm = TRUE)),
            mean_births = round(mean(births, na.rm = TRUE)),
            .groups = "drop")

cat("\nSweden mean ASFR / population / births by age group and tier:\n")
print(swe_asfr |>
        select(age_group, education, mean_asfr) |>
        pivot_wider(names_from = education, values_from = mean_asfr), n = Inf)

cat("\nSweden mean population by age group and tier:\n")
print(swe_asfr |>
        select(age_group, education, mean_pop) |>
        pivot_wider(names_from = education, values_from = mean_pop), n = Inf)

cat("\nAny duplicated Sweden cells in asfr_data?\n")
print(asfr_data |>
        filter(country == "Sweden") |>
        count(year, age_group, education) |>
        filter(n > 1))

# ── 5. REGENERATE TABLE 4.1 ──────────────────────────────────────────────────
# The dissertation's Table 4.1 reports low 1.74 / medium 1.47 / high 1.57,
# which does not match the current etfr_data. Regenerate from source.

cat("\n\n=== SECTION 5: TABLE 4.1 REGENERATED FROM CURRENT DATA ===\n")
cat("Compare against the dissertation, which currently reports:\n")
cat("  low 1.74 [0.36, 3.39] | medium 1.47 [0.51, 2.61] | high 1.57 [0.48, 2.42]\n\n")

tab41 <- etfr_full |>
  mutate(education = factor(education, levels = c("low", "medium", "high"))) |>
  group_by(education) |>
  summarise(
    mean_etfr = round(mean(etfr, na.rm = TRUE), 2),
    min_etfr  = round(min(etfr, na.rm = TRUE), 2),
    max_etfr  = round(max(etfr, na.rm = TRUE), 2),
    sd_etfr   = round(sd(etfr, na.rm = TRUE), 2),
    n         = n(),
    .groups   = "drop"
  ) |>
  mutate(isced = c("0-2", "3-4", "5-8"))

print(tab41)
cat("\nN total:", nrow(etfr_full), "\n")

# ── 6. FIGURES ───────────────────────────────────────────────────────────────

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 6a. Education composition by age group -- the artefact diagnostic
p_comp <- edu_comp |>
  pivot_longer(c(low, medium, high), names_to = "education", values_to = "share") |>
  mutate(education = factor(education, levels = c("low", "medium", "high"))) |>
  ggplot(aes(x = age_group, y = share, fill = education)) +
  geom_col() +
  scale_fill_manual(values = c(low = "#d73027", medium = "#fee090", high = "#4575b4")) +
  labs(
    title    = "Education composition of women by age group",
    subtitle = "Mean % across all country-years. At 15-19 nearly all women are classified 'low'\nbecause upper secondary is not yet complete -- the low tier there is not a behavioural category.",
    x = "Age group", y = "% of women", fill = "Education"
  ) +
  theme_minimal(base_size = 11)

ggsave("output/figures/diagnostic_agefloor_composition.png",
       p_comp, width = 8, height = 5, dpi = 300)

# 6b. eTFR contribution by age group and tier
p_contrib <- contrib |>
  ggplot(aes(x = age_group, y = etfr_contrib, fill = education)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(low = "#d73027", medium = "#fee090", high = "#4575b4")) +
  labs(
    title    = "Where each tier's fertility comes from",
    subtitle = "Contribution to eTFR by age group (5 x mean ASFR)",
    x = "Age group", y = "Contribution to eTFR", fill = "Education"
  ) +
  theme_minimal(base_size = 11)

ggsave("output/figures/diagnostic_agefloor_contribution.png",
       p_contrib, width = 8, height = 5, dpi = 300)

# 6c. Strict H1 across age floors
p_sweep <- sweep_tab |>
  ggplot(aes(x = age_range, y = strict_H1)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_col(aes(fill = strict_H1 < 0), width = 0.6) +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d73027"),
                    labels = c("TRUE" = "H1 holds", "FALSE" = "H1 reversed"),
                    name = NULL) +
  labs(
    title    = "Strict H1 contrast (high vs low) across age floors",
    subtitle = "Negative = high-educated women have lower fertility than low-educated",
    x = "Age range of eTFR", y = "beta (high vs low)"
  ) +
  theme_minimal(base_size = 11)

ggsave("output/figures/diagnostic_agefloor_sweep.png",
       p_sweep, width = 7, height = 5, dpi = 300)

cat("\nFigures saved to output/figures/\n")

# ── 7. SAVE ──────────────────────────────────────────────────────────────────

saveRDS(
  list(
    sweep_tab       = sweep_tab,
    tier_means      = tier_means,
    contrib         = contrib,
    edu_comp        = edu_comp,
    edu_comp_country = edu_comp_country,
    tab41           = tab41,
    models          = models
  ),
  "data/derived/agefloor_sweep.rds"
)

cat("Saved to data/derived/agefloor_sweep.rds\n")


