# =============================================================================
# 06b_ess_cultural_predictors.R
# Read local ESS CSV, extract 8 attitudinal items, aggregate to 3 country-level
# composite cultural indicators, merge into Dataset 2 for H3 analysis.
# =============================================================================

library(dplyr)
setwd("~/Desktop/dissertation")

# ── 1. Read only the columns we need from the local ESS file ─────────────────
library(data.table)

cat("Reading ESS data from local CSV...\n")

ess_full <- fread(
  "data/raw/ESS_data/Datafile-subset.csv",
  stringsAsFactors = FALSE,
  showProgress = TRUE
)

cat("Full file:", nrow(ess_full), "rows x", ncol(ess_full), "columns\n")

# Select only our 8 target variables + identifiers
ess <- ess_full |>
  select(cntry, essround,
         rlgdgr,    # religiosity 0-10 (10 = very religious)
         rlgatnd,   # religious attendance 1-7 (1 = every day, 7 = never)
         wmcpwrk,   # women cut down paid work for family 1-5 (1 = agree strongly)
         mnrgtjb,   # men more right to job when scarce 1-5 (1 = agree strongly)
         ipcrtiv,   # creativity/openness 1-6 (1 = very much like me)
         impfree,   # autonomy/self-direction 1-6 (1 = very much like me)
         imptrad,   # tradition 1-6 (1 = very much like me)
         ipfrule    # conformity 1-6 (1 = very much like me)
  )

rm(ess_full)  # free memory
gc()

cat("Selected:", nrow(ess), "rows x", ncol(ess), "columns\n")

# ── 2. Filter to target countries ────────────────────────────────────────────
target_iso <- c(
  "AT", "BE", "HR", "CZ", "DK", "EE", "FI", "GR", "HU",
  "LV", "NO", "PL", "PT", "RO", "RS", "SK", "SI", "ES", "SE", "TR"
)

iso_to_name <- c(
  AT = "Austria", BE = "Belgium", HR = "Croatia", CZ = "Czechia",
  DK = "Denmark", EE = "Estonia", FI = "Finland", GR = "Greece",
  HU = "Hungary", LV = "Latvia", NO = "Norway", PL = "Poland",
  PT = "Portugal", RO = "Romania", RS = "Serbia", SK = "Slovakia",
  SI = "Slovenia", ES = "Spain", SE = "Sweden", TR = "Türkiye"
)

ess <- ess |> filter(cntry %in% target_iso)
cat("After country filter:", nrow(ess), "rows\n")

cat("\n--- Respondents per country ---\n")
print(ess |> count(cntry, name = "n") |> arrange(cntry))

cat("\n--- Rounds per country ---\n")
print(ess |> group_by(cntry) |> summarise(rounds = paste(sort(unique(essround)), collapse=","), n_rounds = n_distinct(essround)) |> arrange(cntry))

# ── 3. Clean: recode ESS missing values to NA ────────────────────────────────
# ESS uses 77 (refusal), 88 (don't know), 99 (no answer) as missing codes
ess_clean <- ess |>
  mutate(
    rlgdgr  = ifelse(rlgdgr  > 10, NA, rlgdgr),
    rlgatnd = ifelse(rlgatnd > 7,  NA, rlgatnd),
    wmcpwrk = ifelse(wmcpwrk > 5,  NA, wmcpwrk),
    mnrgtjb = ifelse(mnrgtjb > 5,  NA, mnrgtjb),
    ipcrtiv = ifelse(ipcrtiv > 6,  NA, ipcrtiv),
    impfree = ifelse(impfree > 6,  NA, impfree),
    imptrad = ifelse(imptrad > 6,  NA, imptrad),
    ipfrule = ifelse(ipfrule > 6,  NA, ipfrule)
  )

cat("\n--- Non-missing counts per variable ---\n")
for (v in c("rlgdgr", "rlgatnd", "wmcpwrk", "mnrgtjb", "ipcrtiv", "impfree", "imptrad", "ipfrule")) {
  n_valid <- sum(!is.na(ess_clean[[v]]))
  cat(sprintf("  %-10s : %6d valid (%5.1f%%)\n", v, n_valid, 100 * n_valid / nrow(ess_clean)))
}

# ── 4. Reverse-code so higher = more of the theoretically relevant direction ─
ess_recode <- ess_clean |>
  mutate(
    # Secularisation: higher = LESS religious
    secular_belief  = 10 - rlgdgr,             # 0-10, 10 = not at all religious
    secular_attend  = rlgatnd,                  # 1-7, 7 = never attends (already correct)

    # Gender egalitarianism: higher = MORE egalitarian
    gender_wmcpwrk  = 6 - wmcpwrk,             # 1-5, 5 = disagree women should cut work
    gender_mnrgtjb  = 6 - mnrgtjb,             # 1-5, 5 = disagree men have more right

    # Schwartz values: reverse so higher = MORE of that value
    schwartz_creative = 7 - ipcrtiv,            # 1-6, 6 = very much values creativity
    schwartz_free     = 7 - impfree,            # 1-6, 6 = very much values autonomy
    schwartz_trad     = 7 - imptrad,            # 1-6, 6 = very much values tradition
    schwartz_conform  = 7 - ipfrule             # 1-6, 6 = very much values conformity
  )

# Map country codes to names
ess_recode$country <- iso_to_name[ess_recode$cntry]

# ── 5. Aggregate to country-level means and build composites ─────────────────
ess_country <- ess_recode |>
  group_by(country) |>
  summarise(
    # ── COMPOSITE 1: Secularisation (0-10 scale) ──
    # Average of reversed religiosity and rescaled attendance
    raw_secular_belief = mean(secular_belief, na.rm = TRUE),
    raw_secular_attend = mean(secular_attend, na.rm = TRUE),
    ess_secular = (mean(secular_belief, na.rm = TRUE) +
                   mean(secular_attend, na.rm = TRUE) * (10 / 7)) / 2,

    # ── COMPOSITE 2: Gender egalitarianism (1-5 scale) ──
    # Average of two reversed gender items
    raw_gender_wmcpwrk = mean(gender_wmcpwrk, na.rm = TRUE),
    raw_gender_mnrgtjb = mean(gender_mnrgtjb, na.rm = TRUE),
    ess_gender_egal = (mean(gender_wmcpwrk, na.rm = TRUE) +
                       mean(gender_mnrgtjb, na.rm = TRUE)) / 2,

    # ── COMPOSITE 3: Autonomy vs conformity ──
    # (openness items) minus (conservation items)
    raw_creative = mean(schwartz_creative, na.rm = TRUE),
    raw_free     = mean(schwartz_free, na.rm = TRUE),
    raw_trad     = mean(schwartz_trad, na.rm = TRUE),
    raw_conform  = mean(schwartz_conform, na.rm = TRUE),
    ess_autonomy = (mean(schwartz_creative, na.rm = TRUE) +
                    mean(schwartz_free, na.rm = TRUE)) -
                   (mean(schwartz_trad, na.rm = TRUE) +
                    mean(schwartz_conform, na.rm = TRUE)),

    # ── Metadata ──
    ess_n_rounds      = n_distinct(essround),
    ess_n_respondents = n(),
    .groups = "drop"
  )

# ── 6. Report ────────────────────────────────────────────────────────────────
cat("\n=== ESS Country-Level Composites ===\n")
print(
  ess_country |>
    select(country, ess_secular, ess_gender_egal, ess_autonomy,
           ess_n_rounds, ess_n_respondents) |>
    arrange(country),
  n = Inf
)

cat("\n--- Composite summary statistics ---\n")
for (v in c("ess_secular", "ess_gender_egal", "ess_autonomy")) {
  vals <- ess_country[[v]]
  cat(sprintf("  %-20s : min=%.3f  max=%.3f  mean=%.3f  sd=%.3f\n",
              v, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
              mean(vals, na.rm = TRUE), sd(vals, na.rm = TRUE)))
}

cat("\n--- Coverage check against 21 dissertation countries ---\n")
all_21 <- c(
  "Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia",
  "Finland", "Greece", "Hungary", "Latvia", "North Macedonia", "Norway",
  "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Türkiye"
)
matched   <- intersect(all_21, ess_country$country)
unmatched <- setdiff(all_21, ess_country$country)
cat("Matched:  ", length(matched), "of 21\n")
cat("Missing:  ", paste(unmatched, collapse = ", "), "\n")

# ── 7. Merge into Dataset 2 ─────────────────────────────────────────────────
d2 <- readRDS("data/derived/dataset2_full.rds")

ess_merge <- ess_country |>
  select(country, ess_secular, ess_gender_egal, ess_autonomy,
         ess_n_rounds, ess_n_respondents)

d2_updated <- d2 |>
  left_join(ess_merge, by = "country")

cat("\n--- Updated Dataset 2 ---\n")
cat("Rows:", nrow(d2_updated), "  Columns:", ncol(d2_updated), "\n")
cat("New columns:", paste(setdiff(names(d2_updated), names(d2)), collapse = ", "), "\n")

cat("\n--- ESS missingness in Dataset 2 ---\n")
for (v in c("ess_secular", "ess_gender_egal", "ess_autonomy")) {
  n_miss <- sum(is.na(d2_updated[[v]]))
  cat(sprintf("  %-20s : %d missing (%d available)\n", v, n_miss, 21 - n_miss))
}

# ── 8. Save ──────────────────────────────────────────────────────────────────
saveRDS(d2_updated, "data/derived/dataset2_full.rds")
cat("\nSaved updated dataset2_full.rds\n")

saveRDS(ess_country, "data/derived/ess_country_aggregates.rds")
write.csv(ess_country, "data/derived/ess_country_aggregates.csv", row.names = FALSE)
cat("Saved ESS aggregates to data/derived/\n")

cat("\n=== DONE ===\n")
cat("Next: update 07_h3_models.R to include ess_secular, ess_gender_egal, ess_autonomy\n")
