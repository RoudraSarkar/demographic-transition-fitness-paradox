<<<<<<< HEAD
# Between Culture and Cost
=======
# Demographic-Transition-Fitness-Paradox
# Between Culture and Cost
## A Cross-Cohort, Cross-Country Test of the Demographic Transition Fitness Paradox
>>>>>>> 702210313f01e132e90492c3b13b5d45c61d8169

**Mechanisms of the education–fertility gradient across Europe and a British cohort**

MSc dissertation, Applied Social Data Science, Trinity College Dublin (2026).
Author: Roudra Sarkar.

This repository contains the full analysis pipeline behind the dissertation: data
preparation, models, robustness checks, and the code that generates every figure and
table in the submitted document.

---

## What the project asks

Across most of history, wealthier people had more surviving children. In contemporary
high-income countries the relationship inverted — a puzzle sometimes called the
*demographic transition fitness paradox*. This dissertation tests four claims about it,
using three datasets at three different scales.

| | Hypothesis | Data | Verdict |
|---|---|---|---|
| **H1** | Education and fertility are negatively related | 21-country Eurostat panel | Supported, but the gradient is a **timing** effect concentrated in ages 20–24 |
| **H2** | The relationship is non-monotonic (turns up at the top) | Same panel | **Supported** — medium-educated women have the lowest fertility in 14 of 21 countries |
| **H3** | Cross-country variation is explained better by cultural than economic indicators | 21-country cross-section | Unsupported — not resolvable at *N* = 21 |
| **H4** | The individual mechanism is attitudinal, not economic | BCS70 cohort | **Falsified** — earnings dominate; attitudes are negligible |

**Headline finding.** The European education–fertility gradient is not a monotonic
decline in family size. It is a U-shape in level plus a tempo effect concentrated in the
early twenties, and the individual-level mechanism behind it is economic rather than
cultural.

---

## ⚠️ Data access and licensing — read before cloning

**No microdata is redistributed in this repository.** Two of the sources cannot legally
be shared:

- **BCS70** (1970 British Cohort Study) is held under academic licence via the
  [UK Data Service](https://ukdataservice.ac.uk). You must register and download it
  yourself. Studies used: `UKDA-SN-2666, 3535, 3723, 5558, 6095, 7473, 9347`.
- **ESS** (European Social Survey) integrated file requires free registration at
  [europeansocialsurvey.org](https://www.europeansocialsurvey.org).

The remaining sources are freely downloadable:

| Source | Series / version | Used for |
|---|---|---|
| Eurostat | `demo_faeduc`, `edat_lfse_03`, `demo_pjangroup` | H1, H2 (fertility panel) |
| V-Dem | v16 | H3 (institutional predictors) |
| OECD | GDP per capita, labour-force participation | H3 (economic predictors) |
| World Bank | Contraceptive prevalence | H3 |

To reproduce from scratch you will need to place the raw files under `data/raw/`
following the structure described in `01_inspect_data.R`.

---

## Repository structure

```
├── scripts/            # numbered analysis pipeline (run in order)
├── data/
│   ├── raw/            # source downloads (not committed — see licensing above)
│   ├── derived/        # constructed analysis datasets (.rds)
│   ├── processed/      # BCS70 analytical file (.rds)
│   └── models/         # fitted model objects (.rds)
├── output/
│   └── figures/        # all 13 figures used in the dissertation
└── README.md
```

---

<<<<<<< HEAD
## The pipeline

Scripts are numbered and **must be run in order** — each depends on objects written by
earlier ones.

### Data construction

| Script | What it does | Writes |
|---|---|---|
| `00_setup_project.R` | Creates directory structure | — |
| `01_inspect_data.R` | Inspects raw source files before cleaning | — |
| `01_clean_eurostat.R` | Filters Eurostat extracts to the countries, age groups and ISCED tiers used | — |
| `02_descriptive_dataset1.R` | Builds education-specific ASFRs and aggregates to the period eTFR panel | `asfr_data.rds`, `etfr_data.rds` |
| `02b_gradient_shape.R` | Classifies the 21 countries by gradient shape | `gradient_shape.rds` |

### H1 and H2 — the population gradient

| Script | What it does | Writes |
|---|---|---|
| `03_h1_models.R` | Two-way fixed-effects models, country × education interactions, robustness battery | `h1_models.rds` |
| `04_h2_models.R` | Quadratic specification and quantile regressions | `h2_models.rds` |

### H3 — cross-country variation

| Script | What it does | Writes |
|---|---|---|
| `05_dataset2_outcomes.R` | Derives country-level gradient outcomes from Dataset 1 | — |
| `06_dataset2_predictors.R` | Assembles V-Dem, OECD and World Bank predictors | — |
| `06b_ess_cultural_predictors.R` | Builds three ESS attitudinal composites from eight core-module items | `dataset2_full.rds` |
| `07_h3_models.R` | XGBoost + SHAP (Stage 1) and six Bayesian models via `brms` (Stage 2) | `h3_xgboost.rds`, `h3_bayesian.rds` |

### H4 — individual mechanism

| Script | What it does | Writes |
|---|---|---|
| `08_h4data_bcs70.R` | Reads and harmonises BCS70 sweeps; builds treatment, outcome, confounders, mediators | `h4_analytical_data.rds` |
| `08_h4dml_bcs70.R` | Double machine learning total effects and sequential mediation decomposition | `h4_dml_results.rds` |

### Robustness

| Script | What it does |
|---|---|
| `09_robustness_age2549.R` | Rebuilds the eTFR over ages 25–49 and re-runs H1/H2 |
| `10_*` | Age-floor sweep (15–49 / 20–49 / 25–49) and age-composition diagnostics |
| `11_*` | Country-clustered bootstrap standard errors for the quantile regressions |
| `12_*` | CR2 small-cluster standard errors with Satterthwaite degrees of freedom |

> **Note:** exact filenames for scripts 10–12 should be checked against the repository;
> the descriptions above reflect what each produces.

### Verification scripts

These check rather than estimate, and are not part of the estimation pipeline:

- `data_check.R` — prints the structure of all three analysis datasets
- `audit_chapter3.R` — extracts reported figures from saved model objects
- `audit_all_chapters.R` — cross-checks every number reported in the dissertation against the saved objects
- `robustness_check.R` — re-runs the population-weighted specification in isolation

---

## Reproduction notes

Things that will trip you up if you re-run this from scratch:

1. **Run `06b` before `07`.** Script 07 reads `dataset2_full.rds`, which `06b` writes.
   Running 07 against a stale version will silently produce H3 results that do not match
   the dissertation.

2. **The ESS gender-egalitarianism items are not reverse-coded.** On this extract,
   `wmcpwrk` and `mnrgtjb` already run in the egalitarian-high direction, so no `6 - x`
   flip is applied — unlike the religiosity and Schwartz items, which are reversed. If
   the composite comes out with the Nordic countries scoring *low*, the direction has
   been inverted somewhere.

3. **`education` must be relevelled to `medium`** before fitting. Medium is the lowest
   tier in 14 of 21 countries, which makes it the stable reference and displays the
   U-shape directly. Note that R defaults to alphabetical order, which would silently
   make `high` the reference.

4. **The ESS integrated file is ~1.4 GB.** Loading it can exhaust memory on smaller
   machines. `06b` only needs eight items, so subsetting on read is advisable.

5. **`brms` requires a working Stan toolchain.** The six Bayesian models take a few
   minutes (four chains × 4,000 iterations each).

6. **Non-employment in the earnings mediator is coded missing, not zero.** This drives
   the sample attrition across mediation stages, which is why `08_h4dml_bcs70.R` also
   reports a common-sample re-estimation holding *N* fixed at 812.

---

## Requirements

- **R 4.5.1**

Key packages:

| Package | Purpose |
|---|---|
| `fixest` | Two-way fixed-effects panel regression |
| `quantreg` | Quantile regression |
| `clubSandwich` | CR2 small-cluster standard errors |
| `xgboost` | Gradient-boosted trees (H3 Stage 1) |
| `SHAPforxgboost` | SHAP decomposition |
| `brms` | Bayesian regression (H3 Stage 2) |
| `DoubleML`, `mlr3`, `ranger` | Double machine learning (H4) |
| `haven` | Reading Stata `.dta` files (BCS70) |
| `dplyr`, `ggplot2` | Data manipulation and plotting |

---

## Figures

All thirteen figures in the dissertation are written to `output/figures/`:

| File | Dissertation figure |
|---|---|
| `h1_gradient_by_type.png` | 4.1 — gradients by shape category |
| `gradient_by_country.png` | 4.2 — gradient for each of the 21 countries |
| `h1_pooled_fe_coefs.png` | 5.1 — pooled fixed-effects coefficients |
| `h2_quadratic_curve.png` | 5.2 — fitted quadratic |
| `h2_quantile_boot.png` | 5.3 — education contrasts across the distribution |
| `h3_shap_importance.png` | 6.1 — SHAP feature importance |
| `h3_shap_beeswarm.png` | 6.2 — SHAP values by country |
| `h3_bayesian_posteriors.png` | 6.3 — posterior distributions |
| `h4_raw_gradient.png` | 6.4 — unadjusted BCS70 gradient by sex |
| `h4_dml_total_effects.png` | 6.5 — DML total effects |
| `h4_dml_mediation_waterfall.png` | 6.6 — mediation decomposition |
| `h3_correlation_matrix.png` | B.1 — predictor correlation matrix |
| `h3_shap_dependence.png` | B.2 — SHAP dependence plots |

---

## Citation

If you refer to this work:

> Sarkar, R. (2026). *Between Culture and Cost: Mechanisms of the education–fertility
> gradient across Europe and a British cohort.* MSc dissertation, Trinity College Dublin.

---
=======
>>>>>>> 702210313f01e132e90492c3b13b5d45c61d8169

## Licence

Code in this repository is released under the MIT Licence. The data are subject to the
terms of their respective providers and are not redistributed here.
