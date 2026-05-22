# Demographic-Transition-Fitness-Paradox
# Has the Wealth-Fertility Relationship Really Inverted?
## A Cross-Cohort, Cross-Country Test of the Demographic Transition Fitness Paradox

**MSc Applied Social Data Science**  
**Trinity College Dublin**  
**2025–2026**

---

## Overview

This repository contains all code, documentation, and outputs for my MSc 
dissertation examining the socioeconomic-fertility gradient across European 
cohorts born 1930–1980.

The dissertation tests four hypotheses:
- **H1 (Inversion):** The relationship between education and completed 
  fertility is negative across European cohorts 1930–1980
- **H2 (J-curve):** The SES-fertility relationship is non-monotonic in 
  recent cohorts, with fertility rising at the top of the distribution
- **H3 (Cultural transmission):** Cross-country variation in the gradient 
  is better predicted by cultural indicators than economic ones
- **H4 (Individual mechanism):** The effect of education on fertility in 
  the BCS70 cohort is mediated primarily by attitudinal rather than 
  earnings pathways

---

## Data

Raw data is not tracked in this repository due to file size and licensing 
restrictions. See `data/raw/README.md` for full instructions on how to 
reproduce the data folder.

**Immediately downloadable:**
- Wittgenstein Centre Human Capital Data Explorer
- Human Fertility Database
- Human Mortality Database
- V-Dem (v16)
- OECD Data Explorer
- World Bank Open Data
- Eurostat

**Requires free registration (~1 day):**
- European Social Survey (rounds 1–11)

**Requires UK Data Service registration (~1–2 weeks):**
- 1970 British Cohort Study (BCS70), sweeps 1–11

---

## Reproducibility

Analysis is conducted in R. To install all required packages:

```r
install.packages(c("wcde", "haven", "tidyverse", "fixest", "mgcv",
                   "xgboost", "iml", "brms", "DoubleML",
                   "countrycode", "mice", "targets"))
```

The analysis pipeline is managed using the `targets` package. To run the 
full pipeline:

```r
library(targets)
tar_make()
```

---

## Pre-registration

The pre-analysis plan is deposited on the Open Science Framework prior to 
individual-level data analysis:  
[OSF link — to be added]

---

## Citation

If you use any part of this work, please cite:

Roudra Sarkar (2026). *Has the Wealth-Fertility Relationship Really Inverted? 
A Cross-Cohort, Cross-Country Test of the Demographic Transition Fitness 
Paradox.* MSc Dissertation, Trinity College Dublin.

---

## Supervisor

Dr. Thomas Chadefaux, Trinity College Dublin

## Contact

rsarkar@tcd.ie
