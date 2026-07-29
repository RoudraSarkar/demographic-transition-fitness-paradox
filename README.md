# Between Culture and Cost

**Mechanisms of the education–fertility gradient across Europe and a British cohort**

MSc dissertation, Applied Social Data Science, Trinity College Dublin (2026).
Roudra Sarkar.

Analysis code for a dissertation testing why more-educated women have fewer children,
using European country panel data and the 1970 British Cohort Study.

**Main findings.** The gradient is not a steady decline: medium-educated women have the
*lowest* fertility in 14 of 21 countries. The apparent negative gradient is largely about
*when* women have children, not how many — it disappears if you count only births from
age 25. And at the individual level the mechanism is economic (earnings), not attitudinal.

---

## Data

**No microdata is included in this repository.** Two sources cannot be redistributed:

- **BCS70** — academic licence via the [UK Data Service](https://ukdataservice.ac.uk).
  Studies used: `UKDA-SN-2666, 3535, 3723, 5558, 6095, 7473, 9347`
- **ESS** — free registration at [europeansocialsurvey.org](https://www.europeansocialsurvey.org)

Freely downloadable: Eurostat (`demo_faeduc`, `edat_lfse_03`, `demo_pjangroup`),
V-Dem v16, OECD, World Bank.

Place raw downloads in `data/raw/` to reproduce.

---

## Structure

```
scripts/          numbered pipeline — run in order
data/raw/         source downloads (not committed)
data/derived/     constructed datasets
data/models/      fitted models
output/figures/   all 13 dissertation figures
```

---

## Pipeline

Run in numerical order — each script depends on the ones before it.

| Script | Purpose |
|---|---|
| `01`–`02b` | Clean Eurostat data, build the fertility panel, classify gradient shapes |
| `03`–`04` | H1 and H2: fixed-effects, quadratic and quantile models |
| `05`–`07` | H3: build country-level predictors, then XGBoost/SHAP and Bayesian models |
| `08` | H4: build BCS70 sample, then double machine learning mediation |
| `09`–`12` | Robustness: age-floor sweep, bootstrap and CR2 standard errors |

`audit_all_chapters.R` cross-checks every number in the dissertation against the saved
model objects.

---

## Requirements

R 4.5.1, with: `fixest`, `quantreg`, `clubSandwich`, `xgboost`, `brms`, `DoubleML`,
`ranger`, `haven`, `dplyr`, `ggplot2`.

`brms` needs a working Stan toolchain.

---

## Notes for re-running

- Run `06b` before `07` — script 07 reads the dataset that `06b` writes.
- The ESS gender items are **not** reverse-coded; they already run egalitarian-high.
  If the Nordic countries score low on that composite, the direction has been inverted.
- `education` must be relevelled to `medium` before fitting, or R defaults to `high`.
- The ESS file is ~1.4 GB and can exhaust memory; subset on read.

---
