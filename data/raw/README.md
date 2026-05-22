# Raw Data

This folder contains all raw datasets used in the dissertation. These files 
are excluded from version control due to file size and data licensing 
restrictions. Follow the instructions below to reproduce the data folder.

---

## Expected Folder Structure
data/raw/
├── wittgenstein/
│   ├── wcde_tfr_by_education.csv
│   └── wcde_tfr_plain.csv
├── hfd/
│   ├── CCF.xlsx
│   ├── mabVH.txt
│   ├── mabVHbo.txt
│   ├── mabRR.txt
│   ├── mabRRbo.txt
│   ├── adjtfrRR.txt
│   └── adjtfrRRbo.txt
├── hmd/
│   └── bltper_1x1.txt
├── vdem/
│   └── V-Dem-CY-Full+Others-v16.csv
├── oecd/
│   ├── female_lfp.csv
│   └── gdp_per_capita.csv
├── worldbank/
│   └── contraceptive_prevalence.csv
├── eurostat/
│   ├── demo_faeduc.csv
│   └── edat_lfse_03.csv
├── ess/
│   └── ESS_integrated_rounds1_11.csv
└── bcs70/
├── UKDA-2666-stata/
├── UKDA-5558-stata/
├── UKDA-6557-stata/
├── UKDA-6941-stata/
├── UKDA-6943-stata/
├── UKDA-7473-stata/
├── UKDA-8547-stata/
└── UKDA-9347-stata/

---

## 1. Wittgenstein Centre Human Capital Data Explorer

**Version:** 2023 update (V3.0), Medium (SSP2) scenario  
**URL:** https://dataexplorer.wittgensteincentre.org/  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Instructions:**
1. Go to https://dataexplorer.wittgensteincentre.org/
2. Set Indicator Type: Demographic Changes
3. Set Indicator: Total Fertility Rate by Education
4. Set Region: Europe, tick "Include countries of selected regions"
5. Set Sex: Female
6. Tick "Include all times"
7. Click Download → save to `data/raw/wittgenstein/`

**Note:** The web explorer only provides education-stratified TFR from 
2020 onwards. Historical education-stratified data is pulled via the 
`wcde` R package in `R/01_data_download.R`.

---

## 2. Human Fertility Database

**Version:** Last modified 23/03/2026  
**URL:** https://www.humanfertility.org  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Instructions:**
1. Go to https://www.humanfertility.org
2. Navigate to Data → Summary indicators
3. Download: Completed cohort fertility → save as `CCF.xlsx`
4. Download: Mean age at birth (cohort, VH) → save as `mabVH.txt`
5. Download: Mean age at birth by birth order (cohort) → save as `mabVHbo.txt`
6. Download: Mean age at birth (period, RR) → save as `mabRR.txt`
7. Download: Mean age at birth by birth order (period) → save as `mabRRbo.txt`
8. Download: Tempo-adjusted TFR (RR) → save as `adjtfrRR.txt`
9. Download: Tempo-adjusted TFR by birth order → save as `adjtfrRRbo.txt`
10. Save all files to `data/raw/hfd/`

**Citation:** Human Fertility Database. Max Planck Institute for Demographic 
Research (Germany) and Vienna Institute of Demography (Austria). 
Available at www.humanfertility.org (data downloaded May 2026).

---

## 3. Human Mortality Database

**Version:** Last modified April 2026  
**URL:** https://www.mortality.org  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Instructions:**
1. Go to https://www.mortality.org
2. Navigate to Data → Bulk download
3. Download: Period life tables, all countries, 1x1 format
4. Save as `bltper_1x1.txt` in `data/raw/hmd/`

**Note:** Only the 1x1 file is needed. The mx column contains age-specific 
death rates — no separate mortality file is required.

---

## 4. V-Dem (Varieties of Democracy)

**Version:** V16 (March 2026)  
**URL:** https://v-dem.net/data/the-v-dem-dataset/  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Instructions:**
1. Go to https://v-dem.net
2. Navigate to Data → V-Dem Dataset
3. Download: Country-Year: V-Dem Full+Others → CSV format
4. Save to `data/raw/vdem/`

**Note:** File is ~400MB. In R, immediately subset to European countries 
and relevant years to reduce size.

---

## 5. OECD Data Explorer

**URL:** https://data-explorer.oecd.org  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Female Labour Force Participation:**
1. Search for "Annual Labour Force Statistics female participation"
2. Select all European countries, annual frequency, female, ages 15-64
3. Tick "Include all times"
4. Download CSV → save as `data/raw/oecd/female_lfp.csv`

**GDP per Capita:**
1. Search for "GDP per capita annual"
2. Select all European countries, annual frequency, USD PPP converted
3. Tick "Include all times"
4. Download CSV → save as `data/raw/oecd/gdp_per_capita.csv`

---

## 6. World Bank

**URL:** https://data.worldbank.org/indicator/SP.DYN.CONU.ZS  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Instructions:**
1. Go to the URL above
2. Click Download → CSV
3. Save as `data/raw/worldbank/contraceptive_prevalence.csv`

**Note:** Data is very sparse for Western European countries (3-12 
observations per country). Treat as partially observed covariate in H3.

---

## 7. Eurostat

**URL:** https://ec.europa.eu/eurostat/data/database  
**Access:** Free, no registration required  
**Downloaded:** May 2026

**Live births by mother's educational attainment (demo_faeduc):**
1. Search for dataset code `demo_faeduc`
2. Select all countries, all years, all education levels, all age groups
3. Download CSV → save as `data/raw/eurostat/demo_faeduc.csv`

**Population by educational attainment (edat_lfse_03):**
1. Search for dataset code `edat_lfse_03`
2. Select all countries, all years, all education levels, both sexes
3. Download CSV → save as `data/raw/eurostat/edat_lfse_03.csv`

---

## 8. European Social Survey (ESS)

**Version:** Rounds 1-11 (2002-2024), integrated file  
**URL:** https://www.europeansocialsurvey.org/data-portal  
**Access:** Free academic registration required (~1 day to clear)  
**Downloaded:** May 2026

**Instructions:**
1. Register at https://www.europeansocialsurvey.org/data-portal
2. Select all rounds (1-11) and all countries
3. Select variable categories: Subjective wellbeing/religion, Gender and 
   household composition, Socio-demographic profile, Human values scale, 
   Rotating modules, Cross-module replicated questions
4. Download as CSV
5. Save to `data/raw/ess/`

**Note:** File is approximately 1.4GB.

---

## 9. 1970 British Cohort Study (BCS70)

**URL:** https://ukdataservice.ac.uk  
**Study page:** https://cls.ucl.ac.uk/cls-studies/1970-british-cohort-study/  
**Access:** UK Data Service End User License — registration required 
(allow 1-2 weeks)  
**Downloaded:** May 2026

**Sweeps required:**

| UKDA SN | Sweep | Year | Age |
|---|---|---|---|
| 2666 | Birth | 1970 | 0 |
| 5558 | Sweep 5 | 1996 | 26 |
| 6557 | Sweep 7 | 2004 | 34 |
| 6941 | Sweep 8 | 2008 | 38 |
| 6943 | Sweep 8 partner | 2008 | 38 |
| 7473 | Sweep 9 | 2012 | 42 |
| 8547 | Sweep 10 | 2016 | 46 |
| 9347 | Sweep 11 | 2021 | 51 |

**Instructions:**
1. Register at https://ukdataservice.ac.uk
2. Search for each study number above
3. Download in Stata (.dta) format
4. Save each to its own subfolder under `data/raw/bcs70/`

**Important:** BCS70 data is licensed under the UK Data Service End User 
License. It may not be redistributed or shared. Do not commit these files 
to any public repository.

**Accompanying documentation:**  
`Deriving-highest-qualification-in-NCDS-and-BCS70.pdf` — essential for 
constructing the education variable consistently across sweeps.