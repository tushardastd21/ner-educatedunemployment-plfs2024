# Education without Absorption: The Formalisation Trap in North-East India

This repository contains the full R replication codebase for the empirical paper evaluating youth labor absorption, formal employment bottlenecks, and regional structural dichotomies across India's North-Eastern Region (NER).

- **Author:** Tushar Das (Centre for Economic Studies and Planning, Jawaharlal Nehru University)
- **Working Paper (SSRN)**
- **Target Journal / Status:** Under review at *Indian Journal of Labour Economics* (IJLE)

---

## Executive Summary
Using unit-record microdata from the Periodic Labour Force Survey (PLFS 2023–24) collected by MoSPI, this study analyzes labor market outcomes among NEET youth aged 15–29 across all eight North-Eastern states (n = 8,480). 

### Core Methodological Highlights:
1. **Firth-Penalized Multinomial Regression:** Utilizes `brglm2` (`multinom` engine with adjusted score penalization) to resolve quasi-complete separation caused by small cell counts in smaller states (Mizoram, Sikkim, Nagaland).
2. **Complex Survey Sampling:** Incorporates design parameters (`ids = FSU`, `strata`, `weights = final_weight`) using `srvyr` and `survey` packages, setting single-PSU strata as certainty units to avoid standard error inflation.
3. **Terrain & Policy Case Study:** Maps district-level terrain dynamics (Hills vs. Plains) and analyzes Sikkim's structural industrial exception (pharmaceutical manufacturing and public administration absorption).

---

## 📁 Repository Structure

```text
├── 01 - data/
│   ├── district_code.xlsx            <- District-level mapping & terrain lookup metadata
│   └── (Raw PLFS files excluded)     <- cperv1.csv & chhv1.csv (Obtain from MoSPI)
├── 03 - output/                      <- Model estimates, CSV tables, and GT HTML outputs
│   ├── 01 - descriptive/             <- Summary statistics & macro indicators
│   ├── 02 - models/                  <- Firth multinomial regression output tables (CSV/HTML)
│   └── 03 - sikkim/                  <- District terrain variance & Sikkim sectoral breakdowns
├── 01 - data_cleaning.R              <- Microdata ingestion, design weights & NEET domain filter
├── 02 - descriptive statistics.R     <- LFPR, WPR, UR & education/gender intersectional tables
├── 03 - regression_model.R          <- Main Firth-penalized multinomial logit (brglm2) & VIF tests
├── 04 - sikkim.R                     <- District structural analysis, svyttest & Sikkim deep-dive
├── .gitignore                        <- Prevents raw microdata upload
└── README.md                         <- Project documentation & reproduction steps
