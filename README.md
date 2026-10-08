# Estimating the Reliability of Smartphone Use Using Dynamic Structural Equation Modelling

**Alexandru Cernat** (University of Manchester)  
**Florian Keusch** (University of Mannheim)

This repository contains the code and materials used for the paper:

> *Estimating the Reliability of Smartphone Use Using Dynamic Structural Equation Modelling*

The project estimates measurement reliability of smartphone use indicators by combining digital trace data and survey data using **Dynamic Structural Equation Models (DSEM)** in **Mplus**, with data preparation and automation handled in **R**. It also shows how this reliability affects substantive results, using two simulations based on published studies with open data and an application to the PINCET data, with latent variable models in **lavaan**.

---

## Repository structure

```
data/        # Input data (not public; only cat_select.csv, the coding of apps into activities, is included)
functions/   # Helper functions to extract reliability, regression and trend estimates from Mplus output
mplus/       # Mplus model files (DSEM specifications) and output
out/         # Main results
scripts/     # Analysis scripts
```

Main scripts (reliability estimation):
- `scripts/01.data_prep.R` – data cleaning and preparation, including the Mplus data files  
- `scripts/02_mplus_automate.R` – automated Mplus model runs  
- `scripts/03_mplus_import.R` – import and combine Mplus results, figures in `out/`  

Impact of reliability on substantive results:
- `scripts/sim_a_scharkow.R` – Simulation A, digital trace data as predictor, based on Scharkow et al. (2020, *PNAS*)  
- `scripts/sim_b_harari.R` – Simulation B, digital trace data as outcome, based on Harari et al. (2020, *Journal of Personality and Social Psychology*)  
- `scripts/pincet_news.R` – application to the PINCET data: daily social media use and news visits on the smartphone, corrected for measurement error  


---

## Reproducibility

**Software versions used**
- R **4.5.1**
- Mplus **9**
- Main R packages: tidyverse 2.0.0, MplusAutomation 1.2, lme4 2.0-6, lavaan 0.7-2 (all versions are recorded in `renv.lock`)

**Package management**  
This project uses `renv` to manage R package versions.

Restore the R environment:
```r
renv::restore()
```

Run the reliability estimation pipeline:
```r
source("scripts/01.data_prep.R")
source("scripts/02_mplus_automate.R")
source("scripts/03_mplus_import.R")
```

Run the analyses of the impact of reliability:
```r
source("scripts/sim_a_scharkow.R")   # about 30 minutes, downloads the data from OSF
source("scripts/sim_b_harari.R")     # about 5 minutes, downloads the data from OSF
source("scripts/pincet_news.R")      # a few minutes, needs the PINCET data
```

---

## Data availability

Due to data protection restrictions, the raw digital trace data are not public. The PINCET survey datasets are available in the GESIS repository, https://doi.org/10.7802/2585. Digital trace data can be requested from the PI of the PINCET project, Ruben Bach (ruben.bach@mzes.uni-mannheim.de).

The two simulations use open data from Scharkow et al. (2020; https://osf.io/pqd9f/) and Harari et al. (2020; https://osf.io/p9rz3/), which the scripts download automatically.

---

## Citation

If you use this code, please cite:

Cernat, A., & Keusch, F. (Year). *Estimating the Reliability of Smartphone Use Using Dynamic Structural Equation Modelling*.

---

## Contact

- Alexandru Cernat – University of Manchester  


---

**Summary**  
This repository provides a scripted R + Mplus workflow for estimating the reliability of smartphone use measures using DSEM, and for showing how this reliability affects substantive results, supporting transparent and reproducible research while respecting data access constraints.
