# Estimating the Reliability of Smartphone Use Using Dynamic Structural Equation Modelling

**Alexandru Cernat** (University of Manchester)  
**Florian Keusch** (Unviersity of Mannheim)

This repository contains the code and materials used for the paper:

> *Estimating the Reliability of Smartphone Use Using Dynamic Structural Equation Modelling*

The project estimates measurement reliability of smartphone use indicators by combining digital trace data and survey data using **Dynamic Structural Equation Models (DSEM)** in **Mplus**, with data preparation and automation handled in **R**.

---

## Repository structure

```
data/        # Input data (not fully public; see notes below)
functions/   # Helper functions for running and combining models
mplus/       # Mplus model files (DSEM specifications)
out/         # Model output and processed results
scripts/     # Main analysis pipeline
```

Main scripts:
- `scripts/01_data_prep.R` – data cleaning and preparation  
- `scripts/02_mplus_automate.R` – automated Mplus model runs  
- `scripts/03_mplus_import.R` – import and combine Mplus results  

---

## Reproducibility

**Software versions used**
- R **4.5.1**
- Mplus **9**

**R environment** (sessionInfo excerpt)
```
R version 4.5.2 (2025-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

attached base packages:
stats, graphics, grDevices, datasets, utils, methods, base

other attached packages:
MplusAutomation, lubridate, forcats, stringr, dplyr, purrr, readr, tidyr,
tibble, ggplot2, tidyverse
```

**Package management**  
This project uses `renv` to manage R package versions.

Restore the R environment:
```r
renv::restore()
```

Run the full analysis pipeline:
```r
source("scripts/01_data_prep.R")
source("scripts/02_mplus_automate.R")
source("scripts/03_mplus_import.R")
```

---

## Data availability

Due to data protection restrictions, the raw digital trace not public. The PINCET survey datasets are available in the GESIS repository, https://doi.org/10.7802/2585. Digital trace data can be requested from the PI of the PINCET project, Ruben Bach (ruben.bach@mzes.uni-mannheim.de). 

---

## Citation

If you use this code, please cite:

Cernat, A., & Keusch, F. (Year). *Estimating the Reliability of Smartphone Use Using Dynamic Structural Equation Modelling*.

---

## Contact

- Alexandru Cernat – University of Manchester  


---

**Summary**  
This repository provides a scripted R + Mplus workflow for estimating the reliability of smartphone use measures using DSEM, supporting transparent and reproducible research while respecting data access constraints.
