# Indirect comparison methods in clinical trials : T1 vs. T2
#### Project status : In progress ⏳
This projet implements three methods for non-anchored indirect comparisons of two treatments, T1 and T2, using simulated clinical trial data. These methos allow us to compare treatments in scenarios where there are no head-to-head trials comparing these treatments, but where each treatment has already been studied in separate trials.

### 📋 Overview
In this repository, we employ the following methods to perform indirect comparisons:

1. **Propensity Score-based methods** - with individual-level data for both trials:
   - Method 1: **Optimal matching**
   - Method 2: **Inverse Probability of Treatment Weighting (IPTW)**
These two methods can be used when individual-level data are available for both trials, allowing a more precise comparison of treatments, by adjusting for confounding variables through propensity score estimation.
2. **Matching-Adjusted Indirect Comparison (MAIC)** - when individual-level data for one trial, but only aggregared data for the other)
This method weights the subjects from the individual-level data trial to match the aggregate characteristics of the other trial.

### 🧬 Dataset
The dataset used in this project is completely **synthetic** and has been generated for demonstration purpose. The data is simulated in the script `Rscripts/02_data-creation.R`. This dataset contains, among other variables, three types of outcomes, commonly encountered in clinical trials :
- Binary outcome (`binary`),
- Continuous outcome (`continous`),
- Survival outcome (`surv`): time-to-event variable representing survival analysis. A related variable `censor` is included as a binary indicator of censored data (=1 if censored).  
The other variables are :
- `id` : a unique identifier,
- `trt` : a variable indicating the treatment group (T1 or T2),
- `sex`,
- `age`,
- `diag`: time since diagnosis,
- `therapies`: number of therapies previously followed.

### 🗂️ Repository structure

|-- Data/  
|   |-- data.rds  
|  
|-- Rscripts/  
|   |-- 00_initialize.R  
|   |-- 01_functions.R  
|   |-- 02_data-creation.R  
|   |-- 03_propensity-score.R  
|   |-- 04_maic.R  
|  
|-- Results/  
|  
|-- README.md


