# Indirect comparison methods in clinical trials
#### Project status : In progress ⏳
This projet implements three methods for indirect comparisons of two treatments (T1 and T2), using simulated clinical trials data. These methods allow us to compare treatments in scenarios where there are no head-to-head trials comparing these treatments, but where each treatment has already been studied in separate trials.

### 📋 Overview
In this repository, we employ the following methods to perform indirect comparisons:

1. **Propensity Score-based methods** - with individual-patient data (IPD) for both trials:
   - Method 1: **Optimal matching**
   - Method 2: **Inverse Probability of Treatment Weighting (IPTW)**
These two methods can be used when IPD are available for both trials, allowing a more precise comparison of treatments, by adjusting for confounding variables through propensity score estimation.
2. **Matching-Adjusted Indirect Comparison (MAIC)** - when IPD for one trial, but only aggregared data (AgD) for the other.
This method weights the subjects from the IPD trial to match the aggregate characteristics of the external trial. With this method, the comparison of a censored criterion (typically a survival outcome) is specific at it requires reconstructing individual data (time and censoring indicator) from the aggregated data we have: a Kapaln-Meier curve. We will see how to use the R package `IPDfromKM` to perform that (cf. script `06_survival-outcome-analyses').

### 🧬 Dataset
The dataset used in this project is completely **synthetic** and has been generated for demonstration purpose. The data is simulated in the script `Rscripts/02_data-creation.R`. This dataset contains, among other variables, two types of outcomes, commonly encountered in clinical trials :
- Binary outcome (`binary`),
- Survival outcome (`survival`): time-to-event variable representing survival analysis. A related variable `censor` is included as a binary indicator of censored data (=1 if censored).  
The other variables are :
- `id` : a unique identifier,
- `trt` : a variable indicating the treatment group (T1 or T2),
- `sex`,
- `age`,
- `diag`: time since diagnosis,
- `therapies`: number of therapies previously followed.

The goal here is to implement various adjustment methods rather than really comparing treatments. Therefore, we will only perform simple calculations of Odds Ratios (OR) for the binary outcome and Hazard Ratios for the survival outcome, rather than conducting more advanced analyses.


### 🗂️ Repository structure

- Data/  
   - data.rds
- Rscripts/
   - 00_initialize.R
   - 01_functions.R
   - 02_data-creation.R
   - 03_propensity-score.R
   - 04_maic.R
   - 05_binary-outcome-analyses.R
   - 06_survival-outcome-analyses.R
- Results/
- README.md


