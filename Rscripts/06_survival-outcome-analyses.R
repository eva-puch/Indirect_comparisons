# Comparing the survival outcome in both trials/groups (T1 and T2),
# with the three different adjustment methods (matching, iptw, maic).
# For instance, a survival outcome can be the Overall Survival or the Progression-Free Survival.
# The comparison of treatments will be based on Odd Ratios (HR) obtained by Cox models.

rm(list=ls())
source("Rscripts/00_initialize.R")

#--------------------   Data loading   -----------------------------------------

data <- readRDS("Data/data_prop-score.rds")

# For optimal matching:
data_match <- data[!is.na(data$subclass),]

# For IPTW
data_iptw <- data[, !names(data) %in% "subclass"]

# For MAIC
data_ipd <- readRDS("Data/ipd_maic.rds")   # individual data for T1
# we will use a weight of 1 for the supposed AgD' data

#--------------------   MATCHING & IPTW (IPD data for T1 and T2)   -------------
#--------------------   Create 'event' variable   ------------------

data$evt <- 1 - as.numeric(as.character(data$censor))
data_match$evt <- 1 - as.numeric(as.character(data_match$censor))
data_iptw$evt <- 1 - as.numeric(as.character(data_iptw$censor))

#--------------------   Survival R objects   -----------------------

surv <- Surv(time = data$survival, event = data$evt)                    # no adjustment
surv_match <- Surv(time = data_match$survival, event = data_match$evt)  # optimal matching
surv_iptw <- Surv(time = data_iptw$survival, event = data_iptw$evt)     # iptw

#--------------------   Choice of reference group ------------------
# for having coherent results with the previous OR (cf binary outcome)

data$trt <- relevel(data$trt, ref="T1")
data_match$trt <- relevel(data_match$trt, ref="T1")
data_iptw$trt <- relevel(data_iptw$trt, ref="T1")

#--------------------   Cox Models   -------------------------------

# No adjustment
cox <- coxph(surv ~ trt, data = data)
summary(cox)

# Optimal matching
cox_match <- coxph(surv_match ~ trt, data = data_match)
summary(cox_match)

# IPTW
cox_iptw <- coxph(surv_iptw ~ trt, weights = ps_weights, data = data_iptw)
summary(cox_iptw)

#--------------------   Forest plot   ------------------------------

method_names <- c("No adjustment", "Matching", "IPTW")
N1 <- c("400", "300", "698")
N2 <- c("300", "300", "695")

forest_plot(models=c("cox", "cox_match", "cox_iptw"),
            type = "HR",
            methods = method_names,
            N_1 = N1,
            N_2 = N2)

#--------------------   MAIC (IPD for T1 and AgD for T2)  ----------------------
#--------------------   Individual data reconstruction from AgD ----

# import the points from the digitalized Agd KM curve (cf '04_maic.R' script)
digit_km <- read.csv("Data/agd-data_maic/digitalized_km_agd.csv")

n_agd <- 300   # number of total subjects in the AgD population
risk_table <- read.csv("data/agd-data_maic/risk_table_agd.csv")   # table of the number of patients at risk at specific times

pre_process <- preprocess(dat=digit_km,
                          trisk = risk_table[,1],   # risk time points
                          nrisk = risk_table[,2],   # nbr of patients at risk at the risk time points
                          totalpts = n_agd,         # total number of patients in AgD population
                          maxy=1)                   # maximal value of y-axis (1 or 100)

evt_agd <- 162 # total number of events in the AgD population

reconstruct <- getIPD(pre_process, armID=0, tot.events = evt_agd)
summary(reconstruct) # assesses the precision of the estimation
plot(reconstruct)

remove(digit_km, n_agd, pre_process, evt_agd)

# Individual data reconstruction for the AgD population
data_recons <- reconstruct$IPD
colnames(data_recons) = c("survival", "evt", "trt")
data_recons$trt <- T2name
data_recons$maic_weights <- 1
data_recons$pop <- "AgD"

#--------------------   Reconstruction of a MAIC dataframe   -------
data_ipd$evt <- 1 - as.numeric(as.character(data_ipd$censor))
data_ipd$pop <- "IPD"
data_maic <- data_ipd %>% select("survival", "evt", "trt", "maic_weights", "pop")

data_maic <- rbind(data_maic, data_recons)
remove(data_ipd, data_recons)

#--------------------   Survival analysis   ------------------------

surv_maic <- Surv(time = data_maic$survival, event = data_maic$evt)     # survival R object

data_maic$trt <- relevel(data_maic$trt, ref="T1")                       # set up the reference treatment for coherence with previous analysis

cox_maic <- coxph(surv_maic ~ trt, data = data_maic)
summary(cox_maic)

#--------------------   Forest plot   ------------------------------

method_names <- c("No adjustment", "Matching", "IPTW", "MAIC")
N1 <- c("400", "300", "698", "293")
N2 <- c("300", "300", "695", "300")

#png("Results/survival-outcome_foresplot.png", width=800, height=500)
forest_plot(models=c("cox", "cox_match", "cox_iptw", "cox_maic"),
            type = "HR",
            methods = method_names,
            N_1 = N1,
            N_2 = N2)
#dev.off()

