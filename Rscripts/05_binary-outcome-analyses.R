# Comparing the binary outcome in both trials/groups (T1 and T2),
# with the three different adjustment methods (matching, iptw, maic).
# For instance, a binary outcome can be a response (1) or no response (0) to the treatment.
# The comparison of treatments will be based on Odd Ratios (OR).

rm(list=ls())
source("Rscripts/00_initialize.R")

#--------------------   Data loading   -----------------------------------------

data <- readRDS("Data/data_prop-score.rds")

# For optimal matching:
data_match <- data[!is.na(data$subclass),]

# For IPTW
data_iptw <- data[, !names(data) %in% "subclass"]

# For MAIC
data_ipd <- readRDS("Data/ipd_maic.rds")            # individual data for T1
data_agd <- readRDS("Data/agd-data_maic/agd.rds")   # only use aggregated data for T2
# we will use a weight of 1 for the supposed AgD' data
data_agd$maic_weights <- 1
data_maic <- rbind(data_ipd, data_agd)
remove(data_ipd, data_agd)


#--------------------   NO ADJUSTMENT   ----------------------------------------

# Create a contingency table for treatments and responses
count_table <- as.matrix(table(data$trt, data$binary))

oddsratio(count_table, method = "wald")                       # T2 as reference : OR(ref=T2) =
or <- oddsratio(count_table[, c(2, 1)], method="wald")        # T1 as reference : OR(ref=T1) =

#--------------------   OPTIMAL MATCHING   -------------------------------------

# Create a contingency table for treatments and responses
count_table <- as.matrix(table(data_match$trt, data_match$binary))

oddsratio(count_table, method = "wald")                       # T2 as reference : OR(ref=T2) =
or_match <- oddsratio(count_table[, c(2, 1)], method="wald")  # T1 as reference : OR(ref=T1) =

#--------------------   IPTW   -------------------------------------------------

# Create a weighted contingency table for treatments and responses
count_table <- round(as.matrix(xtabs(ps_weights ~ trt + binary, data = data_iptw)),0)

oddsratio(count_table, method = "wald")                       # T2 as reference : OR(ref=T2) =
or_iptw <- oddsratio(count_table[, c(2, 1)], method="wald")   # T1 as reference : OR(ref=T1) =

#--------------------   MAIC   -------------------------------------------------

# Create a weighted contingency table for treatments and responses
count_table <- round(as.matrix(xtabs(maic_weights ~ trt + binary, data = data_maic)),0)

oddsratio(count_table, method = "wald")                       # T2 as reference : OR(ref=T2) =
or_maic <- oddsratio(count_table[, c(2, 1)], method="wald")   # T1 as reference : OR(ref=T1) =

remove(count_table)

#--------------------   Forest plot of results   -------------------------------

method_names <- c("No adjustment", "Matching", "IPTW", "MAIC")
N1 <- c("400", "300", "698", "295")
N2 <- c("300", "300", "695", "300")

#png("Results/binary-outcome_forestplot.png", width=800, height=500)
forest_plot(models=c("or", "or_match", "or_iptw", "or_maic"),
            type = "OR",
            methods = method_names,
            N_1 = N1,
            N_2 = N2)
#dev.off()
