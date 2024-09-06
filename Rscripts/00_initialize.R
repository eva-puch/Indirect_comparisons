rm(list=ls()) # clear the R environment

#--------------------   Librairies   -------------------------------------------

library(dplyr)            # dataframe manipulation
library(MatchIt)          # matching
library(optmatch)         # optimal matching
library(maic)             # performing MAIC
library(tableone)         # calculation of standardized mean differences (SMD)
library(knitr)            # managing R outputs
library(ggplot2)          # plots
library(patchwork)        # handling multiple plots
library(cowplot)          # managing plots
library(survival)         # survival analysis
library(survey)           # survey design
library(survminer)        # survival curves with ggplot
library(epitools)         # calculation of odds ratio
library(forestploter)     # forest plot
library(gridExtra)        # multiple pots in the same graph
library(plotly)           # interactive data visualization

#--------------------   Parametres   -------------------------------------------

colT1 <- "turquoise3"
colT2 <- "darkgoldenrod2"
T1name <- "T1"
T2name <- "T2"

#--------------------   Programmes launches   ----------------------------------
source("Rscripts/01_functions.R")
