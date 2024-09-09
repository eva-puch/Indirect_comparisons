# Loading the 00_initialize.R script
source("Rscripts/00_initialize.R")

#--------------------   Simulating data   --------------------------------------
set.seed(123)

# generate data for first group (treatment T1)
n1 <- 400
dataT1 <- tibble(
  id = 1:n1,                                                              # unique id
  trt = "T1",                                                             # treatment = T1
  sex = rbinom(n1, 1, 0.35),                                              # sex (0,1)
  age  = round(pmax(pmin(rnorm(n1, mean = 63.4, sd = 10), 90), 37), 1),   # age at inclusion
  diag = round(pmax(pmin(rnorm(n1, mean = 80, sd = 65), 450), 1),1),      # time since diagnosis (months)
  therapies = round(pmax(pmin(rnorm(n1, mean = 2.3, sd = 1.4), 7), 0)),   # number of previous therapies
  binary = rbinom(n1, 1, 0.97),                                           # binary outcome (e.g. : at least one response to treatment)
  survival = pmax(pmin(rweibull(n1, 1.5, 57), 65), 1),                    # survival outcome (e.g : progression-free survival)
  censor = rbinom(n1, 1, 0.35)                                            # censor indicator for the survival outcome
)

# generate data for the second group (treatment T2)
n2 <- 300
dataT2 <- tibble(
  id = n1+1:n2,                                                           # unique id
  trt = "T2",                                                             # treatment = T1
  sex = rbinom(n2, 1, 0.30),                                              # sex (0,1)
  age  = round(pmax(pmin(rnorm(n2, mean = 58.3, sd = 10), 85), 32),1),    # age at inclusion
  diag = round(pmax(pmin(rnorm(n2, mean = 90, sd = 60), 290), 1), 1),     # time since diagnosis (months)
  therapies = round(pmax(pmin(rnorm(n2, mean = 1.7, sd = 1.3), 7), 0)),   # number of previous therapies
  binary = rbinom(n2, 1, 0.94),                                           # binary outcome : at least one response to treatment,
  survival = pmax(pmin(rweibull(n2, 1.5, 45), 55), 1),                    # survival outcome (e.g : progression-free survival)
  censor = rbinom(n2, 1, 0.50)                                            # censor indicator for the survival outcome
)

# Merging the two dataframes
data <- rbind(dataT1, dataT2)
remove(n1, n2, dataT1, dataT2)

#--------------------   Adjust the type of some variables   --------------------

# Add a binary treatment variable to 0(control)/1(treatment)
# T1 = 1 (treatment)
# T2 = 0 (control)
data$trtb <- as.factor(ifelse(data$trt == "T1", 1, 0))

# Modifying some type's variable
data$id <- as.character(data$id)
data$sex <- as.factor(data$sex)
data$trt <- as.factor(data$trt)
data$trt <- relevel(data$trt, ref="T2")
data$binary <- as.factor(data$binary)
data$censor <- as.factor(data$censor)

summary(data)
head(data)

#--------------------   Export the dataframe   ---------------------------------

# RDS format to keep variable's type
rownames(data)=NULL
saveRDS(data, file = "Data/data.rds")
