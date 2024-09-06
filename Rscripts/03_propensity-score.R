source("Rscripts/00_initialize.R")

#--------------------   Import the dataset   -----------------------------------

data <- readRDS("data/processed_data/data.rds")
summary(data)

#--------------------   Propensity score: estimation   -------------------------

# GLM model including relevant variables, qualitative or quantitative : sex, age, diag, therapies
data <- data[complete.cases(data[, c("sex", "age", "diag", "therapies")]), ] # in case of missing data

model <- glm(trt ~ sex + age + diag + therapies, data=data, family=binomial)
summary(model)

data$prop_score <- predict(model, type="response")
