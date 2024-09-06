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

#--------------------   Propensity score: description   ------------------------

by(data$prop_score, data$trt, summary)

# boxolot
plot_ly(data,
        x = ~prop_score,
        y = ~trt,
        type = "box",
        orientation = "h",
        boxpoints = FALSE,
        color = ~trt,
        colors = c(colT2,colT1)) %>%
  layout(xaxis = list(range = c(-0.1, 1), title = "Propensity Score"),
         yaxis = list(title = ""),
         legend = list(
           title = list(text = "Treatment"),
           traceorder = "reversed"))

# density
ps <- ggplot(data, aes(x = prop_score, fill = trt)) +
  geom_density(alpha = 0.5) +
  labs(x = "Propensity score", y = "") +
  theme_minimal() +
  scale_fill_manual(name = "Treatment", values = c(colT1,colT2), breaks = c("T1", "T2")) +
  theme(legend.position = "left") +
  guides(fill = guide_legend(title = "Treatment"))
ps
