source("Rscripts/00_initialize.R")

#--------------------   Import the dataset   -----------------------------------

data <- readRDS("Data/data.rds")
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

#--------------------   Optimal matching   -------------------------------------
# Optimal matching ----

res_opti <- matched_smd(treatment="trtb", ps="prop_score", data=data[!is.na(data$prop_score),],
                        method="optimal", ratio=1,
                        allvars=c("therapies", "diag", "sex", "age"),
                        catvars = c("sex"),
                        labels = c("Number of therapies", "Time since diagnosis", "Sex", "Age"),
                        strata = "trt")

data <- left_join(data, res_opti$matched[,c("id","subclass")], by="id")


# Variable's distribution before (left) and after (right) matching ----

match_distrib(data = data,
              variable = "prop_score",
              match_var = "subclass",
              group = "trt",
              absname = "Propensity score")

match_distrib(data = data,
              variable = "age",
              match_var = "subclass",
              group = "trt",
              absname = "Age at inclusion")

match_distrib(data = data,
              variable = "diag",
              match_var = "subclass",
              group = "trt",
              absname = "Time since diagnosis")

match_distrib(data = data,
              variable = "therapies",
              type = "quali",
              match_var = "subclass",
              group = "trt",
              absname = "Number of previous therapies")

match_distrib(data = data,
              variable = "sex",
              type = "quali",
              match_var = "subclass",
              group = "trt",
              absname = "Sex")

#--------------------   IPTW   -------------------------------------------------
# IPTW ----
res_iptw <- iptw_smd(data = data,
                     treatment = "trtb",
                     ps = "prop_score",
                     allvars=c("therapies", "diag", "sex", "age"),
                     catvars = c("sex"),
                     labels = c("Number of therapies", "Time since diagnosis", "Sex", "Age"),
                     strata = "trt")

data <- left_join(data, res_iptw$weighted[,c("id", "ps_weights")], by = "id")

# Weights distribution ----

by(data$ps_weights, data$trt, summary)

# boxplot
plot_ly(data,
        x = ~ps_weights,
        y = ~trt,
        type = "box",
        orientation = "h",
        boxpoints = FALSE,
        color = ~trt,
        colors = c(colT2, colT1)) %>%
  layout(xaxis = list(title = "IPTW Weights"),
         yaxis = list(title = ""),
         legend = list(
           title = list(text = "Treatment"),
           traceorder = "reversed"))

# density
ggplot(data, aes(x = ps_weights, fill = trt)) +
  geom_density(alpha = 0.5) +
  labs(x = "IPTW Weights", y = "") +
  theme_minimal() +
  scale_fill_manual(name = "Treatment", values = c(colT1,colT2), breaks = c("T1", "T2")) +
  theme(legend.position = "left") +
  guides(fill = guide_legend(title = "Treatment"))


# Variable's distribution before and after IPTW ----

iptw_distrib(data = data,
             variable = "prop_score",
             weights = "ps_weights",
             group = "trt",
             absname = "Propensity score")

iptw_distrib(data = data,
             variable = "age",
             weights = "ps_weights",
             group = "trt",
             absname = "Age at inclusion")

iptw_distrib(data = data,
             variable = "diag",
             weights = "ps_weights",
             group = "trt",
             absname = "Time since diagnosis")

iptw_distrib(data = data,
             variable = "therapies",
             weights = "ps_weights",
             group = "trt",
             absname = "Number of previous therapies",
             type = "quali")


#------------   Export dataframe with the added variables   ----------------------
# The added variables are : prop_score, subclass, ps_weights

# RDS format to keep variable's type
rownames(data)=NULL
saveRDS(data, file = "Data/data_prop-score.rds")

