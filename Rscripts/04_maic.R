source("Rscripts/00_initialize.R")

# Assuming we have individual-patient data for group T1
# and only aggregated data for group T2

#--------------------   Import the dataset   -----------------------------------

data <- readRDS("Data/data.rds")

ipd <- data[data$trt == 'T1', ] # IPD data
agd <- data[data$trt == 'T2', ] # AgD data

# binary variables have to be coded as numeric 0/1:
ipd$sex <- as.numeric(ifelse(ipd$sex=="0", 0, 1))

#--------------------   AgD data summary   -------------------------------------

# Age (continuous)
mean_age <- mean(agd$age, na.rm=TRUE)
sd_age <- sd(agd$age, na.rm=TRUE)

# Sex (qualitative)
prop_male <- mean(agd$sex=='1')

# Time since diagnostic (continuous)
mean_diag <- mean(agd$diag, na.rm=TRUE)
sd_diag <- sd(agd$diag, na.rm=TRUE)

# Number of previous therapies (continous)
mean_therapies <- mean(agd$therapies, na.rm=TRUE)
sd_therapies <- sd(agd$therapies, na.rm=TRUE)

#--------------------   MAIC   -------------------------------------------------

res_maic <- maic_adjust(ipd,
                        variables = c("age", "sex", "diag", "therapies"),
                        target_values = c(mean_age, prop_male, mean_diag, mean_therapies),
                        type = c("mean", "proportion", "mean", "mean"))

ipd$maic_weights <- as.numeric(res_maic$wgt)

# Weights distribution ----

plot_ly(
  x = ~ipd$maic_weights,
  type = "histogram",
  nbinsx = 30,
  marker = list(
    color = "#53868B",     
    line = list(
      color = "white",
      width = 1   
    )
  ),
  hovertemplate = ~paste(
    "Frequency: %{y:.2f}<br>"  # Affiche la fréquence y avec deux décimales
  )
) %>%
  layout(
    xaxis = list(title = "MAIC weights"),
    yaxis = list(title = "Frequency")
  )

# Distributions of variable in IPD population, before and after MAIC ----

maic_distrib(table = ipd,
             variable = "age",
             var_type = "quanti",
             opac = 0.7)

maic_distrib(table = ipd,
             variable = "sex",
             var_type = "quali",
             opac = 0.7)

maic_distrib(table = ipd,
             variable = "diag",
             var_type = "quanti",
             opac = 0.7)

maic_distrib(table = ipd,
             variable = "therapies",
             var_type = "quali",
             opac = 0.7)