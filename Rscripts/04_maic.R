rm(list=ls())
source("Rscripts/00_initialize.R")

# Assuming we have individual-patient data for group T1
# and only aggregated data for group T2

#--------------------   Import the dataset   -----------------------------------

data <- readRDS("Data/data.rds")

ipd <- data[data$trt == 'T1', ] # IPD data
agd <- data[data$trt == 'T2', ] # AgD data

# binary variables have to be coded as numeric 0/1:
ipd$sex <- as.numeric(ifelse(ipd$sex=="0", 0, 1))

# Export IPD in .csv format (for maic_shiny app)
write.csv(ipd, "Data/ipd-data_maic.csv", row.names=FALSE)


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

remove(mean_age, sd_age, prop_male, mean_diag, sd_diag, mean_therapies, sd_therapies)

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
    yaxis = list(title = "Frequency"),
    plot_bgcolor = 'rgba(240, 240, 240, 1)',
    paper_bgcolor = 'white'
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

#------------   Export IPD with the MAIC weights   -----------------------------
# RDS format to keep variable's type
rownames(ipd)=NULL
saveRDS(ipd, file = "Data/ipd_maic.rds")

#------------   Export AgD   ---------------------------------------------------
rownames(agd)=NULL
saveRDS(agd, file = "Data/agd-data_maic/agd.rds")

#------------   KM curve from AgD (specific to survival outcome)  --------------
surv_agd <- Surv(time = agd$survival, event = 1-as.numeric(as.character(agd$censor)))
km_agd <- survfit(surv_agd ~ 1, data = agd)

png(filename = "Data/agd-data_maic/km_agd.png")
plot(km_agd, main = "KM curve from aggregated data (AgD)")
while (!is.null(dev.list())) {
  dev.off()
}


# Obtain the 'patients at risk' table from AgD
ggsurv_agd <- ggsurvplot(
  km_agd,
  data = agd,
  pval = FALSE,
  conf.int = TRUE,
  risk.table = TRUE,
  ggtheme = theme_minimal())

risk_table_agd <- ggsurv_agd$table$data[,2:3]

write.csv(risk_table_agd, "Data/agd-data_maic/risk_table_agd.csv", row.names=FALSE)

# Reconstruction of IPD data for the AgD population from KM curves (IPDfromKM package) ----

#points <- getpoints("data/agd-data_maic/km_agd.png", x1=0, x2=50, y1=0, y2=1)

# export this 'point' datafame
#write.csv(points, file = "data/agd-data_maic/digitalized_km_agd.csv", row.names=FALSE)
