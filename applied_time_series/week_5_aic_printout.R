remove(list= ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# AIC and Model Quality ---------------------------------------------------

# Generate data -----------------------------------------------------------
set.seed(43)
data <- arima.sim(list(order = c(2,0,0),ar = c(0.7,-0.2)),n = 2000)

par(mfrow = c(3,1))
plot(ts(data))
acf(ts(data),main = "ACF")
pacf(ts(data),main = "PACF")

# Estimation using ARIMA command --------------------------------------------
phi_hat <- arima(data,order = c(2,0,0), include.mean = FALSE)


# Table of AIC and SSE values ---------------------------------------------
aic_values <- sapply(1:5, function(i)arima(data,order = c(i,0,0), include.mean =F)$aic)
sse <- sapply(1:5, function(i)sum(resid(arima(data,order = c(i,0,0), include.mean =F))**2))

tibble(
    AIC = aic_values,
    SSE = sse
)