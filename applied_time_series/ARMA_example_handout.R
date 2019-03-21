remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)


# Example: ARMA Model -----------------------------------------------------
par(mfrow = c(3,1))
plot(discoveries,
     main = "Time series number of major scientific discoveries in a year",
     ylab = "Discoveries")

acf(discoveries, main = "ACF of number of scientific discoveries")
pacf(discoveries, main = "PACF of number of major scientific discoveries")

# Fit different ARMA models and obtain AIC --------------------------------
models <- expand.grid(x = c(0:3),
                      y = c(0:3)) %>% as_tibble()

p <- sapply(1:nrow(models), function(i)AIC(arima(discoveries, order = c(models$x[i],0,models$y[i]))))
models$AIC <- p

# Using forecast package --------------------------------
library(forecast)
forecast::auto.arima(discoveries, d = 0, approximation = FALSE)
forecast::auto.arima(discoveries, d = 0, approximation = TRUE)

##set BIC as selection parameter
forecast::auto.arima(discoveries, d = 0,ic = "bic",approximation = TRUE)

##Regular AIC as selection parameter
forecast::auto.arima(discoveries, d = 0,ic = "aic",approximation = TRUE)