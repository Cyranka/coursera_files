remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series")
library(astsa);library(forecast);library(tidyverse)

# Plot the data -----------------------------------------------------------
plot(USAccDeaths, lwd = 0.85, col = "red",
     ylab = "Total deaths",
     main = "Monthly total accidental deaths")

# Remove seasonality trend ------------------------------------------------
plot(diff(USAccDeaths,12), lwd = 0.85, col = "red",
     ylab = "Total deaths",
     main = "Seasonality removed monthly total accidental deaths")

# Remove non-seasonal trend -----------------------------------------------
acData <- diff(diff(USAccDeaths),12)

par(mfrow = c(2,1))
acf(acData, main = "ACF of differenced data")
pacf(acData, main = "PACF of differenced data")

# Fit best model ----------------------------------------------------------
model_fit <- arima(USAccDeaths,order = c(0,1,1),
                   seasonal = list(order = c(0,1,1),period = 12))

sarima_fit <- sarima(USAccDeaths, 0,1,1,0,1,1,12)
sarima_fit$ttable

# Forecast ----------------------------------------------------------------
predictions <- astsa::sarima.for(USAccDeaths,12,0,1,1,0,1,1,12)
predictions