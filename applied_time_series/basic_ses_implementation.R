remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series")
library(astsa);library(forecast);library(tidyverse)

rain_ts <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip = 1)

# Implementing SES --------------------------------------------------------

# Define parameters -------------------------------------------------------
alpha <- 0.2
forecast_values <- NULL
n <- length(rain_ts)

# Naive forecasting -------------------------------------------------------
forecast_values[1] <- rain_ts[1]
for(i in 1:n){
    forecast_values[i + 1] <- alpha*rain_ts[i] + (1-alpha)*forecast_values[i]
}
remove(i)
forecast_values[101]

