remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series")
library(astsa);library(forecast);library(tidyverse)


# Read data ---------------------------------------------------------------
x <- readxl::read_excel("volume-of-money-abs-definition-m.xlsx", sheet = 1)
money_data <- ts(x$m1, start = c(1960,2), frequency = 12) ##Define seasonality

# Plot data ---------------------------------------------------------------
par(mfrow = c(3,1))

plot(money_data, main = "Time plot of volume of money",
     ylab = "Volume", xlab = "Time")
acf(money_data, main = "ACF of volume of money")
pacf(money_data, main = "PACF of volume of money")

# Define transformations and smoothing parameters -------------------------
data <- money_data
N <- length(data)
alpha <- 0.7
beta <- 0.5

# Define storage variables ------------------------------------------------
forecast <- NULL
level <- NULL
trend <- NULL

# Initialize level and trend ----------------------------------------------
level[1] <- data[1]
trend[1] <- data[2] - data[1]

# Define initial naive forecasts ------------------------------------------
forecast[1] <- data[1]
forecast[2] <- data[2]

# Define for loop to forecast the series ----------------------------------
for(n in 2:N){
    ##Update series
    level[n] <- alpha*data[n] + 
        (1 - alpha)*(level[n-1] + trend[n-1])
    
    ##Update trend
    trend[n] <- beta*(level[n] - level[n-1]) + 
        (1 - beta)*trend[n-1]
    
    ##Update forecast
    forecast[n + 1] <- level[n] + trend[n]
}
remove(n)

# Investigate series ------------------------------------------------------
forecast[3:N]

# Estimate using Holt-Winters routine -------------------------------------
naive_hw <- HoltWinters(data, alpha = 0.5, beta = 0.5, gamma = FALSE)
naive_hw$fitted %>%
    as_tibble()

# Plot Holt-Winters output ------------------------------------------------
par(mfrow = c(1,2))
plot(naive_hw,
     main = "Holt-winters with random parameters")

optimized_hw <- HoltWinters(data, gamma = FALSE)
plot(optimized_hw, main = "Holt-winters with optimal parameters")

