remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# Simulation of an ARIMA (2,1,1) process ----------------------------------


# Define parameters -------------------------------------------------------
phi <- c(0.7,0.2) ##Autoregressive parameters
beta <- c(0.5) ##Moving average parameter
sigma <- 3 ##Variance of the residuals
m <- 10000 ##Total observations


# Simulate ----------------------------------------------------------------
set.seed(5)

simulated_arima <- arima.sim(n=m,list(order = c(2,1,1), ar = phi, ma=beta))

# Plot simulated series ---------------------------------------------------
plot(simulated_arima, ylab = "",main = "Simulated ARIMA (2,1,1)",
     col = "blue", lwd = 0.5)

# Get the ACF of the ARIMA (2,1,1) process --------------------------------
acf(simulated_arima) ##Notice how it decays extremely slowly

# Difference the series ---------------------------------------------------
diff_simulated_arima <- base::diff(simulated_arima)

# Plot the differenced series ---------------------------------------------
plot(diff_simulated_arima, ylab = "",main = "Differenced simulated ARIMA (2,1,1)",
     col = "red", lwd = 0.5) ##Now it looks stationary

# Obtain the ACF and PACF of the differenced series --------------------------------
par(mfrow = c(2,1))
acf(diff_simulated_arima, main = "ACF differenced simulated ARIMA (2,1,1)")
pacf(diff_simulated_arima,main = "PACF differenced simulated ARIMA (2,1,1)")

# Estimate model statistics -----------------------------------------------
model_statistics <- sarima(simulated_arima,2,1,1,0,0,0)

model_statistics$fit %>% broom::tidy() %>%
    mutate_if(is_numeric,funs(round(.,2))) ##Estimated model parameters

# Using forecast ----------------------------------------------------------
forecast_arima <- forecast::auto.arima(simulated_arima)
forecast_arima$coef %>% broom::tidy()
forecast_arima


# Fitting alternative models ----------------------------------------------

# ARMA (4,0,0) on the differenced series ----------------------------------
fit_1 <- arima(diff_simulated_arima, order= c(4,0,0))
fit_1$coef %>% broom::tidy() ##Coefficients
fit_1$aic ##Akaike informationc criterion

# ARMA (2,0,1) on the differenced series ----------------------------------
fit_2<-arima(diff_simulated_arima, order=c(2,0,1))
fit_2$coef %>% broom::tidy()

# ARIMA (2,1,1) on the original series ---------------------------------
fit_3<-arima(simulated_arima, order=c(2,1,1))
fit_3$coef %>% broom::tidy()
