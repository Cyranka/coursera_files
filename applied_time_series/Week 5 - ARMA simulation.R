remove(list= ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# ARMA (p,q) Simulation ---------------------------------------------------
set.seed(500)
data <- arima.sim(list(order = c(1,0,1), ar =0.7,ma = 0.2),n = 1000000)

# Plot the data, ACF, and PACF --------------------------------------------
par(mfrow = c(3,1))

plot(data, main = "ARMA (1,1) time series: phi_1 = 0.7, theta_1 = 0.2",
     xlim = c(0,400))
acf(data, main = "Autocorrelation of ARMA(1,1), phi_1 = 0.7, theta_1 = 0.2")
pacf(data, main = "Partial autocorrelation of ARMA(1,1), phi_1 = 0.7, theta_1 = 0.2")

# Obtain the ACF ----------------------------------------------------------
head(acf(data,plot = F)$acf)


# Theoretical values for the ACF ------------------------------------------
rho_1 <- ((1  + 0.7*0.2)*(0.7 + 0.2))/(1 + 0.2**2 + (2*0.7*0.2))
rho_2 <- 0.7*rho_1
rho_3 <- 0.7*rho_2
