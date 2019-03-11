remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(tidyverse)

##AR(1) process
set.seed(2020)
phi1 = 0.4
X_ts <- arima.sim(list(ar = c(phi1)), n=1000)
par(mfrow=c(2,1))
plot(X_ts,main=paste("AR(1) Time Series, phi1=",phi1),
     ylab = "Value at time T")
X_acf = acf(X_ts, main="Autocorrelation of AR(1) Time Series")
head(X_acf$acf) ##first coefficients
mean(X_ts) ##Mean
var(X_ts) ##Variance

# Simulating an AR(2) model -----------------------------------------------
##The closer to 1, the higher the dependency on history

set.seed(2017)
X_ts <- arima.sim(list(ar = c(0.7,0.2)), n = 1000)

par(mfrow = c(2,1))
plot(X_ts, main = "AR(2) Time Series, phi_1 = 0.7, phi_2 = 0.2",
     ylab = "Value at time T")

X_acf <- acf(X_ts, main = "Autocorrelation of AR(2) Time Series")


# Simulation of AR(2) processes -------------------------------------------
rm(list = ls())
par(mfrow=c(2,1))

phi1 = .7; phi2 = 0.2;
X.ts <- arima.sim(list(ar = c(phi1, phi2)), n=1000)
plot(X.ts,main=paste("AR(2) Time Series, phi1=",phi1,"phi2=",phi2))
acf(X.ts,main="ACF")


