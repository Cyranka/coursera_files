remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
par(mfrow = c(1,1))

set.seed(10)
phi_1 <- 0.6
phi_2 <- -0.6
data_ts <- arima.sim(n= 1000,list(ar = c(phi_1,phi_2)))

acf(data_ts,type = "partial",
    main = paste("PACF of time series data with phi_1=",phi_1," and phi_2= ",phi_2))