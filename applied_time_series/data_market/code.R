remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/data_market/")
library(tidyverse);library(astsa)

# Load data ---------------------------------------------------------------
x <- readxl::read_excel("annual-sheep-population-1000s-in.xlsx")

# Time plot of the data ---------------------------------------------------
# There is an overall declining trend
plot(ts(x$pop), ylab = "Total", main = "Annual sheep population in the UK",
     lwd = 0.7, col = "blue")

# Check ACF and PACF of regular data --------------------------------------
par(mfrow = c(2,1))

acf(x = x$pop, main = "ACF of annual sheep population in the UK", ylab = "")
pacf(x = x$pop,main = "ACF of annual sheep population in the UK", ylab = "")

##Slowly decaying ACF and PACF cutting at lag 3: Implies an AR model

# Take regular differencing ----------------------------------
differenced_series <- diff(x$pop)

par(mfrow = c(1,1))
plot(ts(differenced_series),
     ylab = "Total",
     main = "Differenced sheep population in the UK",
     lwd = 0.7, col = "red")

par(mfrow = c(2,1))
acf(x = differenced_series,lag.max = 60,
    main = "ACF of differenced sheep population in the UK",
    ylab = "")
pacf(x = differenced_series,lag.max = 60,
     main = "PACF of differenced sheep population in the UK",
     ylab = "")

###Auto-ARIMA just reiterates the PACF and ACF of the series
forecast::auto.arima(x$pop,d = 1, max.p = 5, max.q = 5)

