remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
par(mfrow = c(3,1))


# AR(2) process simulation ------------------------------------------------
phi_1 <- 0.6
phi_2 <- 0.2
data_ts <- arima.sim(n = 500,list(ar = c(phi_1,phi_2)))
plot(data_ts,main = paste("Autoregressive process with phi_1=",phi_1,", phi_2=",phi_2))
acf(data_ts, main = "Autocorrelation function")
acf(data_ts, type = "partial", main = "Partial Autocorrelation Function")

# AR(3) process simulation ------------------------------------------------
rm(list = ls())
phi_1 <- 0.9
phi_2 <- -0.6
phi_3 <- 0.3
data_ts <- arima.sim(n = 500,list(ar = c(phi_1,phi_2, phi_3)))
plot(data_ts,main = paste("Autoregressive process with phi_1=",phi_1,
                          ", phi_2=",phi_2,", phi_3=",phi_3))
acf(data_ts, main = "Autocorrelation function")
acf(data_ts, type = "partial", main = "Partial Autocorrelation Function")


# Beveridge price index ---------------------------------------------------
rm(list = ls())
dev.off()

beveridge <- readr::read_csv("beveridge-wheat-price-index-1500.csv")
beveridge_ts <- ts(beveridge[,2], start = 1500)
plot(beveridge_ts, ylab = "Price", main = "Beveridge wheat price data")
beveridge_MA <- filter(beveridge_ts, rep(1/31,31), sides = 2)
lines(beveridge_MA, col = "red")

##Scaling data points
par(mfrow = c(3,1))
Y = beveridge_ts/beveridge_MA
plot(Y, ylab = "Scaled price", main = "Transformed beveridge wheat price data")
acf(na.omit(Y),
    main = "Autocorrelation function of transformed beveridge data")
acf(na.omit(Y),type = "partial",
    main = "Partial autocorrelation function of transformed beveridge data")

# Fit AR and pick based on AIC --------------------------------------------
ar_fit <- ar(na.omit(Y), order.max = 5)
ar_fit

