remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse);library(forecast)

# Load data ---------------------------------------------------------------
x <- read_csv("monthly-sales-for-a-souvenir-sho.csv")

# Plot  time --------------------------------------------------------------
plot(ts(x$sales), main = "Time-series plot of monthly sales",
     ylab = "Total sales", xlab = "Time", lwd = 0.75,
     col = "blue")

# Transform the data to control variance -----------------------------------
series_log <- log(x$sales)
plot(ts(series_log), main = "Time-series plot of log monthly sales",
     ylab = "Total sales", xlab = "Time", lwd = 0.75,
     col = "red") ##Notice how the variance shrunk

# Perform non-seasonal and seasonal differencing of order 1 ---------------
diff_series <- diff(diff(series_log),lag = 12)
plot(ts(diff_series),main = "Differenced time-series plot of log monthly sales",
     ylab = "", xlab = "Time", lwd = 0.75,
     col = "forestgreen") 

# Check the ACF and PACF of the data --------------------------------------
par(mfrow =c(2,1))
acf(diff_series, lag.max = 50, 
    main = "ACF of differenced log data")

pacf(diff_series, lag.max = 50, 
    main = "PACF of differenced log data") ##No seasonal lags, but 1 AR(1)

# Fit models --------------------------------------------------------------
p <- c(0,1)
d <- 1
q <- c(0,1)
P <- c(0,1)
D <- 1
Q <- c(0,1,2)

model_grid <- expand.grid(p,d,q,P,D,Q) %>%
    magrittr::set_colnames(c("p", "d", "q", "P", "D","Q")) %>%
    as_tibble()

models <- lapply(1:24, function(i){
    print(i)
    arima(diff_series,
          order = c(model_grid$p[i],
                    model_grid$d[i],
                    model_grid$q[i]),
          seasonal = list(order = c(model_grid$P[i],
                                    model_grid$D[i],
                                    model_grid$Q[i]),period = 12))})

p_values <- lapply(1:length(models), function(i)
    Box.test(models[[i]]$residuals,lag =log(length(models[[i]]$residuals))))



aic_dataset <- tibble(
    model = 1:24,
    aic = sapply(models, function(i)i$aic),
    p_values = sapply(p_values, function(i)i$p.value))

model_grid %>% slice(16)

# Best model is model 11 ------------------------------------------
par(mfrow= c(1,1))
model<- arima(x=series_log, order = c(1,1,1), seasonal = list(order=c(1,1,1), period=12))

plot(forecast::forecast(model))


# Using sarima function ---------------------------------------------------
model_2 <- astsa::sarima.for(series_log,12,1,1,1,1,1,1,12)


plot.ts(c(x$sales,exp(model_2$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)