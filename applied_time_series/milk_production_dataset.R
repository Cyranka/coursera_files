remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(tidyverse);library(astsa)


# Load data ---------------------------------------------------------------
x <- read_csv("monthly-milk-production-pounds-p.csv")

# Time plot of the data ---------------------------------------------------
plot(ts(x$total), ylab = "Total", main = "Milk production by cow pounds (1962 - 1975)",
     lwd = 0.7, col = "blue")

# Check ACF and PACF of regular data --------------------------------------
par(mfrow = c(2,1))
acf(x = x$total,lag.max = 48, main = "ACF of milk production by cow in pounds", ylab = "")
pacf(x = x$total,lag.max = 48, main = "PACF of milk production by cow in pounds", ylab = "")

# Take seasonal and regular differencing ----------------------------------
differenced_series <- diff(diff(x$total),lag = 12) ##Monthly data

# Plot differenced data ---------------------------------------------------
par(mfrow = c(1,1))
plot(ts(differenced_series), ylab = "Total", main = "Differenced milk production by cow pounds (1962 - 1975)\nRegular and seasonal differencing",
     lwd = 0.7, col = "red")

# Check ACF and PACF of differenced data ----------------------------------
par(mfrow = c(2,1))
acf(x = differenced_series,lag.max = 60,
    main = "ACF of differenced milk production by cow in pounds\nRegular and seasonal differencing",
    ylab = "")
pacf(x = differenced_series,lag.max = 60,
     main = "PACF of differenced milk production by cow in pounds\nRegular and seasonal differencing",
     ylab = "")

# Create models
p <- c(0,1)
d <- 1
q <- 1
P <- c(0,1,2,3)
D <- 1
Q <- c(0,1)

model_grid <- expand.grid(p,d,q,P,D,Q) %>%
    magrittr::set_colnames(c("p", "d", "q", "P", "D","Q")) %>%
    as_tibble()
 
models <- lapply(1:15, function(i){
    print(i)
    arima(differenced_series,
          order = c(model_grid$p[i],
          model_grid$d[i],
          model_grid$q[i]),
          seasonal = list(order = c(model_grid$P[i],
                           model_grid$D[i],
                           model_grid$Q[i]),period = 12))})

p_values <- lapply(1:length(models), function(i)
    Box.test(models[[i]]$residuals,lag =log(length(models[[i]]$residuals))))

aic_dataset <- tibble(
    model = 1:15,
    aic = sapply(models, function(i)i$aic),
    p_values = sapply(p_values, function(i)i$p.value))

View(aic_dataset)
# Best model is model 11 ------------------------------------------
par(mfrow= c(1,1))
model<- arima(x=x$total, order = c(0,1,1), seasonal = list(order=c(1,1,1), period=12))

plot(forecast::forecast(model))
