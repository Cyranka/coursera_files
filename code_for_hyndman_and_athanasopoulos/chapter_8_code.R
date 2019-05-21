remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal);library(urca)

# Section 8.1: Stationarity -----------------------------------------------
Box.test(diff(goog200),
         lag = 10,
         type = "Ljung-Box") ##Ljung-Box test of differenced series


# Section 8.1: Seasonal differencing --------------------------------------
cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
    autoplot(facets = TRUE) + 
    xlab("Year") + ylab("") + 
    ggtitle("Antidiabetic drug sales")

# Section 8.1: Seasonal and first-order differencing ----------------------
cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" = diff(log(usmelec),12),
      "Doubly\nd differenced logs" = diff(diff(log(usmelec),12),1)) %>%
    autoplot(facets = TRUE) + 
    xlab("Year") + ylab("") + 
    ggtitle("Monthly US net electricity generation")

# Section 8.1: unit-root tests using urca ---------------------------------
goog %>% ur.kpss() %>% summary()
goog %>% diff(1) %>% ur.kpss() %>% summary()

ndiffs(goog) ##This function tells you the number of first differences to produce a stationary data

usmelec %>% log() %>% nsdiffs() ##Number of seasonal differences
usmelec %>% log() %>% diff(lag= 12) %>% ndiffs()


# Section 8.3: non-seasonal ARIMA models ----------------------------------
autoplot(uschange[,"Consumption"]) + 
    xlab("Year") + 
    ylab("Quarterly percentage change")

(fit <- auto.arima(uschange[,"Consumption"])) ##Auto-arima model

fit %>% forecast(h = 10) %>%
    autoplot(include = 80)

fit %>% forecast(h = 80) %>%
    autoplot(include = 80) ##Notice that the forecasts stabilize with same s.d and mean of the data
