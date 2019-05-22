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

# Section 8.5: ACF and PACF -----------------------------------------------
ggAcf(uschange[,"Consumption"],main = "") ##Autocorrelation plot
ggPacf(uschange[,"Consumption"],main = "") ##Partial autocorrelation plot

fit2 <- Arima(uschange[,"Consumption"], order = c(3,0,0))
summary(fit2)

fit3 <- auto.arima(uschange[,"Consumption"],seasonal = FALSE,
                   stepwise = FALSE, approximation = FALSE) ##This leads to the consideration of other models

summary(fit3)

# Section 8.7: ARIMA modelling --------------------------------------------
rm(list = ls())

##Remove seasonality so seasonal differencing is unnecessary
eeadj <- elecequip %>% stl(s.window = "periodic") %>% seasadj()

autoplot(elecequip, alpha =0.5, color = "gray") + 
    autolayer(eeadj) ##Plot the series

eeadj %>% diff(lag = 1) %>% ggtsdisplay(main = "") ##Take first differences
eeadj %>% diff(lag = 1) %>% urca::ur.kpss() %>% summary() ##This also confirms the data

(fit <- Arima(eeadj, order = c(3,1,1))) ##Took first differences, but no seasonal differences

checkresiduals(fit) ##Ljung-box text confirms residuals are white-noise
autoplot(forecast(fit)) ##c = 0 and first differences, so forecasts will converge to a constant

# Section 8.9: Sarima model -----------------------------------------------
rm(list = ls())

autoplot(euretail) + ylab("Retail index") + xlab("Year")

euretail %>% diff(lag = 4) %>% ggtsdisplay() ##Remove seasonal difference
euretail %>% diff(lag = 4) %>% diff() %>% ggtsdisplay()

euretail %>%
    Arima(order = c(0,1,1),seasonal = c(0,1,1)) %>%
    residuals() %>% ggtsdisplay() ###

fit3 <- Arima(euretail, order = c(0,1,3), seasonal = c(0,1,1))
checkresiduals(fit3)

fit3 %>% forecast(h = 12) %>% autoplot()

# Section 8.9: Cortecosteroid drug sales in Australia ---------------------
lh02 <- log(h02) ##Stabilize variance
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales" = lh02) %>%
    autoplot(facets = TRUE) + 
    xlab("Year") + ylab("")

lh02 %>% diff(lag = 12) %>%
    ggtsdisplay(xlab = "Year",
                main = "Seasonally differenced H02 scripts")

fit <- Arima(h02, order = c(3,0,1),seasonal = c(0,1,2),lambda = 0) ##Log transform with lambda = 0
summary(fit)
checkresiduals(fit, lag = 36)
