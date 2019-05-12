remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal)

# Simple exponential smoothing --------------------------------------------
oildata <- window(oil, start = 1996) ##Oil is annual data
autoplot(oildata) + 
    ylab("Oil (millions of tonnes)") + xlab("Year")

#Estimate parameters for oildata using simple exponential smoothing.
fc <- ses(oildata, h = 5)
round(accuracy(fc),2) ##Check accuracy: This is different from ts-cv and works only against future values
autoplot(fc) + 
    autolayer(fitted(fc), series = "Fitted") + 
    ylab("Oil (millions of tonnes)") + xlab("Year")##

# Exponential smoothing with a trend --------------------------------------
rm(list = ls())
air <- window(ausair, start = 1990) ##Window the data
autoplot(air)

fc <- holt(air, h = 5)

autoplot(fc) ##Notice it is a basic trend estimate

# Introducing a damp parameter --------------------------------------------
fc <- holt(air, h = 15)
fc2 <- holt(air, damped = TRUE, phi = 0.9, h = 15)

autoplot(air) + 
    autolayer(fc, series = "Holt's method", PI = FALSE) + 
    autolayer(fc2, series = "Damped Holt's method", PI = FALSE) + 
    ggtitle("Forecasts from Holt's method") + xlab("Year") + 
    ylab("Air passengers in Australia (millions)") + 
    guides(color = guide_legend(title = "Forecast"))

# Comparison of SES, Holt’s method, and Holt’s with damped parameter --------
# Exponential smoothing with a trend seems to be useful for yearly data
autoplot(livestock) + 
    xlab("Year") + 
    ylab("Livestock, sheep in Asia (millions)")

#Use time-series cross validation to compare methods: one step ahead
e1 <- tsCV(livestock, ses, h = 1) ##No use of future values
e2 <- tsCV(livestock, holt, h = 1) ##No use of future values
e3 <- tsCV(livestock, holt, damped = TRUE, h = 1) ##No use of future values

#MSE
mean(e1**2, na.rm = TRUE)
mean(e2**2, na.rm = TRUE)
mean(e3**2, na.rm = TRUE)

#MAE
mean(abs(e1), na.rm = TRUE)
mean(abs(e2), na.rm = TRUE)
mean(abs(e3), na.rm = TRUE)

fc <- holt(livestock, damped = TRUE)
fc[["model"]] ##Obtain parameters of the damped model

autoplot(fc) + 
    xlab("Year") + 
    ylab("Livestock, sheep in Asia (millions)")


# Holt-winters seasonal method --------------------------------------------
rm(list = ls())

aust <- window(austourists, start = 2005) ##Quarterly series, m = 4

autoplot(aust) + 
    labs(x = "Time", y = "Total nights (in millions)") ##Variation seems uniform

fit1 <- hw(aust, seasonal = "additive")
fit2 <- hw(aust, seasonal = "multiplicative")

autoplot(aust) + 
    autolayer(fit1, series = "HW additive forecasts", PI = FALSE) + 
    autolayer(fit2, series = "HW multiplicative forecasts", PI = FALSE) + 
    xlab("Year") + 
    ylab("Visitor night (millions)") + 
    ggtitle("International visitors nights in Australia") + 
    guides(color = guide_legend(title = "Forecast"))

##Let's check AIC: fit2 has a smaller AIC
summary(fit1)
summary(fit2)

fit_1f <- function(x, h){forecast(hw(x, seasonal = "additive",h = h))}
fit_2f <- function(x, h){forecast(hw(x, seasonal = "multiplicative",h = h))}

#Checking one-step ahead cross validation: fit2 has smaller MSE
mean(tsCV(y = aust, fit_1f, h = 1)**2, na.rm = TRUE) 
mean(tsCV(y = aust, fit_2f, h = 1)**2, na.rm = TRUE) 

#Analyze mulitplicative fit
summary(fit2)

##Holt-winters method with daily data: using damped meth
rm(list =ls())
fc <- hw(subset(hyndsight, end = length(hyndsight)-35),
         damped = TRUE, seasonal = "multiplicative", h = 35)

autoplot(hyndsight) + 
    autolayer(fc, series = "HW multip damped", PI = FALSE) + 
    guides(color = guide_legend(title = "Daily forecasts"))

##Check accuracy
predicted <- fc$mean 
real <- subset(hyndsight, start = 330)

accuracy(fc, real) ##Checkin accuracy measures

mean(abs(real - predicted)) ##MAE

# ETS Framework -----------------------------------------------------------
rm(list = ls())
aust <- window(austourists, start = 2005) ##M = 4
autoplot(aust)

fit <- ets(aust) ##Automatically fit ETS models
summary(fit) ##Summary of the model

#obtain one step training errors and model multiplicative errors.
cbind("Residuals" = residuals(fit),
      "Forecast errors" = residuals(fit, type = "response")) %>%
    autoplot(facet = TRUE) + 
    xlab("Year") + ylab("")

#obtain forecasts from an ETS model
fit %>% forecast(h = 8) %>%
    autoplot() + 
    ylab("International visitor night in Australia (millions)")