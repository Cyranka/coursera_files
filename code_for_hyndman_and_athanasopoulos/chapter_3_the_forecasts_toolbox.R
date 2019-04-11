remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)

# Average method ----------------------------------------------------------
meanf(dj,2) ##First is the series, second is the forecast horizon

# Naive method ------------------------------------------------------------
rwf(dj,10)

# Seasonal naive ----------------------------------------------------------
snaive(a10,2)

# Drift method ------------------------------------------------------------
rwf(a10,2,drift = TRUE)

# Simple forecasting techniques applied to ausbeer ------------------------
beer2 <- window(ausbeer, start = 1992, end = c(2007,4))

autoplot(beer2) + 
    autolayer(meanf(beer2, h = 11),
              series = "Mean", PI = FALSE) + 
    autolayer(naive(beer2, h = 11),
              series = "Naive", PI = FALSE) + 
    autolayer(snaive(beer2, h = 11),
          series = "Seasonal naive", PI = FALSE) + 
    ggtitle("Forecasts for quarterly beer production") + 
    xlab("Year") + ylab("Megalitres") + 
    guides(color = guide_legend(title = "Forecasts"))

# Simple forecasting applied to Google stock price ------------------------
autoplot(goog200) + 
    autolayer(meanf(goog200, h = 40),
              series = "Mean", PI = FALSE) + 
    autolayer(rwf(goog200, h = 40),
              series = "Naive", PI = FALSE) + 
    autolayer(rwf(goog200, h = 40, drift = TRUE),
              series = "Drift", PI = FALSE) + 
    ggtitle("Google stock (daily ending 6 Dec 2013)") + 
    xlab("Day") + ylab("Closing price (US$)") + 
    guides(color = guide_legend(title = "Forecasts"))

# Calendar adjustment -----------------------------------------------------
dframe <- cbind(
    Monthly = milk,
    DailyAverage = milk/monthdays(milk)
) ##Obtain average milk produced by day: Total milk produced/total days

autoplot(dframe, facet = TRUE)+
    xlab("Years") + ylab("Pounds") + 
    ggtitle("Milk produced per cow")

# Box-Cox transformation --------------------------------------------------
lambda_example <- BoxCox.lambda(elec)

dframe <- cbind(
    Regular = elec,
    Transformed_elec = BoxCox(elec, lambda_example)
)


autoplot(dframe, facet = TRUE)+
    xlab("Time") + ylab("Total") + 
    ggtitle("Monthly electricity production: regular series and Box-Cox transformed")

# Bias-adjusted forecasts -------------------------------------------------
fc <- rwf(eggs, drift = TRUE, lambda = 0, h = 50, level = 80)
fc2 <- rwf(eggs, drift = TRUE, lambda = 0, h = 50, level = 80,
           biasadj = TRUE) ##This will return the mean of the distribution

autoplot(eggs) + 
    autolayer(fc, series = "Simple back transformation") + 
    autolayer(fc2, series = "Bias adjusted", PI = FALSE) + 
    guides(color = guide_legend(title = "Forecast"))

