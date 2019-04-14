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

# Forecasting examples: GOOG ----------------------------------------------------
#plot the series
autoplot(goog200) + 
    xlab("Day") + 
    ylab("Closing price (US$)") + 
    ggtitle("Google stock (daily ending 6 december 2013)")

#Obtain residuals from naive forecasting
res <- residuals(naive(goog200))
autoplot(res) + 
    xlab("Day") + 
    ylab("Residual") + 
    ggtitle("Residuals from naive forecasting on GOOG series") #Plot the residuals


#Histogram of residuals
res %>%
    as_tibble() %>%
    filter(!is.na(x)) %>%
    ggplot(aes(x = x)) + geom_histogram(fill = "white", color= "black") + 
    labs(title = "Histogram of residuals from naive forecasting on GOOG series")

#ACF of residuals
ggAcf(res) + ggtitle("AFC of residuals")

#Box-pierce test for residual autocorrelation
Box.test(res, lag = 10,fitdf = 0) ##Fitdf is number of parameters, lag is maximum lag

#Ljung-box test for residual autocorrelation
Box.test(res, lag = 10,fitdf = 0,
         type = "Lj") ##Fitdf is number of parameters, lag is maximum lag

checkresiduals(naive(goog200))

# Functions to subset a time-series ---------------------------------------
window(ausbeer, start = 1995) ##Start series in 1995
subset(ausbeer, start = length(ausbeer)-4*5) #Extracts last five years
subset(ausbeer, quarter = 1) ##Extracts 1st quarter

# Examples of MASE --------------------------------------------------------


# MASE 1: Australian beer production --------------------------------------
beer2 <- window(ausbeer, start = 1992, end = c(2007,4)) ##Training data
beerfit1 <- meanf(beer2, h = 10) ##Mean forecast, 10 future values
beerfit2 <- rwf(beer2, h = 10) ##ARIMA (0,1,0): Last value forecast
beerfit3 <- snaive(beer2, h = 10) ##SARIMA (0,0,0)(0,1,0)m: Seasonal last value forecast

autoplot(window(ausbeer, start = 1992)) + 
    autolayer(beerfit1, series = "Mean", PI = FALSE) + 
    autolayer(beerfit2, series = "Naive", PI = FALSE) + 
    autolayer(beerfit3, series = "Seasonal naive", PI = FALSE) + 
    xlab("Year") + ylab("Megalitres") + 
    ggtitle("Forecasts for quarterly beer production") + 
    guides(color = guide_legend(titles = "Forecast"))

beer3 <- window(ausbeer, start = 2008)
accuracy(beerfit1, beer3) %>% broom::tidy() %>% 
    select(`.rownames`, RMSE, MAE, MAPE, MASE)
accuracy(beerfit2, beer3) %>% broom::tidy() %>% 
    select(`.rownames`, RMSE, MAE, MAPE, MASE)
accuracy(beerfit3, beer3) %>% broom::tidy() %>% 
    select(`.rownames`, RMSE, MAE, MAPE, MASE)

# MASE 2: GOOG Stock prices --------------------------------------
googfc1 <- meanf(goog200, h = 40)
googfc2 <- rwf(goog200, h = 40)
googfc3 <- rwf(goog200, drift = TRUE, h = 40)

autoplot(subset(goog, end = 240)) + 
    autolayer(googfc1, PI = FALSE, series = "Mean") + 
    autolayer(googfc2, PI = FALSE, series = "Naive") + 
    autolayer(googfc3, PI = FALSE, series = "Drift") + 
    xlab("Day") + 
    ylab("Closing price (US$)") + 
    ggtitle("Google stock price (daily ending 6 Dec 2013)") + 
    guides(color = guide_legend(title = "Forecast"))

googtest <- window(goog, start = 201, end = 240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

# Using cross-validation --------------------------------------------------
e <- tsCV(goog200, rwf , drift = TRUE, h = 1)
sqrt(mean(e**2, na.rm = TRUE)) ##RMSE of Cross-validated 

e <- goog200 %>%
    tsCV(forecastfunction = rwf, drift = TRUE, h = 1)
e**2 %>% mean(na.rm = TRUE) %>% sqrt() ##RMSE of cross-validated errors
res <- goog200 %>% rwf(drift = TRUE) %>%
    residuals()

res**2 %>% mean(na.rm = TRUE) %>% sqrt() ##RMSE of errors

# Different forecasting horizons: goog ------------------------------------
e <- tsCV(goog200, forecastfunction = naive, h = 8)
mse <- colMeans(e**2, na.rm = TRUE)

tibble(h = 1:8, MSE = mse) %>% 
    ggplot(aes(x = h, y = MSE)) + 
    geom_point(color = "black")

# Forecasting with prediction intervals -----------------------------------
naive(goog200) ##Producing forecasting intervals
autoplot(naive(goog200))

naive(goog200, bootstrap = TRUE) ##Bootstrapped prediction intervals


