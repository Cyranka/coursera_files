remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal);library(urca)

# Question 2: ACF and PACF ------------------------------------------------
frequency(ibmclose) #Dai
autoplot(ibmclose) + 
    theme_minimal() + 
    labs(x = "Time", y = "Closing price") ##Plot the data

ggAcf(ibmclose,main = "ACF for IBM closing price") ##regular data
ggPacf(ibmclose,main = "PACF for IBM closing price") ##regular data

ggAcf(diff(ibmclose, lag = 1),main = "ACF for IBM differenced price")
ggPacf(diff(ibmclose, lag = 1),main = "PACF for IBM differenced price")

autoplot(diff(ibmclose, lag = 1)) + 
    theme_minimal() + 
    labs(x = "Time", y = "Differenced price")

# Question 3 --------------------------------------------------------------
series <- list(usnetelec,usgdp,mcopper,enplanements,visitors)
obtain_bc_lambda <- sapply(series,function(i)BoxCox.lambda(i)) ##Stabilize variance
obtain_ndiff <- sapply(series,function(i)ndiffs(i))


# Question 4 --------------------------------------------------------------
frequency(enplanements)


# Question 5 --------------------------------------------------------------
remove(list = ls())

retaildata <- readxl::read_excel("retail.xlsx", skip = 1)
my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

autoplot(my_retail_series)

##Stabilize variance
my_retail_series %>% stl(s.window = "periodic") %>%
    autoplot() ##Doesn't seem necessary, but let's do it

box_cox_lambda <- BoxCox.lambda(my_retail_series)
box_cox_retail <- BoxCox(my_retail_series, box_cox_lambda)

ndiffs(box_cox_retail)

# Question 6 --------------------------------------------------------------
remove(list =ls())
set.seed(1)
y <- ts(numeric(100)) ##Length
e <- rnorm(100) ##Errors
for(i in 2:100){
    y[i] <- 0.95*y[i-1] + e[i]
}
remove(i)

autoplot(y)

##Generate an MA(1) model
set.seed(1)
y <- ts(numeric(100)) ##Length
e <- rnorm(100)

for(i in 2:100){
    y[i] <- e[i] + 0.1*e[i-1]
}

autoplot(y)

##Generate an ARMA(1,1) model
set.seed(1)
y <- ts(numeric(100)) ##Length
e <- rnorm(100)


for(i in 2:100){
    y[i] <- 0.6*y[i-1] + 0.6*e[i-1] + e[i]
}
remove(i)
autoplot(y)

##generate an ARMA(2,0)
set.seed(1)
y2 <- ts(numeric(100)) ##Length
e <- rnorm(100)

for(i in 3:100){
    y2[i] <- -0.8*y2[i-1] + 0.3*y2[i-1] + e[i]
}

autoplot(y2)

# Question 7 --------------------------------------------------------------
rm(list = ls());dev.off()

#a) Study the series
autoplot(wmurders) ##Yearly data
ndiffs(wmurders) ##How many differences we need? 2

autoplot(diff(diff(wmurders))) ##Twice differenced data seems to be mean stationary:Variance is not great though

#ACF
ggAcf(diff(diff(wmurders))) ## et_2 seems to be significant: MA(2)
ggPacf(diff(wmurders)) ##AR (2)

#d) Fit an ARIMA(2,2,2) model
fit_1 <- Arima(wmurders, order = c(2,2,2))
checkresiduals(fit_1)

#e) Forecast h = 3
forecasts_arima222 <- forecast(fit_1, h = 3)
summary(fit_1)

forecasts_arima222

autoplot(forecast(fit_1, h = 3))

#f) Fit auto.arima
(auto_fit <- auto.arima(wmurders, stepwise = FALSE, approximation = FALSE))

# Question 9: USGDP -------------------------------------------------------
rm(list = ls())
autoplot(usgdp) + theme_minimal() ##There is definitely a trend
gdp_lambda <- BoxCox.lambda(usgdp)

#Use auto.arima to find a model
auto_arima_gdp <- auto.arima(usgdp, lambda = "auto",approximation = FALSE,stepwise = FALSE)
summary(auto_arima_gdp)

#Use ARIMA: Transformation comes before differencing: stabilize then difference
autoplot(diff(diff(BoxCox(usgdp,gdp_lambda)))) ##Twice differenced series
ggAcf(diff(diff(BoxCox(usgdp,gdp_lambda)))) ##ACF suggests an MA(1), the others are barely significant
ggPacf(diff(diff(BoxCox(usgdp,gdp_lambda)))) ##PACF suggests, perhaps an AR(3)

self_arima <- Arima(usgdp, lambda = gdp_lambda,order = c(3,2,1))

##Check models
autoplot(usgdp, alpha =0.4, size =3) + 
    autolayer(fitted(auto_arima_gdp))

autoplot(usgdp, alpha =0.4, size =3) + 
             autolayer(fitted(self_arima))

fit_1 <- function(y,h){
    forecast(Arima(y,order = c(0,1,2),include.drift = TRUE,lambda = gdp_lambda),h=h)
    }
auto_arima_tscv <- tsCV(y = usgdp,forecastfunction = fit_1, h = 1)

fit_2 <- function(y,h){
    forecast(Arima(y,order = c(3,2,1),lambda = gdp_lambda),h=h)
}
self_arima_tscv <- tsCV(y = usgdp,forecastfunction = fit_2, h = 1)

mean(auto_arima_tscv**2, na.rm = TRUE)
mean(self_arima_tscv**2, na.rm = TRUE)

#e) 
checkresiduals(auto_arima_gdp)

#f) Forecasts
autoplot(forecast(auto_arima_gdp, h = 12)) ##Next three years
arima_forecasts <- forecast(auto_arima_gdp, h = 12)

#Fit an ETS
ets_forecasts <- forecast(ets(usgdp), h = 12)
autoplot(ets_forecasts)

# Question 10: austourists ------------------------------------------------
rm(list = ls());dev.off()

#a) Plot the data
autoplot(austourists)
austourists %>% stl(s.window = "periodic") %>%
    autoplot()

#b) Describe the ACF
ggtsdisplay(austourists)

#c) Describe the PACF
ggtsdisplay(austourists)

#d) Produce seasonal differenced data, but first transform the data
obtain_lambda <- BoxCox.lambda(austourists)
transformed_data <- BoxCox(austourists, lambda = obtain_lambda)
season_diff <- diff(transformed_data, lag = 4)
ggtsdisplay(season_diff)
season_diff %>% urca::ur.kpss()

#e) Use auto.arima
fit_auto <- auto.arima(austourists, lambda = obtain_lambda)
summary(fit_auto)

#e.1) Use TSCV to fit the model
my_model <- function(x, h){forecast(Arima(x,lambda = obtain_lambda,order = c(1,0,1),
                                 seasonal = c(1,1,1)),h =h)}

auto_model <- function(x, h){forecast(Arima(x,lambda = obtain_lambda,order = c(1,0,0),
                                   seasonal = c(0,1,1), include.drift = TRUE), h=h)}


e_1 <- tsCV(austourists, my_model, 1)
e_2 <- tsCV(austourists, auto_model, 1)

mean(e_1**2, na.rm = TRUE)
mean(e_2**2, na.rm = TRUE)

autoplot(austourists, alpha =0.5) + 
    autolayer(fitted(fit_auto))

autoplot(forecast(fit_auto, h = 8))


# Question 11 -------------------------------------------------------------
rm(list = ls());dev.off()

autoplot(usmelec) ##Monthly data

#a) 12 Moving average
twelve_ma <- stats::filter(usmelec, filter = rep(1/12,12),sides = 2)
autoplot(twelve_ma)

usmelec %>% stl(s.window = "periodic") %>%
    autoplot()

#b) Find suitable lambda
usmelec_lambda <- BoxCox.lambda(usmelec)

#c) Take seasonal differencing
diff(BoxCox(usmelec, usmelec_lambda), lag = 4) %>%ndiffs()

#d) Look at ACF and PACF to find suitable ARIMA models
ggtsdisplay(diff(BoxCox(usmelec,usmelec_lambda), lag = 4))

#e) Use Auto-Arima to find suitable model
fit_auto <- auto.arima(usmelec, lambda = usmelec_lambda)
my_fit <- Arima(usmelec, lambda = usmelec_lambda,
                order = c(2,0,1), seasonal = c(0,1,3))
summary(my_fit)
summary(fit_auto)

#e) Check residuals
checkresiduals(fit_auto)

#f) Forecast the next fifteen years
next_15 <- forecast(fit_auto, h = 180)
autoplot(next_15)

#f1) Compare to current values
actual <- readxl::read_excel("Table_7.1_Electricity_Overview.xlsx", sheet = 1) %>%
    pull(total) %>% ts(start = c(1973,01),frequency = 12)

target_values <- window(actual, start = c(2013,7))

accuracy(forecast(fit_auto,h = 67),target_values)

# Question 12 -------------------------------------------------------------
rm(list = ls());dev.off()

#a) Plot the data
autoplot(mcopper)
frequency(mcopper) ##Monthly data

lambda_mcopper <- BoxCox.lambda(mcopper)

#b) Find Arima model automatically
fit_auto <- auto.arima(mcopper, lambda = lambda_mcopper)
summary(fit_auto)

#c) Try other models: Seasonal differencing is not necessary
diff(mcopper) %>% ur.kpss() %>% summary() ## A single difference suffices

data_transform <- diff(BoxCox(mcopper, lambda_mcopper))
ggtsdisplay(data_transform)

fit_model1 <- Arima(mcopper,
                    lambda = lambda_mcopper,
                    order = c(1,1,1))

fit_model2 <- Arima(mcopper,
                    lambda = lambda_mcopper,
                    order = c(2,1,1))

#Let's use ts cv
function_fit1 <- function(x, h){forecast(Arima(x,
                                               lambda = lambda_mcopper,
                                               order = c(1,1,1)),h=h)}

function_fit2 <- function(x, h){forecast(Arima(x,
                                               lambda = lambda_mcopper,
                                               order = c(2,1,1)),h=h)}

function_fit_auto <- function(x, h){forecast(Arima(x,
                                                   lambda = lambda_mcopper,
                                                   order = c(0,1,1)),h = h)}

e_1 <- tsCV(mcopper, function_fit1, h = 1)
e_2 <- tsCV(mcopper, function_fit2, h = 1)
e_3 <- tsCV(mcopper, function_fit_auto, h = 1)

sqrt(mean(e_1**2, na.rm = TRUE))
sqrt(mean(e_2**2, na.rm = TRUE))
sqrt(mean(e_3**2, na.rm = TRUE))

#Producing forecasts
autoplot(forecast(fit_auto, h = 12))

#Fitting an ets
ets_fit <- ets(mcopper)
summary(ets_fit)

forecast(ets_fit, h = 12) ##
autoplot(forecast(ets_fit, h = 12))

# Question 13 -------------------------------------------------------------
rm(list = ls());dev.off()
#Plot the data
autoplot(auscafe)

#a) Find lambda
auscafe_lambda <- BoxCox.lambda(auscafe)

#b)Find number of differencing necessary
nsdiffs(BoxCox(auscafe, auscafe_lambda)) %>% ndiffs()
diff(BoxCox(auscafe, auscafe_lambda),12) %>%
    diff() %>%
    urca::ur.kpss() %>% summary()

#c) Diagnose ACF and PACF plots
diff(BoxCox(auscafe, auscafe_lambda),12) %>%
    diff() %>%ggtsdisplay()

fit_auto <- auto.arima(auscafe, lambda = auscafe_lambda)
fit_exhaustive <- auto.arima(auscafe, lambda = auscafe_lambda,
                             approximation = FALSE, stepwise = FALSE)
summary(fit_auto)
summary(fit_exhaustive)
#d) Check residuals
checkresiduals(fit_exhaustive)

#e) Forecast
auto_forecast <- forecast(fit_exhaustive, h = 24)
auto_forecast
autoplot(auto_forecast)

#f) ETS
ets_model <- ets(auscafe,lambda = auscafe_lambda)
autoplot(forecast(ets_model, h = 24))

ets_tscv <- function(x,h){forecast(ets(x,lambda = auscafe_lambda),h = 1)}
arima_tscv <- function(x,h){forecast(Arima(x,
                                           lambda = auscafe_lambda,
                                           order = c(3,0,1),
                                           seasonal = c(0,1,1),
                                           include.drift = TRUE),h = 1)}

j_1 <- tsCV(auscafe, ets_tscv)
j_2 <- tsCV(auscafe, arima_tscv)

sqrt(mean(j_1**2, na.rm = TRUE))
sqrt(mean(j_2**2, na.rm = TRUE))

# Question 14 -------------------------------------------------------------
#Use a non-seasonal method by removing seasonality

#Let's split the data into test and train
auscafe_train <- window(auscafe, end = c(2015,9))
auscafe_test <- window(auscafe, start = c(2015,10))

##Forecast using arima
arima_q14 <- auto.arima(auscafe_train, lambda = "auto")
arima_forecats <- forecast(arima_q14, h = 24)

##Forecast using stlf
forecasts_stlf <- auscafe_train %>% stlf(lambda = "auto",method = "arima", h = 24)

##Check accuracy
accuracy(arima_forecats, auscafe_test)[,c("RMSE","MAE","MAPE")] ##For ARIMA model
accuracy(forecasts_stlf, auscafe_test)[,c("RMSE","MAE","MAPE")] ##For ARIMA on data without seasonality

# Question 15 -------------------------------------------------------------
rm(list = ls());dev.off()

retaildata <- readxl::read_excel("retail.xlsx", skip = 1)
my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

autoplot(my_retail_series)

#Transform the data to stabilize variance
retail_bc <- BoxCox.lambda(my_retail_series)

#Check number of differences: 
x <- my_retail_series %>%
    BoxCox(lambda = retail_bc) %>%
    diff(lag = 12) %>% diff(lag = 1)

ggtsdisplay(x)

##Use Auto.ARIMA
auto_arima <- auto.arima(my_retail_series, lambda = retail_bc)
auto_arima

#Check real values
real <- readxl::read_excel("current_retail_data.xlsx", sheet = 2) %>%
        pull(A3349335T) %>%
        ts(frequency = 12,start = c(1982,4)) %>%
        window(start = c(2014,1))

##Forecast values
arima_forecasts <- forecast(auto_arima, h = 34)

##Accuracy
accuracy(arima_forecasts, real)

##Plot the data
full_real <- readxl::read_excel("current_retail_data.xlsx", sheet = 2) %>%
    pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

autoplot(full_real, alpha = 0.5) + 
    autolayer(arima_forecasts, PI = FALSE) + theme_minimal()

# Question 16 -------------------------------------------------------------
rm(list =ls());dev.off()

#Plot the data
autoplot(sheep) + 
    theme_minimal() + 
    labs(y = "Sheep population")

#Take differences and analyze 
sheep %>% diff(lag = 1) %>%
    urca::ur.kpss() %>% summary()

#
sheep %>% diff(lag = 1) %>%
    ggtsdisplay()

##Auto-arima solution
arima_sheep <- auto.arima(sheep)
summary(arima_sheep)

##Hand calculation - might be wrong
1797 + 0.4210*(1797 - 1791) - 0.2018*(1791 - 1627) - 0.3044*(1627 - 1665)
1777.998 + 0.4210*(1778.12 - 1797) - 0.2018*(1797 - 1791) - 0.3044*(1791 - 1627)
1719.79 + 0.42*(1719.79 - 1778.12) - 0.2*(1778.12 - 1797) - 0.3*(1797 - 1791)

##Using forecast
forecast(arima_sheep, h = 3)

# Question 17 -------------------------------------------------------------
rm(list =ls());dev.off()

#Plot the data
autoplot(bicoal)

#Obtain PACF and ACF
urca::ur.kpss(bicoal) %>% summary()

bicoal %>% ggtsdisplay()

#Manual forecasts
162 + 0.83*545 -0.34*552 + 0.55*534 -0.38*512
162 + 0.83*525.81 -0.34*545 + 0.55*552 -0.38*534
162 + 0.83*513.8023 -0.34*525.81 + 0.55*545 -0.38*552

#fit model
fit_auto <- auto.arima(bicoal, approximation = FALSE, stepwise = FALSE)
f1 <- forecast(fit_auto, h = 3)
autoplot(f1)

##Recalculate forecasts
162 + 0.8334*545 -0.3443*552 + 0.5525*534 -0.3780*512
162 + 0.83*525.81 -0.34*545 + 0.55*552 -0.38*534
162 + 0.83*513.8023 -0.34*525.81 + 0.55*545 -0.38*552