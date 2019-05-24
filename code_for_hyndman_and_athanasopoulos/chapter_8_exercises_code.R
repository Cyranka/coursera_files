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