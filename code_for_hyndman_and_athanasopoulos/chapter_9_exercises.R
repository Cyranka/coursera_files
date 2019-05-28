remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal);library(urca)

# Question 1 --------------------------------------------------------------
#Plot the data
autoplot(advert, facets = TRUE)

#Use tslm to create a time-series regression models with normal errors
fit_tslm <- tslm(sales~advert, data = advert) #Fit the model

#Plot the two series
autoplot(advert[,"sales"]) + 
    autolayer(fitted(fit_tslm), series = "Fitted values")

#display summary and check residuals
summary(fit_tslm)
checkresiduals(fit_tslm)

#fit an Arima model with errors c(0,0,0)
fit_arima <- Arima(advert[,"sales"], xreg=advert[,"advert"],
                   order=c(0,0,0))

summary(fit_arima)

#Fit an auto.arima
auto_fit <- auto.arima(advert[,"sales"],xreg = advert[,"advert"])
summary(auto_fit)

autoplot(advert[,"sales"]) + 
    autolayer(fitted(auto_fit), series = "Fitted values")
checkresiduals(auto_fit)

#Forecast next 6 months
forecast_dynamic <- forecast(auto_fit,
         xreg = cbind(
             xreg = rep(10,6)
))

autoplot(forecast_dynamic)
forecast_dynamic

# Question 2: Lake Huron Dataset ------------------------------------------
rm(list =ls());dev.off()
autoplot(huron) ##plot the data

t <- time(huron)
t_knot <- 1920
t_pw <- ts(pmax(0, t - t_knot), start = t[1])
huron_xreg <- cbind(t = t, t_pw = t_pw)

huron_dreg_auto <- auto.arima(
    huron, xreg = huron_xreg
)

summary(huron_dreg_auto)

h <- 30
t_new <- t[length(t)] + seq(h) ##New Trend
t_pw_new <- t_pw[length(t_pw)] + seq(h) ##New piecewise
newdata <- cbind(t = t_new, t_pw = t_pw_new)

forecasts <- forecast(huron_dreg_auto,xreg = newdata)
autoplot(forecasts)

# Question 3: Motel Dataset -----------------------------------------------
remove(list = ls());dev.off()

#a) Calculate mean price per month
avg_cost <- ts(motel[,"Takings"]/motel[,"Roomnights"], start = c(1980,1),frequency = 12)

#b)Calculate CPI
#Included in the data

#c)Plots of both variables
autoplot(cbind(avg_cost = avg_cost,
               CPI = motel[,"CPI"]), facets = TRUE)

#d) Fit an arima model: Turn lambda = 0 to perform logarithm transformation
fit_arima <- auto.arima(avg_cost, xreg = cbind(CPI = motel[,"CPI"]),lambda = 0)
summary(fit_arima)
autoplot(fitted(fit_arima)) +
    autolayer(avg_cost, series = "Average cost")

#e.1)Forecast CPI For the next 12 months using ets
ets_model <- ets(motel[,"CPI"], lambda = 0)
cpi_forecasts <- forecast(ets_model, h = 12)
autoplot(cpi_forecasts)
point_cpi <- cpi_forecasts$mean

#e.2) Use CPU forecasts to predict room takings
autoplot(forecast(fit_arima, xreg = cbind(CPI = point_cpi)))

# Question 4: Gasoline series ---------------------------------------------
remove(list = ls());dev.off()
autoplot(gasoline, xlab = "Year")

##Decompose the data
stl_decomposition <- stl(gasoline, s.window = "periodic")

trendcycle(stl_decomposition)%>%
    autoplot()

trendcycle(stl_decomposition)

##Set nodes
t <- time(gasoline)
tbreak_1 <- 2007.582
tbreak_2 <- 2012.0
tbreak_3 <- 2016.0

tb1 <- ts(pmax(0, t - tbreak_1), start = 1991.1, frequency = 52.17857)
tb2 <- ts(pmax(0, t - tbreak_2), start = 1991.1, frequency = 52.17857)
tb3 <- ts(pmax(0, t - tbreak_3), start = 1991.1, frequency = 52.17857)

# Fit model ---------------------------------------------------------------
aic_values<- vector(mode = "numeric", length = 26)

#Fit models and get AICc: MUST BE A FOR LOOP
for(num in 1:26){
    aic_values[[num]] <- CV(
        tslm(
            gasoline ~ t + tb1 + tb2 + tb3 + fourier(gasoline, K = num)
        )
    )[["AICc"]]
}
remove(num)

which(aic_values == min(aic_values)) ##12 is best

k_12_model <- tslm(gasoline ~ t + tb1 + tb2 + tb3 + fourier(gasoline, K = 12))
autoplot(fitted(k_12_model)) + 
    autolayer(gasoline, alpha = 0.5)

#Fit auto.arima model
# arima_errors_model <- auto.arima(
#     gasoline, xreg = cbind(
#         t = t,
#         tb1 = tb1,
#         tb2 = tb2,
#         tb3 = tb3,
#         fourier(gasoline, K = 12)
#     ),
#     max.p = 3,max.d = 3,max.q = 3,
#     max.P = 3, max.D = 3, max.Q = 3
# )

# Question 6 --------------------------------------------------------------
remove(list = ls());dev.off()
retaildata <- readxl::read_excel("retail.xlsx", skip = 1)
my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

autoplot(my_retail_series)
box_cox_lambda <- BoxCox.lambda(my_retail_series)
t <- time(my_retail_series) #Trend variable

#loop auto-arima
auto_arima_models <- list()
for(i in 1:6){
    print(i)
    auto_arima_models[[i]] <- auto.arima(my_retail_series,
                         xreg = cbind(
                             trend = t,
                             fourier(BoxCox(my_retail_series, box_cox_lambda),K = i)),
                         lambda = box_cox_lambda)

}

aicc <- sapply(1:6, function(i)auto_arima_models[[i]]$aicc)

#Best model
best_model <- auto_arima_models[[6]]
autoplot(fitted(best_model)) + 
    autolayer(my_retail_series, alpha = 0.5)

#Check residuals
checkresiduals(best_model)

##Auto-Arima
auto_arima <- auto.arima(my_retail_series, lambda = box_cox_lambda)

##Check best model through cross validation
