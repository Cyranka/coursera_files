remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(fpp2);library(tidyverse)


# Example 11.1 ------------------------------------------------------------
##Notice that calls ha two seasonal periods specified
p1 <- autoplot(calls) + 
    ylab("Call volume") + xlab("Weeks") + 
    scale_x_continuous(breaks = seq(1,33,by = 2))
p2 <- autoplot(window(calls, end = 4)) + 
    ylab("Call volume") + xlab("Weeks") + 
    scale_x_continuous(minor_breaks = seq(1,4,by = 0.2))
gridExtra::grid.arrange(p1,  p2)

# STL with multiple seasonal periods --------------------------------------
calls %>% mstl() %>%
    autoplot() + xlab("Week")

##This decomposition can be used to forecast:
## Each seasonal component will be forecasted naive: next will be same as past
## Trend is forecast using ETS

calls %>% stlf() %>%
    autoplot() + xlab("Week")

# Dynamic harmonic regression ---------------------------------------------
fit <- auto.arima(calls, seasonal = FALSE,lambda = 0,
                  xreg = fourier(calls, K = c(10,10))) ##Add fourier terms

fit %>% forecast(
    xreg = fourier(calls, K = c(10,10), h = 2.169)) %>%
    autoplot(include = 5*169) + 
    ylab("Call volume") + xlab("Weeks")

# Complex seasonality with covariates -------------------------------------
#This is half-hourly electricity demand
autoplot(elecdemand[,c("Demand","Temperature")], facet=TRUE) + 
    scale_x_continuous(minor_breaks=NULL, breaks=2014+ cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))/365, labels=month.abb) + xlab("Time") + ylab("")


elecdemand %>% as.data.frame() %>% ggplot(aes(x=Temperature, y=Demand)) + 
    geom_point() + xlab("Temperature (degrees Celsius)") + ylab("Demand (GW)")

#Fit a model
cooling <- pmax(elecdemand[,"Temperature"], 18)
fit <- auto.arima(elecdemand[,"Demand"],
                  xreg = cbind(fourier(elecdemand, c(10,10,0)),
                               heating=elecdemand[,"Temperature"],
                               cooling=cooling))

# Section 11.2: Vector autoregression ---------------------------------------------------
library(vars)

##Choose lag length using information criteria: BIC is preferred
VARselect(uschange[,1:2], lag.max=8, type="const")[["selection"]]

##Fit first model with only one lag
var1 <- VAR(uschange[,1:2], p=1, type="const")
var1
serial.test(var1, lags.pt=10, type="PT.asymptotic") ##Check residual autocorrelation

##Fit second model with two lags
var2 <- VAR(uschange[,1:2], p=2, type="const") 
serial.test(var2, lags.pt=10, type="PT.asymptotic")##Check residual autocorrelation

##Fit third model with three lags
var3 <- VAR(uschange[,1:2], p=3, type="const") 
serial.test(var3, lags.pt=10, type="PT.asymptotic") ##Check residual autocorrelation

#Plot model with uncorrelated residuals
forecast(var3) %>%
    autoplot() + 
    xlab("Year") + 
    ggtitle("VAR estimates of consumption and income")

# Example 11.3 ------------------------------------------------------------
autoplot(sunspotarea) ##Annual data

fit <- nnetar(sunspotarea, lambda = 0) ##Fit the neural network
autoplot(forecast(fit, h = 30))

fit

# Simulate future paths ---------------------------------------------------
sim <- ts(matrix(0,nrow = 30L,ncol = 9L),
          start = end(sunspotarea)[1L] + 1L) ##Matrix of possible futures

for(i in seq(9)){
    sim[,i] <- simulate(fit, nsim = 30)
}
remove(i)

autoplot(sunspotarea) + autolayer(sim)

fcast <- forecast(fit, PI = TRUE, h = 30)
autoplot(fcast)

# Bootstraping time-series ------------------------------------------------
bootseries <- bld.mbb.bootstrap(debitcards, 10) %>% 
    as.data.frame() %>% ts(start=2000, frequency=12) 

autoplot(debitcards) + 
    autolayer(bootseries, colour=TRUE) + 
    autolayer(debitcards, colour=FALSE) + 
    ylab("Bootstrapped series") + 
    guides(colour="none")



