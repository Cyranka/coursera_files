remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal);library(urca)

# Section 9.2: Regression with ARIMA errors -------------------------------
#Plot the data
autoplot(uschange[,1:2], facets = TRUE) + 
    xlab("Year") + ylab("") + 
    ggtitle("Quarterly changes in US consumption and personal income")

#Fit a dynamic regression model
fit <- auto.arima(uschange[,"Consumption"],
                  xreg = uschange[,"Income"]) ##This will pick an ARIMA model for the errors

summary(fit)

#Check residuals
cbind("Regression errors" = residuals(fit, type = "regression"),
      "ARIMA errors" = residuals(fit, type = "innovation")) %>%
    autoplot(facets = TRUE)

checkresiduals(fit)

#Forecast 
fcast <- forecast(fit, xreg = rep(mean(uschange[,2]),8))
autoplot(fcast) + xlab("Year") + 
    ylab("Percentage change")

#Predicting electricity demand
rm(list = ls());dev.off()

xreg <- cbind(MaxTemp = elecdaily[,"Temperature"],
              MaxTempSq = elecdaily[,"Temperature"]**2,
              Workday = elecdaily[,"WorkDay"])

fit <- auto.arima(elecdaily[,"Demand"], xreg = xreg)
checkresiduals(fit)

#Calculate forecasts assuming future values for temperature
fcast <- forecast(fit,
                  xreg = cbind(
                      xreg.MaxTemp = rep(26,14),
                      xreg.MaxTempSq = rep(26**2,14),
                      xreg.Workday = c(0,1,0,0,1,1,1,1,1,0,0,1,1,1))
                  )

autoplot(fcast) + ylab("Electricity Demand (GW)")

# Section 9.4: Stochastic and deterministic trends ------------------------
rm(list = ls());dev.off()
trend <- seq_along(austa) ##A trend is just a number
fit1 <- auto.arima(austa, d = 0, xreg = trend) ##Deterministic trend
summary(fit1)

fit2 <- auto.arima(austa, d = 1)
fit2 ##Drift parameter is the stochastic trend

fc1 <- forecast(fit1, xreg = cbind(trend= length(austa) + 1:10))
fc2 <- forecast(fit2, h = 10)

autoplot(austa) + 
    autolayer(fc2, series = "Stochastic trend") + 
    autolayer(fc1, series = "Deterministic trend") + 
    ggtitle("Forecasts from trend models") + 
    xlab("Year") + 
    ylab("Visitors to Australia (millions)") + guides(color = guide_legend(title = "Forecast"))

# Section 9.5: Dyanmic harmonic regression --------------------------------
rm(list = ls());dev.off()

cafe04 <- window(auscafe, start = 2004)
plots <- list()

for(i in seq(6)){
    fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i),
                      seasonal = FALSE, lambda = 0)
    plots[[i]] <- autoplot(forecast(fit,
                                    xreg = fourier(cafe04, K=i, h = 24))) + 
        xlab(paste("K=",i, " AICC=", round(fit[["aicc"]],2))) + 
        ylab("") + ylim(1.5,4.7)
}
gridExtra::grid.arrange(
    plots[[1]],plots[[2]],plots[[3]],
    plots[[4]],plots[[5]], plots[[6]], nrow = 2
)

# Section 9.6: Lagged predictors ------------------------
rm(list = ls());dev.off()

autoplot(insurance, facets = TRUE) + 
    xlab("Year") + ylab("") + 
    ggtitle("Insurance advertising and quotations")


Advert <- cbind(
    AdLag0 = insurance[,"TV.advert"],
    AdLag1 = stats::lag(insurance[,"TV.advert"], -1),
    AdLag2 = stats::lag(insurance[,"TV.advert"], -2),
    AdLag2 = stats::lag(insurance[,"TV.advert"], -3)
) %>% head(NROW(insurance))

#Restrict data so models have similar fitting periods
fit1 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1],stationary = TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1:2],stationary = TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1:3],stationary = TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1:4],stationary = TRUE)

c(fit1[["aicc"]],
  fit2[["aicc"]],
  fit3[["aicc"]],
  fit4[["aicc"]]) ##Two lagged predictors has the smallest AICc

final_fit <- auto.arima(insurance[,1], xreg = Advert[,1:2],
                        stationary = TRUE)
summary(final_fit)