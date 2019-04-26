remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)


# Regression model --------------------------------------------------------
##Consumption and income plot
autoplot(uschange[,c("Consumption", "Income")]) + 
    ylab("% change") + xlab("Year")

##Scatterplot
uschange %>%
    as.data.frame() %>%
    ggplot(aes(x = Income, y = Consumption)) + 
    ylab("Consumption (quarterly % change)") + 
    xlab("Income (quarterly % change)") + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE)

##Fitted model
tslm(Consumption ~ Income, data = uschange)

# Multiple regression model -----------------------------------------------
#time-series
autoplot(uschange[,3:5], facets = TRUE,colour = TRUE,show.legend = FALSE) 

##GGpairs
uschange %>%
    as.data.frame() %>% 
    GGally::ggpairs()

##Fit model to the data.
fit_consMR <- tslm(
    Consumption ~ Income + Production + Unemployment + Savings,
    data = uschange
)

summary(fit_consMR)

##Plot actual series and fitted values
autoplot(uschange[,"Consumption"], series = "Data") + 
    autolayer(fitted(fit_consMR), series = "Fitted") + 
    xlab("Year") + ylab("") + 
    ggtitle("Percent change in US consumption expenditure")

cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit_consMR)) %>%
    as.data.frame() %>%
    ggplot(aes(x = Data, y = Fitted)) + 
    geom_point() + 
    xlab("Fitted (predicted values)") + 
    ylab("Data (actual values)") + 
    ggtitle("Percent change in US consumption expenditure") + 
    geom_abline(intercept = 0, slope = 1)


# Check residuals ---------------------------------------------------------
checkresiduals(fit_consMR)

# Residuals vs. Fitted values ---------------------------------------------
cbind(Fitted = fitted(fit_consMR),
      Residuals = residuals(fit_consMR)) %>%
    as_tibble() %>%
    ggplot(aes(x = Fitted, y = Residuals)) + 
    geom_point()

# Spurious regression example ---------------------------------------------
aussies <- window(ausair, end = 2011)
fit <- tslm(aussies ~ guinearice)
summary(fit) #R-squared equal to 0.9568 (95.7%)

checkresiduals(fit)

# Using dummy variables: example ------------------------------------------
beer2 <- window(ausbeer, start = 1992)
autoplot(beer2) + 
    xlab("Year") + 
    ylab("Megalitres")

fit_beer <- tslm(beer2~ trend + season) ##Trend and season are created automatically
summary(fit_beer)

autoplot(beer2, series = "Data") + 
    autolayer(fitted(fit_beer), series = "Fitted") + 
    xlab("Year") + ylab("Megalitres") + 
    ggtitle("Quarterly beer production")

# Using fourier series ----------------------------------------------------
fourier_beer <- tslm(beer2 ~ trend + fourier(beer2, K = 2))
summary(fourier_beer)

# Selecting predictors ----------------------------------------------------
CV(fit_consMR)

# Forecasts with regression models ----------------------------------------
remove(list = ls())
beer2 <- window(ausbeer, start = 1992)
fit_beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit_beer)
autoplot(fcast) + 
    ggtitle("Forecasts of beer production using regression") + 
    xlab("Year") + ylab("megalitres")

# Scenario-based forecasting ----------------------------------------------
fit_consBest <- tslm(
    Consumption ~ Income + Savings + Unemployment,
    data = uschange
)
h <- 4
newdata <- data.frame(
    Income = c(1,1,1,1),
    Savings = c(0.5,0.5,0.5,0.5),
    Unemployment = c(0,0,0,0)
) ##Create new data with scenario 1

fcast_up <- forecast(fit_consBest, newdata = newdata)

newdata2 <- data.frame(
    Income = rep(-1,h),
    Savings = rep(-0.5,h),
    Unemployment = c(0,0,0,0)
)
fcast_down <- forecast(fit_consBest, newdata = newdata2)
 
# Generating forecast intervals -------------------------------------------
fit_cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast_ave <- forecast(fit_cons,
                      newdata = data.frame(
                          Income = rep(mean(uschange[,"Income"],h))
                      ))

fcast_up <- forecast(fit_cons,
                     newdata = data.frame(Income = rep(5,h)))
autoplot(uschange[,"Consumption"]) + 
    ylab("% change in US consumption") + 
    autolayer(fcast_ave, series = "Average increase", PI = TRUE) + 
    autolayer(fcast_up, series = "Extreme increase", PI = TRUE)

# Non-linear forecasting: Boston marathon -------------------------------------------
rm(list = ls())

marathon %>%
    splinef(lambda = 0) %>%
    autoplot() ##fit natural splines o the data

marathon %>%
    splinef(lambda = 0) %>%
    checkresiduals() ##Check residuals