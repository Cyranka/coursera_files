remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_3/")
library(readxl)
library(gdata)
library(lmtest)
x <- read_excel("TrainExer 3-5.xlsx")

##Question (a)
full_model <- lm(LogEqPrem ~ BookMarket + NTIS + DivPrice + EarnPrice + Inflation, data = x)
print(summary(full_model), digits = 3) ##Multiple R Squared = 0.108

restricted_model <- lm(LogEqPrem ~ BookMarket, data = x)
print(summary(restricted_model), digits = 3) ##Multiple R Squared = 0.633

f_numerator <- (summary(full_model)$r.squared - summary(restricted_model)$r.squared)/4
f_denominator <- (1 - summary(full_model)$r.squared)/(nrow(x) - 6)
f_value = f_numerator/f_denominator

df(x = f_value, df1 = 4,nrow(x) - 6)

##Question (b): Performing a RESET Test
keep(x, full_model, restricted_model, sure = TRUE)

fitted_values_sq <- restricted_model$fitted.values**2
reset_regression <- lm(x$LogEqPrem ~ x$BookMarket + fitted_values_sq)
summary(reset_regression)

f_reset_numerator <- (summary(reset_regression)$r.squared - summary(restricted_model)$r.squared)/1
f_reset_denominator <- (1 - summary(reset_regression)$r.squared)/(nrow(x) - 2 - 1)

f_reset <- f_reset_numerator/f_reset_denominator
round(f_reset,3)
1 - round(pf(f_reset, df1 = 1,df2 = 84),3) ##Need to check this
resettest(x$LogEqPrem ~ x$BookMarket, power =2, type = "regressor") ##RESET Test Gives the Right Parameter

##(c)Perform a Chow Break Test
remove(f_reset, f_reset_denominator, f_reset_numerator, fitted_values_sq, reset_regression)

sum_unrestricted <- sum(residuals(restricted_model)**2)
before_break <- lm(LogEqPrem ~ BookMarket, data = subset(x, x$Year <1980))
after_break <- lm(LogEqPrem ~ BookMarket, data = subset(x, x$Year >=1980))

sum_res_before_break <- sum(residuals(before_break)**2)
sum_res_after_break <- sum(residuals(after_break)**2)
##2 Parameters: 1 Intercept and 1 Slope Coefficient: BooktoMarket Ratio
f_chow_numerator <- (sum_unrestricted - sum_res_before_break - sum_res_after_break)/2
f_chow_denominator <- (sum_res_after_break + sum_res_before_break)/(nrow(x) -4)

f_chow <- f_chow_numerator/f_chow_denominator

1 - round(pf(f_chow,2,83),2)##Probability, associated with f_chow


#(d) Perform a Chow Forecast Test
forecast_chow_numerator <- (sum_unrestricted - sum_res_before_break)/34
forecast_chow_denominator <- (sum_res_before_break)/(53 - 2)

f_forecast_chow <- round(forecast_chow_numerator/forecast_chow_denominator,3)

1 - round(pf(f_forecast_chow,34,51),3)