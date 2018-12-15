remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_6/")
library(readxl)
library(dynlm)
library(gdata)
library(ecm)
x <- read_excel("TrainExer64.xlsx")

##Plot Data
plot(x$YEAR, x$RPK1, col = "red",type = "l",pch = 19, main = "Time Series RPK", xlab = "Year",ylab = "RPK", ylim = c(10,30))
lines(x$YEAR, x$RPK2, col = "blue", type = "l")

#Creating the lagged variables
x$lag1_x1 <- c(0,x$DX1[1:nrow(x)-1])
x$lag2_x1 <- c(0,x$lag1_x1[1:nrow(x)-1])

x$lag1_x2 <- c(0,x$DX2[1:nrow(x)-1])
x$lag2_x2 <- c(0,x$lag1_x2[1:nrow(x)-1])


#Granger Causality of X1
unrestricted_x1 <- lm(DX1 ~ lag1_x1 + lag2_x1 + lag1_x2 + lag2_x2, data = x)
restricted_x1 <- lm(DX1 ~ lag1_x1 + lag2_x1, data = x)

numerator1 <- (summary(unrestricted_x1)$r.squared - summary(restricted_x1)$r.squared)/2
denominator1 <- (1 - summary(unrestricted_x1)$r.squared)/(nrow(x) - 5)
fValue_x1 <- numerator1/denominator1

round(1 - pf(fValue_x1, 2,36),2) ##

#Granger Causality of X2
unrestricted_x2 <- lm(DX2 ~ lag1_x1 + lag2_x1 + lag1_x2 + lag2_x2, data = x)
restricted_x2 <- lm(DX1 ~ lag1_x2 + lag2_x2, data = x)

numerator2 <- (summary(unrestricted_x2)$r.squared - summary(restricted_x2)$r.squared)/2
denominator2 <- (1 - summary(unrestricted_x2)$r.squared)/(nrow(x) - 5)

fValue_x2 <- numerator2/denominator2
round(1 - pf(fValue_x2, 2, 36))

##Question (b): Dickey Fuller Test
keep(x, sure = TRUE)
library(tseries)

adf1 <- adf.test(x$X1,alternative = "stationary", k = 1)
print(adf1, digits = 5)
adf2 <- adf.test(x$X2, alternative = "stationary", k = 1)
print(adf2, digits = 5)

##Question (c): Engle-Granger Test for Cointegration
keep(x, sure = TRUE)

first_lm <- lm(X2 ~ X1, data = x)
print(summary(first_lm), digits = 1)

firstLm_residuals <- residuals(first_lm)
adf.test(firstLm_residuals, alternative = c("stationary"),k = 1) #
adf_cointegration <- adf.test(firstLm_residuals, alternative = c("stationary"),k = 1)
attributes(adf_cointegration)

##Question (d): Error Correction Model
keep(x,firstLm_residuals,sure = TRUE)
x$error_correction_term <- c(0,x$X2[1:40]) - 0.92*c(0,x$X1[1:40])
ecm1 <- lm(DX1 ~lag1_x1 + error_correction_term, data = x)
summary(ecm1)

ecm2 <- lm(DX2 ~lag1_x2 + error_correction_term, data = x)
summary(ecm2)


