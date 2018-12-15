remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_4/")
library(readxl)
library(gdata)
x <- read_excel("TrainExer45.xls")

##(a) Obtaining Biased OLS Coefficients##
ols <- lm(GPA ~ GENDER + PARTICIPATION, data = x)
print(summary(ols), digits = 2)

##(b) Fitting 2SLS
## b.i) First Stage
first_stage <- lm(PARTICIPATION ~ GENDER + EMAIL, data = x)
xhat_first_stage <- fitted.values(first_stage) ##Obtain Fitted Values

## b.ii) Second Stage: Regress Y on Fitted Values and all exogenous variables
second_stage <- lm(x$GPA ~ x$GENDER + xhat_first_stage)
print(summary(second_stage), digits = 2)

##NOTICE THAT THE STD. ERRORS FOR THE BETA VECTOR ARE WRONG -> NEEDS TO CALCULATE RESIDUALS CORRECTLY

## c) Calculating the Ratio Between STD. ERRORS
std_errors2sls <- round(coef(summary(second_stage))[,"Std. Error"],3)
std_errorlectures <- c(0.048, 0.048,0.115)

round(std_errors2sls/std_errorlectures,2)
