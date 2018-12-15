remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_4/")
library(readxl)
library(gdata)
x <- read_excel("TrainExer44.xlsx")

#Obtaining Biased OLS Coefficient
bias_ols <- lm(GC ~ PG + RI, data = x)
print(summary(bias_ols), digits = 2)
elasticity_ols <- (1 - round(exp(-0.528),4))*100

#Performing 2SLS
#Part 1: Regress X on the Instruments to obtain the Explained Part: Notice that RI was also included in the model
first_stage <- lm(PG ~ RI + RPT + RPN + RPU, data = x)

#Part 1.1: Obtain the fitted values
first_stage_xhat <- fitted.values(first_stage)

#Part 2: Regress Y on Fitted Values: YOU MUST INCLUDE ALL EXOGENOUS VARIABLE
second_stage <- lm(x$GC ~ first_stage_xhat + x$RI)
print(summary(second_stage), digits = 2)

elasticity_2sls <- (1 - round(exp(-0.54),4))*100
elasticity_2sls

##Sargan Test: Test to determine if the Instruments Are Correlated with the Error Term
residuals_2sls <- x$GC - as.matrix(cbind(1,x$PG, x$RI)) %*% as.matrix(as.vector(coefficients(second_stage)))

reg1 <- lm(residuals_2sls ~  x$RI + x$RPT + x$RPN + x$RPU)
print(summary(reg1), digits = 4)

test_chi <- 30*summary(reg1)$r.squared
round(1 - pchisq(test_chi,df = 4 - 2),2)
