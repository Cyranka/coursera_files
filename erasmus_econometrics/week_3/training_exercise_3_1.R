remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_3/")
library(readxl)
x <- read_excel("TrainExer 3-1.xlsx")

## (a) Regression of Difference Log-Index on BookMarket##
log_Index <- log(x$Index)
lnchange_Index <- diff(log_Index, differences = 1)
x_1 <- x[-1,] ##Need to drop the first observation
x_1$lnchange_Index <- lnchange_Index

reg_1 <- lm(lnchange_Index ~ BookMarket, data = x_1)
summary(reg_1)

## (b) Regression of Index on BookMarket: Don't need to drop the first observation
reg_2 <- lm(Index ~ BookMarket, data = x)
summary(reg_2) #The Effect of BookMarket is still statistically significant

##
par(mfrow = c(1,2))
plot(residuals(reg_1), pch = 19, ylab =  "Residuals", xlab =  "Index", main = "Residuals of Log Difference")
plot(residuals(reg_2), pch = 19, ylab =  "Residuals", xlab =  "Index", main = "Residuals of non-transformed variable")

plot(residuals(reg_1),y = reg_1$fitted.values, pch = 19, ylab =  "Residuals", xlab =  "Fitted Values", main = "Residuals of Log Difference")
plot(residuals(reg_2),y = reg_2$fitted.values, pch = 19, ylab =  "Residuals", xlab =  "Fitted Values", main = "Residuals of Log Difference")