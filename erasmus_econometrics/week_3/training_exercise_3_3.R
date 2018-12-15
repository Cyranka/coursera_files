remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_3/")
library(readxl)
x <- read_excel("TrainExer 3-3.xlsx")

## (b) Regression of Difference Log-Index on BookMarket and BookMarketˆ2##
log_Index <- log(x$Index)
lnchange_Index <- diff(log_Index, differences = 1)
x_1 <- x[-1,] ##Need to drop the first observation
x_1$lnchange_Index <- lnchange_Index
x_1$BookMarketSquared <- x_1$BookMarket**2

model1 <- lm(lnchange_Index ~ BookMarket + BookMarketSquared, data = x_1)
summary(model1)

## (c) Defining a constant for 1980 and regressing it: 1980 should not necessary be a variable
x_1$dummy1980 <- ifelse(x_1$Year >= 1980, 1, 0)
model2 <- lm(lnchange_Index ~ BookMarket + BookMarket:dummy1980, data = x_1)
summary(model2)