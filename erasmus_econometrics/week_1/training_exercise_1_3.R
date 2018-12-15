remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_1/")
library(readxl)
x <- read_excel("TrainExer13.xls")

##Computing slope
names(x) <- c("Game", "Year","Winning_Time")

b <- round(cov(x$Winning_Time, x$Game)/var(x$Game),2) ##-0.04

##Computing Intercept
a <- round(mean(x$Winning_Time) - b*mean(x$Game),2) ## 10.4

##Computing s
lm_fit <- lm(Winning_Time ~ Game, data = x)

#Get Residuals and calculate standard error of the residuals
reg_res <- residuals(lm_fit)
s <- round(sqrt(sum(reg_res**2)/13),4)

##Calculate Rˆ2
r_squared <- round(1 - (sum(reg_res**2)/sum((x$Winning_Time - mean(x$Winning_Time))**2)),4)

##Check Answers
summary(lm_fit)

##Plot the Data
plot(x$Game, x$Winning_Time, pch = 19, ylab = "Winning Time",xlab = "Game")
abline(a = a, b = b, col = "blue", lwd = 4)


##Predictions
predict(lm_fit, newdata = data.frame(Game = c(16,17,18)))

##Over Predicts 2008 and 2012, Under Predicts 2016