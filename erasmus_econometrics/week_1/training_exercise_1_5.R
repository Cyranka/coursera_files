remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_1/")
library(readxl)
x <- read_excel("TrainExer15.xls")

names(x) <- c("game", "year","win_men", "win_women")

##Fit Linear Model
lm_men <- lm(win_men ~ game, data = x)
lm_women <- lm(win_women ~ game, data = x)

summary(lm_men)
summary(lm_women)

##Fit Non-Linear Model
log_men <- lm(log(win_men) ~ game, data = x)
log_women <- lm(log(win_women) ~ game, data = x)


##predict with linear trend
predict(lm_men, data.frame(game = c(49)))
predict(lm_women, data.frame(game = c(49)))

###
par(mfrow = c(1,2))
plot(x = x$game, y = x$win_men, xlab = "Game #", ylab = "Winning Time Men", pch = 19, ylim = c(5,15), xlim = c(0,70), main = "Linear Models")
abline(lm_men$coefficients[1], lm_men$coefficients[2], col = "blue")
abline(lm_women$coefficients[1], lm_women$coefficients[2], col = "red")

plot(x = x$game, y = log(x$win_men), xlab = "Game #", ylab = "Log Winning Time Men", pch = 19, ylim = c(2,4), xlim = c(0,70), main = "Exponential Models")
abline(log_men$coefficients[1], log_men$coefficients[2], col = "blue")
abline(log_women$coefficients[1], log_women$coefficients[2], col = "red")