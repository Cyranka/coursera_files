remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_1/")
library(readxl)
x <- read_excel("TrainExer11.xls")

##Question a)
par(mfrow = c(1,3))
hist(x$Age, main = "Histogram of Age", col = "blue", xlab = "Age") ##Age Histogram
hist(x$Expenditures, main = "Histogram of Expenditures", col = "red", xlab = "Expenditures") ##Expenditures
plot(x$Age, x$Expenditures, pch = 19, ylab = "Expenditures", xlab = "Age")

##Question d)
round(mean(x$Expenditures), 2) ##Sample mean for all observations

##Question e)
round(mean(subset(x, Age >=40)$Expenditures),2) ##Mean for people over 40
round(mean(subset(x, Age < 40)$Expenditures),2) ##Mean for people under 40

##Question f)
LinearModel <- lm(Expenditures ~ Age, data = x)
round(predict(LinearModel, newdata = data.frame(Age = c(50,25))),2)
##Or you can use the uncondtional mean for each group