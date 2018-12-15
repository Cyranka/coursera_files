remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_1/")
library(readxl)

x <- read_excel("TestExer1-holiday expenditures-round2.xls")

##Question 1##
first_model <- lm(Expenditures ~ Age, data = x)
print(summary(first_model),digits = 2)

##Question 2
plot(x = x$Age, y = x$Expenditures,xlab = "Age", ylab = "Expenditures",
     main = "Plot of Average Expenditure by Age", pch = 19)

abline(a = first_model$coefficients[1], b = first_model$coefficients[2], col = "red", lwd = 3)

##Question 3
under_40 <- subset(x, x$Age < 40)
over_40 <- subset(x, x$Age >= 40)

#Under 40 model
model_under40 <- lm(Expenditures ~ Age, data = under_40)
print(summary(model_under40), digits = 2)

#Over 40 model
model_over40 <- lm(Expenditures ~ Age, data = over_40)
print(summary(model_over40), digits = 2)

##Part D Graph
par(mfrow = c(1,2))
plot(x = x$Age, y = x$Expenditures,xlab = "Age", ylab = "Expenditures",
     main = "Plot of Average Expenditure by Age: Initial Model", pch = 19)
abline(a = first_model$coefficients[1], b = first_model$coefficients[2], col = "red", lwd = 3)

plot(x = x$Age, y = x$Expenditures,xlab = "Age", ylab = "Expenditures",
     main = "Plot of Average Expenditure by Age: Different Models", pch = 19)
abline(a = model_under40$coefficients[1], b = model_under40$coefficients[2], col = "blue", lwd = 3)
abline(a = model_over40$coefficients[1], b = model_over40$coefficients[2], col = "green", lwd = 3)


print(paste("Estimate of the Intercept: ", as.character(round(coef(summary(first_model))[,"Estimate"][1],2)), sep = ""))

##Joining two clusters provide a misleading result that there is a negative association
##between age and expenditures. Separating the two sets of observations provide a more accurate
##description of the dataset.