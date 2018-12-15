remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_4/")
library(readxl)
library(gdata)
x <- read_excel("TrainExer42.xls")

###(a) Events only affect sales through its effect on price
reg_1 <- lm(SALES0_0 ~ PRICE0, data = x)
reg_2 <- lm(SALES0_1 ~ PRICE1, data = x)
reg_3 <- lm(SALES0_5 ~ PRICE5, data = x)
reg_4 <- lm(SALES0_10 ~ PRICE10, data = x)

coef_vector1 <- c(coefficients(reg_1)[2], coefficients(reg_2)[2], coefficients(reg_3)[2], coefficients(reg_4)[2])
round(coef_vector1,2)

r2_vector <- c(summary(reg_1)$r.squared, summary(reg_2)$r.squared, summary(reg_3)$r.squared, summary(reg_4)$r.squared)
round(r2_vector,4)*100


###(b) Events do not affect prices, but affect sales directly
keep(x, sure = TRUE)

reg_1 <- lm(SALES0_0 ~ PRICE0, data = x)
reg_2 <- lm(SALES1_0 ~ PRICE0, data = x)
reg_3 <- lm(SALES5_0 ~ PRICE0, data = x)
reg_4 <- lm(SALES10_0 ~ PRICE0, data = x)

coef_vector1 <- c(coefficients(reg_1)[2], coefficients(reg_2)[2], coefficients(reg_3)[2], coefficients(reg_4)[2])
round(coef_vector1,2)

r2_vector <- c(summary(reg_1)$r.squared, summary(reg_2)$r.squared, summary(reg_3)$r.squared, summary(reg_4)$r.squared)
round(r2_vector,4)*100

###(c) Events affect prices AND sales directly
keep(x, sure = TRUE)

reg_1 <- lm(SALES0_0 ~ PRICE0, data = x)
reg_2 <- lm(SALES1_1 ~ PRICE1, data = x)
reg_3 <- lm(SALES5_5 ~ PRICE5, data = x)
reg_4 <- lm(SALES10_10 ~ PRICE10, data = x)

coef_vector1 <- c(coefficients(reg_1)[2], coefficients(reg_2)[2], coefficients(reg_3)[2], coefficients(reg_4)[2])
round(coef_vector1,2)

r2_vector <- c(summary(reg_1)$r.squared, summary(reg_2)$r.squared, summary(reg_3)$r.squared, summary(reg_4)$r.squared)
round(r2_vector,4)*100