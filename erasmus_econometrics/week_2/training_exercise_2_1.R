remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_2/")
library(readxl)
x <- read_excel("TrainExer21.xls")

##Question (a)
gender_lm <- lm(LogWage ~ Female, data = x)
summary(gender_lm)

##Question (b)
#Grab Residuals
gender_lm_residuals <- residuals(gender_lm)

#(b)(i) Regress Residuals on Education
education <- x$Educ
res_on_edu <- lm(gender_lm_residuals ~ education)
summary(res_on_edu)

##(b)(ii) Regress Residuals on Part Time
part_time <- x$Parttime
res_on_parttime <- lm(gender_lm_residuals ~ part_time)
summary(res_on_parttime)



