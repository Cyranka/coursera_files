remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_5/")
library(readxl)
library(gdata)
library(fmsb)
library(mfx)

#
x <- read_excel("TrainExer5-5.xls")
x <- x[complete.cases(x$activity),]

#Estimating the Parameters of a Logit Model
x$age_square <- (x$age/10)**2
logit <- glm(response ~ male + activity + age + age_square, data = x, family = "binomial")
print(summary(logit),digits = 2)

#Get Log Likelihood of the Model
log_likelihood <- round(as.numeric(logLik(logit)),3)

#Obtaining the 'reverse model'
x$reverse_response <- ifelse(x$response == 0, 1, 0)
reverse_logit <- glm(reverse_response ~ male + activity + age + age_square, data = x, family = "binomial")

print(summary(reverse_logit),digits = 2)

#Model with Parameter Restrictions
restricted_logit <- glm(response ~ age + age_square, data = x, family = "binomial")
log_likRestrict <- round(as.numeric(logLik(restricted_logit)),3)

test_statics <- (-2)*(log_likRestrict - log_likelihood)
round(1 - pchisq(test_statics,2),3)

##For the test exercise
to_predict <- data.frame(male = 1, activity = 0, age = 50, age_square = 25)
to_predict2 <- data.frame(male = 1, activity = 1, age = 50, age_square = 25)
predict(logit, to_predict, type = "response")
predict(logit, to_predict2, type = "response")
