remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_5/")
library(readxl)
library(gdata)
library(fmsb)
library(mfx)
x <- read_excel("Data Lecture 5-5.xls")
x <- x[1:925,]
x$age_square <- (x$age/10)**2
logit <- glm(response ~ male + activity + age + age_square, data = x, family = "binomial")
intercept_only <- glm(response ~ 1, data = x, family = "binomial")
print(summary(logit),digits = 2)

#Get Log Likelihood
log_likelihood_full <- logLik(logit)
log_likelihood_intercept_only <- logLik(intercept_only)

##Mcfadeen R2
mcfadden <- round(1 - (log_likelihood_full/log_likelihood_intercept_only),2)

##Nagelkerke R2
round(1 - NagelkerkeR2(logit)$R2,3) ##Ask question

##Odds Ration
round(exp(round(logit$coefficients,2)),2)

##An odds ratio of 2.59 means that male customers are 2.59 times more likely to invest,
## holding all other variables constant

#Average Marginal Effects
marginal_effects <- logitmfx(logit$formula, data = x)

#Test Exercise 5: Question (a)
active_male_50 <- as.data.frame(t(c(1,1,50,25))) ##
names(active_male_50) <- c("male", "activity", "age", "age_square")
predict(logit, active_male_50, type = "response") #Probability equals 1

prob_0 <- round(1 - predict(logit, active_male_50, type = "response"),4) ##Probability equals 0
prob_0*1*(0.914)

#Inactive, Male, 50 Years Old
inactive_male_50 <- as.data.frame(t(c(1,0,50,25)))
names(inactive_male_50) <- c("male","activity", "age", "age_square")
predict(logit, inactive_male_50, type = "response") #Probability that it equals 1
prob_0 <- 1 - predict(logit, inactive_male_50, type = "response")
prob_0
