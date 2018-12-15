remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_7/")
source("shift_function.R")
library(readxl)
library(lmtest)
library(gdata)

##Load the Data
x <- read_excel("week_7_test_data.xls")
x$lagl1 <- shift(x$li1,-1)
x$lagl2 <- shift(x$li2, -1)

##Question (a)
intercept_only <- glm(GDPIMPR~1, data = x[5:244,], family = "binomial")
round(summary(intercept_only)$coefficients,3)

model2 <- glm(GDPIMPR~lagl1, data = x[5:244,], family = "binomial")
round(summary(model2)$coefficients,3)

model3 <- glm(GDPIMPR~lagl2, data = x[5:244,], family = "binomial")
round(summary(model3)$coefficients,3)

fullModel <- glm(GDPIMPR ~ lagl1 + lagl2, data = x[5:244,], family = "binomial")
round(summary(fullModel)$coefficients,3)

lrTest1 <- as.numeric(-2*(logLik(intercept_only) - logLik(fullModel)))
round(1 - pchisq(lrTest1,2),10) ##Reject H0 in favor of HA

lrTest2 <- as.numeric(-2*(logLik(model2) - logLik(fullModel)))
round(1 - pchisq(lrTest2,1),10) ##Reject H0 in favor of HA

lrTest3 <- as.numeric(-2*(logLik(model3) - logLik(fullModel)))
round(1 - pchisq(lrTest3,1),10) ##Reject H0 in favor of HA

##Question (b)
keep(x, intercept_only,fullModel,shift,sure = TRUE)

x$lagl1_2 <- shift(x$li1,-2)
x$lagl2_2 <- shift(x$li2, -2)


fullModel2 <-  glm(GDPIMPR ~ lagl1 + lagl2_2, data = x[5:244,], family = "binomial")
round(summary(fullModel2)$coefficients,3)

fullModel3 <-  glm(GDPIMPR ~ lagl1_2 + lagl2, data = x[5:244,], family = "binomial")
round(summary(fullModel3)$coefficients,3)

fullModel4 <-  glm(GDPIMPR ~ lagl1_2 + lagl2_2, data = x[5:244,], family = "binomial")
round(summary(fullModel4)$coefficients,3)


mcfadden1 <- round(as.numeric((1 - logLik(fullModel)/logLik(intercept_only))),4)
mcfadden2 <- round(as.numeric((1 - logLik(fullModel2)/logLik(intercept_only))),4)
mcfadden3 <- round(as.numeric((1 - logLik(fullModel3)/logLik(intercept_only))),4) ##Highest McFadden
mcfadden4 <- round(as.numeric((1 - logLik(fullModel4)/logLik(intercept_only))),4)

##Question (c)
keep(x, fullModel3,shift,sure = TRUE)

prob_predictions <- predict.glm(fullModel3, x[245:264,], type = "response")
class_predictions <- ifelse(prob_predictions > 0.5,1,0)

table(x[245:264,]$GDPIMPR) ##Baseline: 0.65
pred_real_table <- table(x[245:264,]$GDPIMPR, class_predictions)
pred_real_table <- pred_real_table/20 
pred_real_table 


##Question (d)
plot(ts(x$LOGGDP), ylab = "LOG GDP", main = "Graph of Log GDP", col = "blue")
x$LOGGDP_diff <- c(0,diff(x$LOGGDP))
x$lag1_LOGGDP_diff <- c(0,x$LOGGDP_diff[1:nrow(x)-1])
x$previous_LOGGDP <- c(0,x$LOGGDP[1:nrow(x)-1])


adf_test <- lm(LOGGDP_diff ~ time(Date) + previous_LOGGDP + lag1_LOGGDP_diff, data = x[5:244,])
round(summary(adf_test)$coefficients,4) ## T-Value = -2.3712

##Question (e)
keep(x, shift, sure = TRUE)
x$previous_GROWTHRATE <- c(0,x$GrowthRate[1:nrow(x)-1])

model11 <- lm(GrowthRate ~ previous_GROWTHRATE + lagl1 + lagl2, data = x[5:244,])
model12 <- lm(GrowthRate ~ previous_GROWTHRATE + lagl1 + lagl2_2, data = x[5:244,])
model21 <-lm(GrowthRate ~ previous_GROWTHRATE + lagl1_2 + lagl2, data = x[5:244,])
model22 <-lm(GrowthRate ~ previous_GROWTHRATE + lagl1_2 + lagl2_2, data = x[5:244,])

summary(model11)$r.squared
summary(model12)$r.squared
summary(model21)$r.squared
summary(model22)$r.squared

round(summary(model11)$coefficients,6)

##Question (f): Breusch-Godfrey Test on Residual Auto Correlations
bg_data <- x[5:244,]
step1_residuals <- residuals(model11)
bg_data$bg_residuals <- step1_residuals
bg_data$lag1_residuals <- shift(bg_data$bg_residuals, -1)

bg_lm <- lm(bg_residuals ~ previous_GROWTHRATE + lagl1 + lagl2 + lag1_residuals, data = bg_data[-1,])
summary(bg_lm)$r.squared

bg_statistic <- summary(bg_lm)$r.squared*239

round(1 - pchisq(bg_statistic, 1),4) ##Cannot reject Null Hypothesis of No Autocorrelation

##Question (g): Generating Predictions
keep(x, shift, model11, sure = TRUE)

y <- x[245:264,]
predictions <- predict(model11, y)

rmse <- function(actual, predicted){
    error <- actual- predicted
    rmse <- sqrt(mean(error^2))
    rmse <- round(rmse, 4)
    return(rmse)
}

rmse_model11 <- rmse(actual = y$GrowthRate, predictions)
rmse_model11

plot(ts(y$GrowthRate), ylab = "", main = "Actual (Blue Line) and Predicted (Red Line) Growth Rates", col = "blue", ylim =c(-0.015, 0.015))
lines(ts(predictions), col = "red")