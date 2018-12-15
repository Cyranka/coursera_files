remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_6/")
library(readxl)
library(dynlm)
library(gdata)
library(ecm)
library(tseries)
#library(Metrics)
source("shift_function.R")

z <- read_excel("TestExer6-CARS-round2.xlsx") ##Full Dataset
x <- z[1:240,] ##Estimation Dataset

##Question (a): Graphs
par(mfrow = c(1,3))
plot(row.names(z),z$TOYOTA_SA,col = "red",type = "l",
     xlab = "Time", ylab = "Monthly Production", 
     main = "Toyota Seasonally Adjusted Production",xaxt = "n", yaxt = "n")

plot(row.names(z), z$OTHER_SA,col = "blue",type = "l",
     xlab = "Time", ylab = "Monthly Production", 
     main = "Other Seasonally Adjusted Production",xaxt = "n", yaxt = "n")

toyota_share <- z$TOYOTA_SA/(z$TOYOTA_SA + z$OTHER_SA)


plot(row.names(z), z$OTHER_SA,col = "darkgreen",type = "l",
     xlab = "Time", ylab = "Monthly Production", 
     main = "Toyota Share of Seasonally Adjusted Production",xaxt = "n", yaxt = "n")
dev.off()

##Question (b): 
# i)Dicky-Fuller Test on Toyota Production
x$TSA_diff <- c(0,diff(x$TOYOTA_SA))
x$lag1_TSAdiff <- c(0,x$TSA_diff[1:nrow(x)-1])
x$lag2_TSAdiff <- c(0,x$lag1_TSAdiff[1:nrow(x)-1])
x$lag3_TSAdiff <- c(0,x$lag2_TSAdiff[1:nrow(x)-1])
x$previous_TSA <- c(0,x$TOYOTA_SA[1:nrow(x)-1])

adf_TSA <- lm(TSA_diff ~time(`YYYY-MM`) + previous_TSA + lag1_TSAdiff + lag2_TSAdiff + lag3_TSAdiff, data = x[-c(1:4),]) ##With Linear Trend
adf_TSA2 <- lm(TSA_diff ~previous_TSA + lag1_TSAdiff + lag2_TSAdiff + lag3_TSAdiff, data = x[-c(1:4),]) ##With Linear Trend
round(summary(adf_TSA)$coefficients,4) ##Coefficients with Linear Trend
round(summary(adf_TSA2)$coefficients,4) ##Coefficients w/out Linear Trend

#adf.test(x$TOYOTA_SA, alternative = "stationary", 3)

# ii)Dicky-Fuller Test on Other Production
x$OSA_diff <- c(0,diff(x$OTHER_SA))
x$lag1_OSAdiff <- c(0,x$OSA_diff[1:nrow(x)-1])
x$lag2_OSAdiff <- c(0,x$lag1_OSAdiff[1:nrow(x)-1])
x$lag3_OSAdiff <- c(0,x$lag2_OSAdiff[1:nrow(x)-1])
x$previous_OSA <- c(0,x$OTHER_SA[1:nrow(x)-1])


adf_OSA <- lm(OSA_diff ~time(`YYYY-MM`) + previous_OSA + lag1_OSAdiff + lag2_OSAdiff + lag3_OSAdiff, data = x[-c(1:4),]) ##With Linear Trend
adf_OSA2 <- lm(OSA_diff ~previous_OSA + lag1_OSAdiff + lag2_OSAdiff + lag3_OSAdiff, data = x[-c(1:4),]) ##With Linear Trend

round(summary(adf_OSA)$coefficients,4) ##Coefficients with Linear Trend
round(summary(adf_OSA2)$coefficients,4) ##Coefficients with Linear Trend
#adf.test(x$OTHER_SA, alternative = "stationary", 3)

## Question (c): Engle-Granter Test for Cointegration
remove(adf_OSA, adf_OSA2, adf_TSA, adf_TSA2, toyota_share)

eg_1step <- lm(TOYOTA_SA ~ OTHER_SA, data = x) #Univariate Regression
eg_1res <- residuals(eg_1step)

diff_res <- c(0,diff(eg_1res))
previous_residual <- c(0,eg_1res[1:length(diff_res)-1])
lag1_diffres <- c(0,diff_res[1:length(diff_res) - 1])
lag2_diffres <- c(0,lag1_diffres[1:length(diff_res) - 1])
lag3_diffres <- c(0,lag2_diffres[1:length(diff_res) - 1])

res_df <- as.data.frame(cbind(diff_res, previous_residual, lag1_diffres, lag2_diffres, lag3_diffres))
res_df$period <- x$`YYYY-MM`

engle_granger1 <- lm(diff_res ~ time(period) + previous_residual + lag1_diffres + lag2_diffres + lag3_diffres, data = res_df[5:nrow(res_df),])
engle_granger2 <- lm(diff_res ~ previous_residual + lag1_diffres + lag2_diffres + lag3_diffres, data = res_df[5:nrow(res_df),])
round(summary(engle_granger1)$coefficients,3)
round(summary(engle_granger2)$coefficients,3)

#adf.test(eg_1res, alternative = "stationary", k = 3)

##Question (d): Sample Autocorrelations and Partial Autocorrelations and Fit new Model
keep(x, y, z,shift,sure = TRUE)

dfModel1 <- as.data.frame(cbind(x$TSA_diff, x$lag1_TSAdiff, x$lag2_TSAdiff, x$lag3_TSAdiff))
colnames(dfModel1) <- c("TSA_diff", "lag1_TSAdiff", "lag2_TSAdiff", "lag3_TSAdiff")
dfModel1$lag4_TSAdiff <- c(0,dfModel1$lag3_TSAdiff[1:nrow(dfModel1)-1])
dfModel1$lag5_TSAdiff <- c(0,dfModel1$lag4_TSAdiff[1:nrow(dfModel1)-1])
dfModel1$lag10_TSAdiff <- shift(dfModel1$TSA_diff, -10)
dfModel1$lag12_TSAdiff <- shift(dfModel1$TSA_diff, -12)

##
model1 <- lm(TSA_diff ~., data = dfModel1[14:nrow(dfModel1),])
print(summary(model1)$coefficients, digits = 3) 

##Question (e): Fit an Error Correction term
ec_term <- c(0,x$TOYOTA_SA[1:nrow(x)-1]) - 0.45*c(0,x$OTHER_SA[1:nrow(x)-1]) ##Error correction term is taken at the original variables, not their differences

dfModel2 <- as.data.frame(cbind(dfModel1,ec_term))
model2 <- lm(TSA_diff ~., data = dfModel2[14:nrow(dfModel1),])
round(summary(model2)$coefficients, digits = 3) 

##Question (f): Make Predictions and Assess Models
##Create new variables for new stuff
z$TSA_diff <- c(0,diff(z$TOYOTA_SA))
z$OSA_diff <- c(0,diff(z$OTHER_SA))
lags_to_include <- c(1,2,3,4,5,10,12)
z$lag1_TSAdiff <- shift(z$TSA_diff,-1)
z$lag2_TSAdiff <- shift(z$TSA_diff,-2)
z$lag3_TSAdiff <- shift(z$TSA_diff,-3)
z$lag4_TSAdiff <- shift(z$TSA_diff,-4)
z$lag5_TSAdiff <- shift(z$TSA_diff,-5)
z$lag10_TSAdiff <- shift(z$TSA_diff,-10)
z$lag12_TSAdiff <- shift(z$TSA_diff,-12)
z$ec_term <- c(0,z$TSA_diff[1:nrow(z)-1]) - 0.45*c(0,z$OSA_diff[1:nrow(z)-1])

y <- z[241:252,] ##Forecast Dataset

forecastModel1 <- predict(model1, y)
forecastModel2 <- predict(model2, y)


rmse <- function(x){
    error <- y$TSA_diff- x
    rmse <- sqrt(mean(error^2))
    rmse <- round(rmse, 4)
    return(rmse)
}

mae <- function(x){
    error <- abs(y$TSA_diff - x)
    mae <- mean(error)
    mae <- round(mae, 4)
    return(mae)
}

rmse_errors <- unlist(lapply(list(forecastModel1, forecastModel2), rmse))
mae_errors <- unlist(lapply(list(forecastModel1,forecastModel2), mae))
error_matrix <- data.frame(rbind(rmse_errors, mae_errors))

names(error_matrix) <- c("No EC Term", "EC Term Included")
row.names(error_matrix) <- c("RMSE Errors", "MAE Errors")

##Plotting Forecasting Residuals
model1_errors <- y$TSA_diff - forecastModel1
model2_errors <- y$TSA_diff - forecastModel2

errors_Frame <- as.data.frame(cbind(1:length(model1_errors), model1_errors,model2_errors))
names(errors_Frame) <- c("observation", "model1_errors", "model2_errors")

plot(errors_Frame$observation, errors_Frame$model1_errors, type = "l", ylab = "Residual",
     xlab = "Observation", main = "Forecast Errors", col = "red", ylim = c(-35000,35000))
lines(errors_Frame$observation, errors_Frame$model2_errors, type = "l",col = "blue")

###
write.csv(error_matrix, "error_matrix_question_f.csv", row.names = TRUE)