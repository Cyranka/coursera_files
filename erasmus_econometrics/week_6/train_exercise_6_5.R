remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_6/")
library(readxl)
library(dynlm)
library(gdata)
library(ecm)
library(tseries)

z <- read_excel("TrainExer65.xlsx")

#Question (a)
x <- subset(z, z$YEAR < 2003) ##Subset the Estimation Sample

par(mfrow = c(1,2))
plot(x$YEAR, x$LOGIP, col = "red", type = "l", xlab = "Year", ylab = "Variable", main = "Log Variables")
lines(x$YEAR, x$LOGCLI, col = "blue")

plot(x$YEAR, x$GIP, col = "red", type = "l", xlab = "Year", ylab = "Variable", main  = "Growth Rates")
lines(x$YEAR, x$GCLI, col = "blue")

#Question (b): Augmented Dickey Fuller Test for Log IP
x$lag1_GIP <- c(0,x$GIP[1:nrow(x)-1])
x$lag2_GIP <- c(0,x$lag1_GIP[1:nrow(x)-1])
x$previous_GIP <- c(0,x$LOGIP[1:42])

adf_GIP <- lm(GIP ~ time(YEAR) + previous_GIP + lag1_GIP + lag2_GIP, data = x[-c(1,2),])
print(summary(adf_GIP)$coefficients, digits = 1) ##Critical value is -3.5, we can't reject H0

#Question (b): Augmented Dickey Fuller Test for Log CLI
x$lag1_GCLI <- c(0,x$GCLI[1:nrow(x)-1])
x$lag2_GCLI <- c(0,x$lag1_GCLI[1:nrow(x)-1])
x$previous_GCLI <- c(0,x$LOGCLI[1:42])

adf_CLI <- lm(GCLI ~ time(YEAR) + previous_GCLI + lag1_GCLI + lag2_GCLI, data = x[-c(1,2),])
print(summary(adf_CLI)$coefficients, digits = 3) #Log CLI not stationary


##Question (c): Engle-Granger Test for Cointegration 
keep(x,z,sure = TRUE)
eg_1step <- lm(LOGIP ~ LOGCLI, data = x) #Univariate Regression
eg_1res <- residuals(eg_1step) ##Saving Residuals
diff_res <- c(0,diff(eg_1res))
previous_residual <- c(0,eg_1res[1:42]) ##Previous Residuals

lag1_diffres <- c(0,diff_res[1:42])
lag2_diffres <- c(0,lag1_diffres[1:42])

res_df <- as.data.frame(cbind(diff_res, previous_residual, lag1_diffres, lag2_diffres))
res_df$year <- x$YEAR

engle_granger <- lm(diff_res ~ time(year) + previous_residual + lag1_diffres + lag2_diffres, data = res_df[4:43,])
round(summary(engle_granger)$coefficients,2) ##Do not include the 0 values

adf.test(eg_1res, alternative = "stationary", k = 2) ##This gives the right answer

##Question (d): Performing f-tests
keep(x, z, sure = TRUE)

#Granger causality of X1 onto X2
unrestricted_GIP <- lm(GIP ~ lag1_GIP + lag2_GIP + lag1_GCLI + lag2_GCLI, data = x[3:43,]) ##killed the 0 values
restricted_GIP <- lm(GIP ~ lag1_GIP + lag2_GIP, data =x[3:43,]) ##killed the 0 values

numerator1 <- (summary(unrestricted_GIP)$r.squared - summary(restricted_GIP)$r.squared)/2
denominator1 <- (1 - summary(unrestricted_GIP)$r.squared)/(41 - 5) ##this is the n rows after removing the 0 values
fValue_x1 <- numerator1/denominator1

round(1 - pf(fValue_x1, 2,36),2) #rejects Ho, GLCI past values help predict present values of GCI

unrestricted_GCLI <- lm(GCLI ~ lag1_GIP + lag2_GIP + lag1_GCLI + lag2_GCLI, data = x[3:43,]) ##killed the 0 values
restricted_GCLI <- lm(GCLI ~ lag1_GCLI + lag2_GCLI, data =x[3:43,]) ##killed the 0 values

numerator2 <- (summary(unrestricted_GCLI)$r.squared - summary(restricted_GCLI)$r.squared)/2
denominator2 <- (1 - summary(unrestricted_GCLI)$r.squared)/(41 - 5)

fValue_x2 <- numerator2/denominator2
round(1 - pf(fValue_x2, 2,36),2)

##Question (e)
keep(x, z, sure = TRUE)

#i) AR(2) model for GIP
ar2_gip <- lm(GIP ~ lag1_GIP + lag2_GIP,data = x[3:43,]) ##Start with the column withouth the 0 
round(summary(ar2_gip)$coefficients, 4) ##Both coefficients are not statiscally significant

ar1_gip <- lm(GIP ~ lag1_GIP, data = x[2:43,]) ##Start with the column withouth the 0
round(summary(ar1_gip)$coefficients, 4) ##Coefficient not statistically significant

gip_intercept <- lm(GIP ~ 1, data = x)

##Make Predictions
z$lag1_GIP <- c(0,z$GIP[1:nrow(z)-1])
z$lag2_GIP <- c(0,z$lag1_GIP[1:nrow(z)-1])
z$previous_GIP <- c(0,z$LOGIP[1:47])

z$lag1_GCLI <- c(0,z$GCLI[1:nrow(z)-1])
z$lag2_GCLI <- c(0,z$lag1_GCLI[1:nrow(z)-1])
z$previous_GCLI <- c(0,z$LOGCLI[1:47])

forecasts_ar1 <- predict(ar1_gip, z[44:48,])
forecasts_ar1_gipIntercept <- predict(gip_intercept, z[44:48,])

##Question (f)
keep(x, z, forecasts_ar1, forecasts_ar1_gipIntercept,sure = TRUE)

#Estimating ADL(2,2) model
adl_22 <- lm(GIP ~ lag1_GIP + lag2_GIP + lag1_GCLI + lag2_GCLI, data = x[3:43,]) #The first row is the row without 0
adl_restricted <- lm(GIP ~ lag1_GCLI, data = x[3:43,]) ##The first row is the row without 0s

numerator <- (summary(adl_22)$r.squared - summary(adl_restricted)$r.squared)/3
denominator <- (1 - summary(adl_22)$r.squared)/(41 - 5)

fValue = numerator/denominator
round(1 - pf(fValue, 3,37),2) #Not statistically significant, the restricted model does as well as the unrestricted

adl_restricted <- lm(GIP ~ lag1_GCLI, data = x[2:43,]) ##The first row is the row without 0s
forecasts_adlRestricted <- predict(adl_restricted, z[44:48,])

rmse <- function(x){
    error <- z$GIP[44:48] - x
    rmse <- sqrt(mean(error^2))
    rmse <- round(rmse, 4)
    return(rmse)
}

mae <- function(x){
    error <- abs(z$GIP[44:48] - x)
    mae <- mean(error)
    mae <- round(mae, 4)
    return(mae)
}

sum_errors <- function(x){
    error <- z$GIP[44:48] - x
    error_sum <- round(sum(error),4)
    return(error_sum)
}

rmse_errors <- unlist(lapply(list(forecasts_ar1_gipIntercept,forecasts_ar1, forecasts_adlRestricted), rmse))
mae_errors <- unlist(lapply(list(forecasts_ar1_gipIntercept,forecasts_ar1, forecasts_adlRestricted), mae))
sum_of_errors <- unlist(lapply(list(forecasts_ar1_gipIntercept,forecasts_ar1, forecasts_adlRestricted), sum_errors))

error_matrix <- data.frame(rbind(rmse_errors, mae_errors, sum_of_errors))
names(error_matrix) <- c("AR 0", "AR 1", "ADL (0,1)")
error_matrix