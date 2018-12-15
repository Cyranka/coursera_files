remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_6/")
library(readxl)
library(dynlm)
x <- read_excel("TrainExer61.xlsx")

##Question (a)
par(mfrow = c(1,3))
plot(x = row.names(x), x$X, pch = 19, xlab = "Time", ylab = "X Variable", 
     main = "X Variable by Time")
plot(x = row.names(x), x$Y, pch =19, xlab = "Time", ylab = "Y Variable",
     main = "Y Variable by Time")
plot(x = x$X, x$Y, pch = 19, xlab = "X Variable", ylab = "Y Variable",
     main = "X and Y Scatter")
abline(lm(x$Y ~ x$X), col = "red", lwd = 3)

##Question (b)
lm_b <- lm(EPSY ~ EPSX, data = x)
round(summary(lm_b)$coefficients,4)

round(summary(lm_b)$coefficients,4)[2,3] ##t-value of EPSX
round(summary(lm_b)$coefficients,4)[2,4] ##p-value associated with t-value of EPSX


##Creating Lagged Variables that make sense
x$EPSX_1 <- c(0,x$EPSX[1:nrow(x)-1])
x$EPSX_2 <- c(0,x$EPSX_1[1:nrow(x)-1])
x$EPSX_3 <- c(0,x$EPSX_2[1:nrow(x)-1])
x$EPSY_1 <- c(0,x$EPSY[1:nrow(x)-1])
x$EPSY_2 <- c(0,x$EPSY_1[1:nrow(x)-1])
x$EPSY_3 <- c(0,x$EPSY_2[1:nrow(x)-1])

lm_c <- lm(EPSY ~EPSX +  EPSX_1 + EPSX_2 + EPSX_3 + EPSY_1 + EPSY_2 + EPSY_3, data = x[-c(1,2,3),])
summary(lm_c) ##The exercise removed the lines with zeros

##Question (d)
lm_d <- lm(Y ~ X, data = x)
print(summary(lm_d), digits = 2)

##Question (e)
res_partd <- as.data.frame(residuals(lm_d))
names(res_partd) <- "residuals"
res_partd$lag_residuals <-  c(0,res_partd$residuals[1:nrow(res_partd)-1])

lm_e <- lm(residuals ~ lag_residuals, data = res_partd[-1,])
print(summary(lm_e), digits = 2)