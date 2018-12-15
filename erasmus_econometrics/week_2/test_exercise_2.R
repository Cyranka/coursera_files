remove(list = ls())
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/online_learning/coursera/econometrics_mooc/week_2/")
library(readxl)
x <- read_excel("TestExer2-GPA-round2.xls")

#(a)
#(i) ## Regress FGPA on SATV and a constant
fgpa_satv <- lm(FGPA ~ SATV, data = x)
summary(fgpa_satv)

#(ii) Calculating a 95% Confidence Interval for SATV
round(confint(fgpa_satv, 'SATV',level = 0.95),3)

#(b)
#(i) ##Regress FGPA on all variables
fgpa_all <- lm(FGPA ~ FEM + SATM + SATV, data = x)
summary(fgpa_all)

#(ii) Calculating a 95% Confidence Interval for SATV
#round(0.01416 + c(-1,1)*qt(p = 0.975,df = nrow(x) - 4)*0.02793,3)
round(confint(fgpa_all, 'SATV', level = 0.95), 3)

#(iii) ##Create a Correlation Matrix: a Correlation Matrix is symmetric
corMatrix <- round(cor(x[,c("FGPA","SATM","SATV","FEM")]),2)
corMatrix[upper.tri(corMatrix, diag = FALSE)] <- ""

#(d) ##Performing an F-Test with the linear restriction that coef for SATV is equal to zero
fgpa_minus_satv <- lm(FGPA ~ FEM + SATM, data = x)

##Getting the restricted and unrestricted Rˆ2
r2_unrestricted <- summary(fgpa_all)$r.squared
r2_restricted <- summary(fgpa_minus_satv)$r.squared

f_numerator <- (r2_unrestricted - r2_restricted)/1
f_denominator <-(1 - r2_unrestricted)/(nrow(x) - 4)

f_value <- round(f_numerator/f_denominator,6) ##F-Value
1 - pf(f_value, 1,605) ##
#qf(p = 0.95,1,604)

#(ii) Checking that F = t^2 when there is only linear restriction
t_value_for_SATV = round(coef(summary(fgpa_all))[,"t value"][4],6)

round(t_value_for_SATV**2,6) == round(f_value,6)


