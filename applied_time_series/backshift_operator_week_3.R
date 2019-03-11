remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(tidyverse)


# Backshift operator and the ACF (Handout) ------------------------------------------
set.seed(2016)
N <- 1000
phi <- 0.4
Z <- rnorm(N,0,1)

X <- NULL
X[1] <- Z[1]

for(t in 2:N){
    X[t] <- Z[t] + phi*X[t-1]
}
remove(t)

X_ts <- ts(X)
X_acf <- acf(X_ts)

head(X_acf$acf) ##first coefficients
mean(X_ts) ##Mean
var(X_ts) ##Standard deviation