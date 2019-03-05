remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")

##Generate a purely random process
set.seed(1)
purely_random_process <- ts(rnorm(100))
purely_random_process

##Produces auto covariance coefficients for every single lag
(acf(purely_random_process, type = "covariance"))

##Produces auto correlation coefficients for every single lag
(acf(purely_random_process, main = "Correlogram of a purely random process",lag.max = 30))
