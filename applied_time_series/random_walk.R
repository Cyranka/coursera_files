remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")

ts_vec <- 0
set.seed(50)

for(i in 2:1000){
    ts_vec[i] <- ts_vec[i -1] + rnorm(1)
}

random_walk <- ts(ts_vec)
plot(random_walk, main = "Random walk simulation",
     ylab = "", xlab = "Days", col = "blue", lwd = 2)

# Obtain the ACF ----------------------------------------------------------
(acf(random_walk)) ##Notice how there is a very high correlation until 30 lags


# Taking first differences ------------------------------------------------
first_diff <- diff(random_walk) ##Difference of lag 1

plot(first_diff, main = "First difference simulation", 
     ylab = "", xlab = "", col = "red", lwd = 2)

(acf(first_diff)) ##ACF of a pure random process

