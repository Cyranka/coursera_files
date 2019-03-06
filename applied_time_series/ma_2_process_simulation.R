remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")

# Generate noise
set.seed(2)
noise <- rnorm(10000)

# Introduce a variable
ma_2 <- NULL


# Loop for generating MA(2) process
for(i in 3:10000){
    ma_2[i] <- noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}
remove(i)

##Shift data to left by 2 units
moving_average_process <- ma_2[3:10000]

# Put time series structure on a vanilla data
moving_average_process <- ts(moving_average_process)

# Partition output graphics as a multi frame of 2 rows and 1 column
par(mfrow=c(2,1))

# plot the process and plot its ACF
plot(moving_average_process, main='A moving average process of order 2', ylab=' ', col='blue')
acf(moving_average_process, main='Correlogram of a moving average process of order 2')
