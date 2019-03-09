remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")

##Graphs of white noise, MA3, and MA5
par( mfrow=c(3,1));
plot( arima.sim(n=150, list(order=c(0,0,0) )  ), main="WN", ylab = "");
plot( arima.sim(n=150, list(ma=c(0.33, 0.33, 0.33)      )  ) , main="MA3",ylab = "");
plot( arima.sim(n=150, list(ma=c(0.2, 0.2, 0.2, 0.2, 0.2) )  ), main="MA5" ,ylab = "");

##
par(mfrow = c(1,1))
set.seed=1
(acf(arima.sim(n=1000, model=list(ma=c(0.5,0.5)))))