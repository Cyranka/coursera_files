remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")


# Simulation of an AR(2) process ------------------------------------------
set.seed(2017)


# Define true model parameters --------------------------------------------
sigma <- 4 #standard deviation
phi <- c(1/3,1/2) ##True model coefficients

n <- 10000 ##Total data points


# Simulate AR(2) process with model parameters ----------------------------
ar_2_process <- arima.sim(n = n,model = list(ar =phi), sd= sigma)


# Find sample autocorrelations (r vector) ---------------------------------
r <- acf(ar_2_process, type = "correlation",plot = FALSE)$acf[2:3]

# Create r matrix ---------------------------------------------------------
R <- matrix(c(1,r[1], r[1],1), nrow = 2)

# Solve for the solution --------------------------------------------------
phi_hat <- solve(R) %*% r ##Estimate for phi vector


# Estimation of the variance ----------------------------------------------
c0 <- acf(ar_2_process, type='covariance', plot=F)$acf[1] ##Variance at lag 0
var_hat <- c0*(1 - sum(phi_hat*r))
var_hat ##Estimate of the variance of the AR(2) process


# Graphing the process ----------------------------------------------------
par(mfrow=c(3,1))
plot(ar_2_process, main='Simulated AR(2)')
acf(ar_2_process, main='ACF')
pacf(ar_2_process, main='PACF')

