remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")

# Simulation of an AR(3) process ------------------------------------------
set.seed(2017)

# Define true model parameters --------------------------------------------
sigma <- 4 #standard deviation
phi <- c(1/3,1/2, 7/100) ##True model coefficients

n <- 100000 ##Total data points

# Simulate AR(2) process with model parameters ----------------------------
# The sigma parameter refers to the sd of the residuals/white noise
ar_3_process <- arima.sim(n = n,model = list(ar =phi), sd= sigma)

# Find sample autocorrelations (r vector) ---------------------------------
r <- acf(ar_3_process, type = "correlation",plot = FALSE)$acf[2:4]

# Create r matrix ---------------------------------------------------------
R <- matrix(c(1,r[1],r[2],r[1],1,r[1],r[2],r[1],1), nrow = 3)

# Solve for the solution --------------------------------------------------
phi_hat <- solve(R) %*% r ##Estimate for phi vector
phi_hat

# Estimation of the variance ----------------------------------------------
c0 <- acf(ar_3_process, type='covariance', plot=F)$acf[1] ##Variance at lag 0
var_hat <- c0*(1 - sum(phi_hat*r))
var_hat ##Estimate of the variance of the white noise

# Graphing the process ----------------------------------------------------
par(mfrow=c(3,1))
plot(ar_3_process, main='Simulated AR(3)', ylab = "AR(3) Process")
acf(ar_3_process, main='ACF')
pacf(ar_3_process, main='PACF')


