remove(list= ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# Lake Huron dataset ------------------------------------------------------
plot(LakeHuron) ##Plot the actual data
plot(diff(LakeHuron)) ##Plot the first differences data

first_diff_lh <- diff(LakeHuron)

# Plot, ACF, and PACF -----------------------------------------------------
par(mfrow = c(3,1))

plot(diff(LakeHuron), ylab = "", main = "First differences of annual measurements of the level of Lake Huron")
acf(first_diff_lh, main = "ACF") ##ACF
pacf(first_diff_lh, main = "PACF",lag.max = 30) ##PACF


# Find the first elements of the ACF -------------------------------------
acf(first_diff_lh, main = "ACF", plot = FALSE)$acf[1:3]

# Fitting an AR(2) model: R matrix ----------------------------------------
R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
r=NULL
r[1:2]=acf(diff(LakeHuron), plot=F)$acf[2:3]
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R

# Fitting an AR(2) model: b vector ----------------------------------------
b=matrix(r,nrow=2,ncol=1)
b

# Yule-Walker estimator ---------------------------------------------------
phi_hat <- solve(R) %*%b
phi_hat
# Estimate the variance of the residuals ----------------------------------
c_0 <- acf(first_diff_lh, type='covariance', plot=F)$acf[1]
sigma_sq_hat <- c_0*(1 - sum(phi_hat*r))
sigma_sq_hat


# Estimate the intercept --------------------------------------------------
phi_0_hat <- mean(first_diff_lh)*(1 - sum(phi_hat)) ##Normal mean coefficient
phi_0_hat


