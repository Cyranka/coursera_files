remove(list= ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# Johnson & Johnson quarterly earnings ------------------------------------
plot(JohnsonJohnson, main = "Johnson & Johnson earnings per share",
     col ="red", lwd = 2, ylab = "Earnings per share")

# Log return of Johnson & Johnson -----------------------------------------
jj_log_return <- diff(log(JohnsonJohnson))
jj_log_return_mean_0 <- jj_log_return - mean(jj_log_return)

# Obtain time plot, ACF, and PACF -----------------------------------------
par(mfrow = c(3,1))
plot(jj_log_return_mean_0, main = "Mean 0 log returns of Johnson & Johnson shares",
     col = "blue", lwd = 2, ylab = "Log returns")

acf(jj_log_return_mean_0, main = "ACF")
pacf(jj_log_return_mean_0, main = "PACF")

# Obtain the r vector -----------------------------------------------------
r <- acf(jj_log_return_mean_0, main = "ACF", plot = FALSE)$acf[2:5]

# Obtain matrix R ---------------------------------------------------------
R <- matrix(1,4,4)

for(i in 1:4){
    for(j in 1:4){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
    }
}

# Solve for sigma hat -----------------------------------------------------
phi_hat <- solve(R) %*% r


# Obtain the estimate of the variance of residuals ------------------------
c_0 <- acf(jj_log_return_mean_0, type = "covariance",plot = FALSE)$acf[1] 
sigma_sq_hat <- c_0*(1 - sum(phi_hat*r))
sigma_sq_hat

# Obtain the constant in the model ----------------------------------------
phi_0_hat <- mean(jj_log_return)*(1 - sum(phi_hat)) ##Normal mean coefficient
phi_0_hat


# Predictions from the model ----------------------------------------------
par(mfrow = c(1,1))
predictions <- vector(length = length(jj_log_return_mean_0), mode = "numeric")
predictions <- vector(length = 83, mode = "numeric")
for(i in 5:length(jj_log_return_mean_0)){
    predictions[i] <- phi_0_hat + 
        phi_hat[1]*jj_log_return[i-1] + 
        phi_hat[2]*jj_log_return[i-2] + 
        phi_hat[3]*jj_log_return[i-3] + 
        phi_hat[4]*jj_log_return[i-4]
}
remove(i)

tibble(
    time = 1:80,
    earnings = jj_log_return[4:83],
    predictions = predictions[4:83]
)%>%
    gather(type, value, - time) %>%
    ggplot(aes(x = time, y = value, color = str_to_title(type))) + 
    geom_line() + 
    hrbrthemes::theme_ipsum_rc() + 
    theme(
        legend.position = "bottom"
    )  + 
    guides(color = guide_legend(title = "Series",title.position = "top",
                                title.hjust = 0.5, label.position = "bottom")) + 
    labs(x = "Time", y = "Earnings")