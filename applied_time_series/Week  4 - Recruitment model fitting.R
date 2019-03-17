remove(list= ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# Load and plot data ------------------------------------------------------
my_data <- rec
plot(my_data, main = "Recruitment time-series", col = "blue", lwd = 1,
     ylab = "Number of new fish")


# Subtract mean to get a series with mean = 0 -----------------------------
ar_process <- my_data - mean(my_data, na.rm = TRUE)



# Obtain ACF and PACF plots -----------------------------------------------
par(mfrow = c(2,1))
acf(ar_process, main = "Recruitment", col = "red", lwd = 3)
pacf(ar_process, main='PACF', col = "green", lwd = 3)


# Obtain sample autocorrelation -------------------------------------------
b <- acf(ar_process, plot=F)$acf[2:3]


# Obtain matrix R ---------------------------------------------------------
R <- matrix(c(1,b[1],b[1],1), nrow = 2)

# Obtain Yule-Walker estimates --------------------------------------------
phi_hat <- solve(R) %*%b


# Obtain Yule-Walker estimate of the variance of the residuals ------------
c_0 <- acf(ar_process, type = "covariance",plot = FALSE)$acf[1] 
sigma_sq_hat <- c_0*(1 - sum(phi_hat*b))
sigma_sq_hat

# Estimate of the intercept/constant term ---------------------------------
phi_0_hat <- mean(my_data)*(1 - sum(phi_hat))
phi_0_hat

# Predictions? ------------------------------------------------------------
par(mfrow = c(1,1))
predictions <- vector(length = 451, mode = "numeric")
for(i in 3:length(ar_process)){
    predictions[i] <- phi_0_hat + phi_hat[1]*ar_process[i-1] + phi_hat[2]*ar_process[i-2]
}
remove(i)

tibble(
    time = 1:451,
    recruitment = ar_process[3:453],
    predictions = predictions[3:453]
) %>%
    gather(type, value, - time) %>%
    ggplot(aes(x = time, y = value, color = str_to_title(type))) + 
    geom_line() + 
    hrbrthemes::theme_ipsum_rc() + 
    theme(
        legend.position = "bottom"
    )  + 
    guides(color = guide_legend(title = "Series",title.position = "top",
                                title.hjust = 0.5, label.position = "bottom")) + 
    labs(x = "Time", y = "Recruitment") + 
    scale_color_manual(values = c("royalblue", "firebrick"))
