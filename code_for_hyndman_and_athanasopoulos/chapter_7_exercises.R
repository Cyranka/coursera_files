remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal)

# Question 1 --------------------------------------------------------------
autoplot(pigs) + 
    labs(x = "Time", y= "Total slaughthered") + 
    theme_minimal()

fit_1 <- ses(pigs, h = 4)
autoplot(fit_1) ##Plot the forecasts obtained using SES

summary(fit_1) ##Parameter values

##Manual calculation of 95% PI for the first forecast
98816.41 + c(-2,2)*10308.58

# Question 2 --------------------------------------------------------------
#Writing function for SES
ses_function <- function(y, alpha,level){
    y_length <- length(y)
    values_list <- vector(mode = "numeric", length = y_length)
    for(j in 0:(y_length-1)){
        values_list[j+1] <- (alpha*((1- alpha)**j)*(y[y_length-j])) + ((1 - alpha)**y_length)*level
    }
    return(sum(values_list))
}

round(ses_function(pigs, 0.2971,77260.0561),2)

# Question 3 --------------------------------------------------------------
#Modify to return SES

ses_function_sse <- function(pars = c(alpha, level), y){
    y_length <- length(y)
    #Value list is the contribution of each previous forecast to the h+1 forecast
    # values_list <- vector(mode = "numeric", length = y_length)
    # for(j in 0:(y_length-1)){
    #     values_list[j+1] <- (alpha*((1- alpha)**j)*(y[y_length-j])) + ((1 - alpha)**y_length)*level
    # }
    alpha <- pars[1]
    level <- pars[2]
    
    fitted_values <- vector(mode = "numeric", length = y_length)
    fitted_values[1] <- level

    for(j in 2:(y_length)){
        fitted_values[j] <- alpha*y[j-1] + (1 - alpha)*fitted_values[j-1]
    }
    
    sse <- sum((y - fitted_values)**2)
    
    return(sse)
}

#Optimize the function
result <- optim(par = c(0.5,50000), fn = ses_function_sse, y = pigs)


# Question 4 --------------------------------------------------------------
combined_function <- function(initial_alpha, initial_level, series){
    first <-  optim(par = c(initial_alpha,initial_level), fn = ses_function_sse, y = series)
    second <- ses_function(series, alpha = first$par[1], level = first$par[2])
    
    return(list(first$par, second))
}

combined_function(0.5,50000,pigs)