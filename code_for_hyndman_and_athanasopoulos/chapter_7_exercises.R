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

# Question 5 --------------------------------------------------------------
rm(list = ls())

paperback <- books[,1]
hardcover <- books[,2]

#a) Plot the series
autoplot(books, facets = TRUE)

#b) Use ses to forecast the next four days
ses_paperback <- ses(y = paperback, h = 4)
ses_hardcover <- ses(y = hardcover, h = 4)

summary(ses_paperback)
autoplot(ses_paperback, series = "Paperback sales for the next four days")

summary(ses_hardcover)
autoplot(ses_hardcover, series = "Hardcover sales for the next four days")

# Question 7 --------------------------------------------------------------
holt_paperback <- holt(paperback, h = 4)
holt_hardcover <- holt(hardcover, h = 4)

#b)Compare SES
summary(holt_paperback)
summary(holt_hardcover)

autoplot(holt_paperback)
autoplot(holt_hardcover)

#d) Produce estimates of the confidence intervals
ses_paperback$mean[1] + c(-1,1)*1.96*accuracy(ses_paperback)[,2] ##SES Paperback
ses_hardcover$mean[1] + c(-1,1)*1.96*accuracy(ses_hardcover)[,2] ##SES Hardcover
holt_paperback$mean[1] + c(-1,1)*1.96*accuracy(holt_paperback)[,2] ##HOLT Paperback
holt_hardcover$mean[1] + c(-1,1)*1.96*accuracy(holt_hardcover)[,2] ##HOLT Hardcover

# Question 7 --------------------------------------------------------------
remove(list =ls())
##Eggs is year data

autoplot(eggs) + 
    labs(x = "Time", y = "Price of eggs")

naive_holt <- holt(eggs, h = 100)
autoplot(naive_holt)

damped_holt <- holt(eggs, h = 100,damped = TRUE,phi = 0.85)
autoplot(damped_holt)

#Use a box-cox
lambda_eggs <- BoxCox.lambda(eggs)

box_cox_holt <- holt(BoxCox(eggs, lambda = lambda_eggs), h = 100)
autoplot(box_cox_holt)


# Question 8 ---------------------------------------------------------------
rm(list =ls())

retaildata <- readxl::read_excel("retail.xlsx", skip = 1)
my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

autoplot(my_retail_series)

#Applying multiplicative method
holt_retail <- holt(my_retail_series,h = 120,seasonal = "multiplicative")
autoplot(holt_retail) ##Plot forecasts only
autoplot(my_retail_series) + 
    autolayer(holt_retail$fitted, series = "Fitted values") ##Fitted values


##Applying a damp parameter
holt_retail_damp <- holt(my_retail_series,h = 120,damped = TRUE,seasonal = "multiplicative")
autoplot(holt_retail_damp)

autoplot(my_retail_series) + 
    autolayer(holt_retail_damp$fitted, series = "Fitted values") ##Fitted values

#RMSE of one step forecasts
e_1 <- tsCV(my_retail_series,holt, h = 1)
e_2 <- tsCV(my_retail_series,holt, h = 1, damped =TRUE)

sqrt(mean(e_1**2, na.rm = TRUE))
sqrt(mean(e_2**2, na.rm = TRUE))

checkresiduals(holt_retail_damp)

#e) Test set RMSE
retail_until_2010 <- window(my_retail_series, end = c(2010,12))
test_retail <- window(my_retail_series, begin = c(2011,1))[346:381] %>%
    ts(frequency = 12, start = c(2011,1))

train_holt <- holt(retail_until_2010, h = 36)
train_holt_damped <- holt(retail_until_2010, h = 36, damped = TRUE)
train_snaive <- snaive(retail_until_2010, h = 36)

accuracy(train_holt, x = test_retail)
accuracy(train_holt_damped, x = test_retail)
accuracy(train_snaive, x = test_retail)

##Plot the data
autoplot(my_retail_series) + 
    autolayer(train_snaive, PI = FALSE)

autoplot(my_retail_series) + 
    autolayer(train_holt, PI = FALSE)

autoplot(my_retail_series) + 
    autolayer(train_holt_damped, PI = FALSE)