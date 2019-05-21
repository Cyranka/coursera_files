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

# Question 9 -------------------------------------------------------------
#Find lambda
box_cox_lambda <- BoxCox.lambda(my_retail_series)
bc_retail_train <- BoxCox(my_retail_series, box_cox_lambda) %>% window(end = c(2010,12))
bc_retail_test <- BoxCox(my_retail_series, box_cox_lambda) 
bc_retail_test <- bc_retail_test[346:381] %>%
    ts(frequency = 12, start = c(2011,1))

stl_transformed <- bc_retail_train %>%
    stl(t.window = 13, s.window = "periodic", robust = TRUE) ##This can be periodic, because of the transformation

autoplot(stl_transformed) ##Notice how it stabliized seasonal variation

##Extract seasonally adjusted data
season_adjusted <- seasadj(stl_transformed)

##Forecast next two years using damped method
##AAN means additive errors, additive trend, no seasonality
forecasts_damped <- stlf(season_adjusted, etsmodel = "AAN", damped = TRUE,h = 24)
forecasts_regular <- stlf(season_adjusted, etsmodel = "AAN", damped = FALSE,h = 24)

##Use ETS to find better model automatically
ets_auto_model <- ets(bc_retail_train)
ets_forecast <- ets_auto_model %>% forecast(h = 24)

##Compare fists
accuracy(forecasts_damped,bc_retail_test)[2,2] ##RMSE
autoplot(forecasts_damped, PI = FALSE)
accuracy(forecasts_regular,bc_retail_test)[2,2] ##RMSE
autoplot(forecasts_regular, PI = FALSE)
accuracy(ets_forecast,bc_retail_test)[2,2] ##RMSE
autoplot(ets_forecast, PI = FALSE)

# Question 10 -------------------------------------------------------------
remove(list = ls())

#Plot the data
autoplot(ukcars);frequency(ukcars)

#use stl to decompose the data: I will assume that the seasonality is additive
stl_decomposition <- ukcars %>%
    stl(t.window = 5, s.window = "periodic", robust = TRUE) 
autoplot(stl_decomposition)

ukcars_seasadj <- seasadj(stl_decomposition)
autoplot(ukcars, series = "Data", alpha = 0.5) + 
    autolayer(ukcars_seasadj, series = "Seasonally adjusted")    

#Forecast the next two years of data
forecast_stl <- stlf(ukcars,h = 8,etsmodel = "AAN",damped = TRUE) ##You can pass the series directly
autoplot(forecast_stl, PI = FALSE)
forecast_holt <- stlf(ukcars,h = 8,etsmodel = "AAN",damped = FALSE)
autoplot(forecast_holt, PI = FALSE)

#ETS model
ets_fit <- ets(ukcars)
ets_f <- ets_fit %>% forecast(h = 8)
autoplot(ets_f)
##
accuracy(forecast_stl)
accuracy(forecast_holt)
accuracy(ets_f)

#Check residuals of better in-sample model
checkresiduals(forecast_holt)