remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)

# Chapter 5: Exercise 1 ---------------------------------------------------
#plot elecdaily
daily20 <- head(elecdaily,20)
autoplot(daily20[,"Demand"],color = "blue") + 
    labs(x = "Time", 
         y = "Electricity demand in GW")

#Regression model with temperature as explanatory variable
fit_model <- tslm(Demand ~ Temperature, data = daily20)
summary(fit_model)

fitted <- tibble(
    demand = as.numeric(daily20[,"Demand"]),
    temperature = as.numeric(daily20[,"Temperature"])
)

##plot the model
fitted %>% ggplot(aes(x = temperature, y = demand)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(x = "Temperature in ºC",
         y = "Demand for electricity in GW")

#Produce a residual plot
checkresiduals(fit_model,test = "LB")

tibble(
    obs = 1:20,
    res = residuals(fit_model)
) %>%
    mutate(scale_res = as.vector(scale(res,center = TRUE,scale = TRUE))) %>%
    arrange(desc(abs(scale_res)))

#Forecast with the model
forecast(fit_model, newdata = tibble(Temperature = c(15,35)))

#Plot demand x temperature for all the available data.
fitted <- tibble(
    demand = as.numeric(elecdaily[,"Demand"]),
    temperature = as.numeric(elecdaily[,"Temperature"])
)

fitted %>% ggplot(aes(x = temperature, y = demand)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(x = "Temperature in ºC",
         y = "Demand for electricity in GW")


# Chapter 5: Exercise 2 ---------------------------------------------------
rm(list = ls())
autoplot(mens400)

fit_model <- tslm(mens400 ~ trend)
fit_model

checkresiduals(fit_model)
forecast(fit_model, newdata = tibble(trend = 32))

# Chapter 5: Exercise 3 ---------------------------------------------------
easter(ausbeer)

# Chapter 5: Exercise 5 ---------------------------------------------------
rm(list = ls())

#a) Plot the data
autoplot(fancy)
ggseasonplot(fancy,
             year.labels = TRUE,
             year.labels.left = TRUE) + 
    ylab("Total sales") + 
    ggtitle("Seasonal plot: monthly sales for a souvenir shop")

#c)Fit a model to the log of the data
#Create time series for surfing
surf <- tibble(dates = seq.Date(from = lubridate::ymd("1987-01-01"),
         to = lubridate::ymd("1993-12-01"),by = "month")) %>%
    mutate(year = lubridate::year(dates),
           month = lubridate::month(dates)) %>%
    mutate(surfing_festival = ifelse(year >=1988 & month ==3,1,0))

surf_dummy <- surf %>% pull(surfing_festival) %>% ts(frequency = 12,
                                                     start = c(1987,1),
                                                     end = c(1993,12))

#c)Fit the model
model_1 <- tslm(formula = log(fancy) ~ trend + season + surf_dummy)

#d)Plot residuals against time and against fitted values
checkresiduals(model_1) ##
tibble(res = residuals(model_1),
       fitted = fitted(model_1)) %>%
    ggplot(aes(x = fitted, y= res)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE)

#e)Boxplot of residuals for every month
tibble(dates = seq.Date(from = lubridate::ymd("1987-01-01"),
                        to = lubridate::ymd("1993-12-01"),by = "month")) %>%
    mutate(year = lubridate::year(dates),
           month = lubridate::month(dates,label = TRUE),
           res = residuals(model_1)) %>%
    ggplot(aes(x = month, y = res)) + 
    geom_boxplot()

#f)Check coefficients
summary(model_1)

#g)Breusch-Godfrey test
checkresiduals(model_1)

#Produce predictions for the next 36 years
future_fancy <- rep(0, 36)
for(i in 1:36){
    if(i %% 12 == 3){
        future_fancy[i] <- 1
    }
} ##This recreates the surf variable

future_fancy <- ts(data = future_fancy,
                   start = c(1994,1),
                   end = c(1996, 12),
                   frequency = 12)

forecasts <- forecast(
    model_1,
    newdata = data.frame(Time = time(future_fancy),
                         surf_dummy = future_fancy)
) ##Fit the model

autoplot(forecasts) ##Plot the forecasts
original_scale_forecasts <- forecasts %>% broom::tidy() %>%
    mutate_all(exp) %>%
    ts(start = c(1994,1),
       end = c(1996,12),
       frequency = 12) ##Put the forecasts in the original scale
autoplot(log(fancy)) + 
    autolayer(forecasts,PI = TRUE) 

# Chapter 5: Exercise 6 ---------------------------------------------------
rm(list = ls())
autoplot(gasoline)

gasoline_new <- window(gasoline, end = c(2004,53))

#Fit a harmonic regression to the data

fit_1 <- tslm(gasoline_new ~ trend + fourier(gasoline_new, K = 4))
fit_2 <- tslm(gasoline_new ~ trend + fourier(gasoline_new, K = 13))
fit_3 <- tslm(gasoline_new ~ trend + fourier(gasoline_new, K = 26))

my_terms <- cbind(
    original = gasoline_new,
    k_4 = fitted(fit_1),
    k_13 = fitted(fit_2),
    k_26 = fitted(fit_3)
)

autoplot(my_terms,colour = TRUE,facets = TRUE)

#Select model using AICc or CV: use CV, because we are doing in-sample
#CV calculates automatically the terms
gdata::keep(gasoline_new, sure = TRUE)

AICc_K <- vector(mode = "numeric", length = 26)

#Fit models and get AICc: MUST BE A FOR LOOP
for(num in 1:26){
    AICc_K[[num]] <- CV(
        tslm(
            gasoline_new ~ trend + fourier(gasoline_new, K = num)
        )
    )[["AICc"]]
}
remove(num)

tibble(
    AICc_K,
    terms = 1:26
) %>% filter(AICc_K == min(AICc_K))

#Fit models using cross validation error
CV_error <- vector(mode = "numeric", length = 26)

for(num in 1:26){
    CV_error[[num]] <- CV(
        tslm(
            gasoline_new ~ trend + fourier(gasoline_new, K = num)
        )
    )[["CV"]]
}
remove(num)

tibble(
    CV_error,
    terms = 1:26
) %>% filter(CV_error == min(CV_error))

#Check residuals of the final model
final_model <- tslm(gasoline_new ~ trend + fourier(gasoline_new, K = 7))
checkresiduals(final_model)

#Forecast using harmonic regression
fc_2005 <- forecast(final_model, newdata = data.frame(fourier(gasoline_new,K = 7,h = 52)))

autoplot(fc_2005) + 
    autolayer(window(gasoline, start = c(2005,0),end = c(2005,53))) + 
    guides(level = FALSE)

# Question 7 --------------------------------------------------------------
rm(list = ls())

##Lake huron dataset
autoplot(LakeHuron)

##b) Fit a linear regression
fit_model <- tslm(LakeHuron ~ trend)

autoplot(cbind(LakeHuron, fitted.values(fit_model)))
CV(tslm(LakeHuron ~ trend))

##b) Fit a piecwise trend model
t <- time(LakeHuron) ##This function creates the vector of times
t_break <- 1915

tb_1 <- ts(pmax(0,t - t_break), start = 1875)
fit_pw <- tslm(LakeHuron ~ t + tb_1)

autoplot(cbind(LakeHuron, fitted.values(fit_pw)))
CV(tslm(LakeHuron ~ trend))
CV(fit_pw)

##Provide linear forecast
forecasts_linear <- forecast(fit_model,h = 8)

##Provide piecewise forecast
t <- time(LakeHuron)
t_new <- t[length(t)] + seq(8)
t_breaknew <- tb_1[length(tb_1)] + seq(8)

piecewise_df <- cbind(
    t = t_new,
    tb_1 = t_breaknew
) %>% as.data.frame()

fcasts_pw <- forecast(fit_pw, piecewise_df)

##Plot forecasts
autoplot(LakeHuron) + 
    autolayer(forecasts_linear)##Fit linear model
    
autoplot(LakeHuron) + 
    autolayer(fcasts_pw)##Fit piecewise model