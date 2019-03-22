remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(astsa);library(tidyverse)

# Daily female births in CA, 1959 -----------------------------------------
birth_data <- read_csv("daily_female_births_ca_1959.csv") %>%
    magrittr::set_colnames(c("date", "total_births"))

# Retrieve series ---------------------------------------------------------
number_of_births <- birth_data %>% pull(total_births)


# Plot the series ---------------------------------------------------------
plot(ts(number_of_births), main = "Daily female births in California, 1959",
     xlab = "Date", ylab = "Total births",
     lwd = 0.75, col = "black")

# Use Ljung-Box to test for autocorrelation -------------------------------
Box.test(number_of_births, lag = log(length(number_of_births))) %>%
    broom::tidy() %>%
    mutate_at(.vars = vars(statistic:parameter),~round(.,2)) ##We reject Ho

# Difference the series and plot ------------------------------------------
plot(ts(diff(number_of_births)),
     main = "Differenced daily female births in California, 1959",
     xlab = "Date",
     ylab = "", 
     lwd = 0.75,col = "gray30") ##Mean looks stationary, ignoring variance

# Use Ljung-box on the differenced series ---------------------------------
Box.test(diff(number_of_births), lag = log(length(diff(number_of_births)))) %>%
    broom::tidy() %>%
    mutate_at(.vars = vars(statistic:parameter),~round(.,2)) ##Again reject H0

# ACF of the differenced series -------------------------------------------
acf(diff(number_of_births),
    main = "ACF on differenced daily female births in CA, 1959")#Suggests MA(2)

# PACF on the differenced series ------------------------------------------
pacf(diff(number_of_births),
    main = "PACF on differenced daily female births in CA, 1959")#Suggests AR(7)

# Fit different ARIMA models with d=1 -------------------------------------
model_1 <- arima(number_of_births,order = c(0,1,1)) ##ARIMA (0,1,1)
model_2 <- arima(number_of_births,order = c(0,1,2)) ##ARIMA (0,1,2)
model_3 <- arima(number_of_births,order = c(7,1,1)) ##ARIMA (7,1,1)
model_4 <- arima(number_of_births,order = c(7,1,2)) ##ARIMA (7,1,2)

# Retrieve SSE and AIC for each model -------------------------------------
aic <- sapply(list(model_1, model_2,model_3,model_4), function(i)i$aic)
sse <- sapply(list(model_1, model_2,model_3,model_4), function(i)sum(i$residuals**2))
q_statistic <- sapply(list(model_1, model_2,model_3,model_4),
                      function(i)Box.test(i$residuals, lag = log(length(i$residuals)))$p.value)


# Put results in a tibble -------------------------------------------------
tibble(
    model = c("ARIMA (0,1,1)","ARIMA (0,1,2)","ARIMA (7,1,1)", "ARIMA (7,1,2)"),
    AIC = aic,
    SSE = sse,
    p_value = q_statistic
    
) ##AIC is smaller in the second model, p-values are all non-significant

# Obtain coefficients -----------------------------------------------------------
dev.off() ##Very large graph
sarima_model <- sarima(number_of_births, 0,1,2,0,0,0)
sarima_model$ttable %>%
    broom::tidy() %>%
    magrittr::set_colnames(c("parameter",
                             "estimate",
                             "std_error",
                             "t_statistic",
                             "p_value"))

# Use ARIMA ---------------------------------------------------------------
fitted_values <- number_of_births - sarima_model$fit$residuals

tibble(
    actual = number_of_births,
    arima_prediction = fitted_values
) %>%
    mutate(time = row_number()) %>%
    ggplot(aes(x = time, y = actual)) + 
    geom_line() + 
    geom_line(aes(x = time, y = arima_prediction),
              inherit.aes = FALSE,
              col = "red")
