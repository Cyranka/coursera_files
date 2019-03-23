remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/applied_time_series/")
library(tidyverse);library(astsa)

# Applied time-series analysis week 5 -------------------------------------
x <- BJsales

# Plot the data -----------------------------------------------------------
plot(BJsales, main = "Box and Jenkins sales example",
     ylab = "Sales", lwd = 0.8, col = "blue")

# Take first difference and plot the data ---------------------------------
plot(diff(BJsales), main = "Differenced time-series",
     ylab = "", lwd = 0.8, col ="red")

# Take a new difference (d = 2) and plot the series -----------------------
plot(diff(diff(BJsales)), main = "2-lag differenced time-series",
     ylab = "", lwd = 0.8, col ="forestgreen")

# Determining AR(p) component: PACF ---------------------------------------
pacf_d2 <- pacf(diff(diff(BJsales)),lag.max = 50,plot = TRUE)

# Determining the MA(q) component: ACF ------------------------------------
acf_d2 <- acf(diff(diff(BJsales)),lag.max = 50,plot = TRUE)

# Try different ARIMA (p,2,q) models and check AIC --------------------------------------
d=2
for(p in 1:4){
    for(q in 1:2){
        if(p+d+q<=6){
            model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
            pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
            sse<-sum(model$residuals^2)
            cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
    }
}

# Try different ARIMA (p,2,q) models and check SSE --------------------------------------
d=2
for(p in 1:4){
    for(q in 1:2){
        if(p+d+q<=8){
            model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
            pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
            sse<-sum(model$residuals^2)
            cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
    }
}

# Look at residuals of model with smallest AIC ----------------------------
model<-arima(BJsales, order=c(0,2,1))
par(mfrow=c(2,2))

plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)

# Retrieve coefficients ---------------------------------------------------
dev.off()
par(mfrow = c(1,1))


model$sigma2
model$coef %>% broom::tidy()

# Fitted values -----------------------------------------------------------
fitted_values <- (BJsales - model$residuals)

tibble(
    time = 1:150,
    actual = as.numeric(BJsales),
    fitted = as.numeric(fitted_values)
) %>%
    gather(type, value, - time) %>%
    ggplot(aes(x = time, y = value, color = type)) + 
    geom_line(size = 0.5) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    ) + 
    guides(
        color = guide_legend(title = "Series", 
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             keywidth = 3)) + 
    labs(x = "Time", y = "Value",
         title = "Fit of a ARIMA (0,2,1) model on the BJSales time-series")
