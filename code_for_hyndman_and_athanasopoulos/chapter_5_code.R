remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)


# Regression model --------------------------------------------------------
##Consumption and income plot
autoplot(uschange[,c("Consumption", "Income")]) + 
    ylab("% change") + xlab("Year")

##Scatterplot
uschange %>%
    as.data.frame() %>%
    ggplot(aes(x = Income, y = Consumption)) + 
    ylab("Consumption (quarterly % change)") + 
    xlab("Income (quarterly % change)") + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE)

##Fitted model
tslm(Consumption ~ Income, data = uschange)

# Multiple regression model -----------------------------------------------
#time-series
autoplot(uschange[,3:5], facets = TRUE,colour = TRUE,show.legend = FALSE) 

##GGpairs
uschange %>%
    as.data.frame() %>% 
    GGally::ggpairs()

##Fit model to the data.
fit_consMR <- tslm(
    Consumption ~ Income + Production + Unemployment + Savings,
    data = uschange
)

summary(fit_consMR)

##Plot actual series and fitted values
autoplot(uschange[,"Consumption"], series = "Data") + 
    autolayer(fitted(fit_consMR), series = "Fitted") + 
    xlab("Year") + ylab("") + 
    ggtitle("Percent change in US consumption expenditure")

cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit_consMR)) %>%
    as.data.frame() %>%
    ggplot(aes(x = Data, y = Fitted)) + 
    geom_point() + 
    xlab("Fitted (predicted values)") + 
    ylab("Data (actual values)") + 
    ggtitle("Percent change in US consumption expenditure") + 
    geom_abline(intercept = 0, slope = 1)