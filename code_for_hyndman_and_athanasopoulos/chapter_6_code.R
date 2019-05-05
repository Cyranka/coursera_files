remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal)

# Section 6.2 moving averages ---------------------------------------------
autoplot(elecsales) + 
    xlab("Year") + 
    ylab("GWh") + 
    ggtitle("Annual electricity sales: South Australia")

#Computing an MA 5 series 
ma(elecsales,5) ##Notice that the window is 2-sided

#Plotting the original series along with a 5-MA
autoplot(elecsales, series = "Data") + 
    autolayer(ma(elecsales,5), series = "5-MA") + 
    xlab("Year") + ylab("GWh") + 
    ggtitle("Annual electricity sales: South Australia") +
    scale_color_manual(values = c("Data" = "grey50", "5-MA" = "red"),
                       breaks = c("Data", "5-MA"))

#Moving average of a moving average
beer2 <- window(ausbeer, start = c(1992,1))
ma4 <- ma(beer2, order = 4, centre = FALSE) ##
ma2x4 <- ma(beer2, order = 4, centre = TRUE) ##Centering the time-series

#Using moving averages to estimate the trend-cycle
autoplot(elecequip, series = "Data") + 
    autolayer(ma(elecequip,12, centre = TRUE), series = "12-MA") + 
    xlab("Year") + ylab("New orders index") + 
    ggtitle("Electrical equipment manufacturing (Euro area)") + 
    scale_colour_manual(
        values = c("Data" = "grey", "12-MA" = "red"),
        breaks = c("Data", "12-MA")
        )

# Classical decomposition -------------------------------------------------
elecequip %>% decompose(type = "multiplicative") %>%
    autoplot() + xlab("Year") + 
    ggtitle("Classical multiplicative decomposition of electrical equipment index")

elecequip %>% decompose(type = "additive") %>%
    autoplot() + xlab("Year") + 
    ggtitle("Classical multiplicative decomposition of electrical equipment index")


# X11 decomposition -------------------------------------------------------
fit_x11 <- elecequip %>%
    seas(x11 = "")
autoplot(fit_x11) + 
    ggtitle("X11 decomposition of electrical equipment index") ##notice how it doesn't oversmooth

seasonal(fit_x11) ##Extract seasonal component
trendcycle(fit_x11) ##Extract trend-cycle component
remainder(fit_x11) ##Extract remainder component
seasadj(fit_x11) ##Extract seasonally-adjusted series

#plotting the series
autoplot(elecequip, series = "Data") + 
    autolayer(trendcycle(fit_x11), series = "Trend") + 
    autolayer(seasadj(fit_x11), series = "Seasonally adjusted") + 
    xlab("Year") + ylab("New orders index") + 
    ggtitle("Electrical equipment manufacturing (Euro area)") + 
    scale_color_manual(
        values = c("gray","blue","red"),
        breaks = c("Data", "Seasonally adjusted", "Trend")
            ) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    )

fit_x11 %>%
    seasonal() %>%
    ggsubseriesplot() + ylab("Seasonal")

# SEATS decomposition -------------------------------------------------------
elecequip %>% seas() %>%
    autoplot() + 
    ggtitle("SEATS decomposition of electrical equipment index")

# STL decomposition -------------------------------------------------------
elecequip %>%
    stl(t.window = 13, s.window = "periodic", robust = TRUE) %>%
    autoplot()

fit_stl <- stl(elecequip, t.window = 13, s.window = "periodic",
               robust = TRUE)

fit_stl %>% seasadj() %>% naive() %>%
    autoplot() + ylab("New orders index") + 
    ggtitle("Naive forecasts of seasonally adjusted data") ##Forecasting seasonally adjusted pattern
                                                           ##Naive method

fit_stl %>% forecast(method = "naive") %>%
    autoplot() + 
    ylab("New orders index")
