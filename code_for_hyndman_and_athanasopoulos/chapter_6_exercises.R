remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2);library(seasonal);library(lubridate)


# Question 6.2: Analysis of plastics dataset ------------------------------
#a)Plotting the data
autoplot(plastics) + 
    xlab("Month") + 
    ylab("Total sales") + 
    ggtitle("Monthly sales of product A for a plastics manufacturer")

ggseasonplot(plastics,
             year.labels = TRUE,
             year.labels.left = TRUE) + 
    ylab("Total sales") + 
    ggtitle("Seasonal plot: monthly sales of product A for a plastics manufacturer")

#b)Using classical multiplicative decomposition
classical_plastics <- plastics %>% decompose(type = "multiplicative") 

seasonal(classical_plastics) ##Seasonal indices
trendcycle(classical_plastics) ##Extract trend-cycle component

autoplot(classical_plastics) + 
    ggtitle("Multiplicative classical decomposition of the plastics series")

#c) Yes
#d) Computing seasonally adjusted data
autoplot(plastics, series = "Original") + 
    autolayer(seasadj(classical_plastics), series = "Seasonally adjusted series")+ 
    scale_color_manual(
        values = c("gray","blue"),
        breaks = c("Original", "Seasonally adjusted series")
    ) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    ) + guides(color = guide_legend(title.position = "top",
                                    title.hjust = 0.5))

#e) Add an outlier to the series
set.seed(3)
random_position <- sample(1:60,1)

plastics[11] <- plastics[11] + 500 ##Change 

classical_plastic_outlier <- plastics %>% decompose(type = "multiplicative") 

autoplot(classical_plastic_outlier) +  ##Just plotting
    ggtitle("Multiplicative classical decomposition of the plastics series with outlier")

autoplot(plastics, series = "Original") + 
    autolayer(seasadj(classical_plastic_outlier), series = "Seasonally adjusted series with outlier")+ 
    scale_color_manual(
        values = c("gray","blue"),
        breaks = c("Original", "Seasonally adjusted series with outlier")
    ) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    ) + guides(color = guide_legend(title.position = "top",
                                    title.hjust = 0.5))

##Create an outlier in the end of the series
rm(plastics)

plastics[59] <- plastics[59] + 500
classical_plastic_outlier_end <- plastics %>% decompose(type = "multiplicative") 


autoplot(plastics, series = "Original") + 
    autolayer(seasadj(classical_plastic_outlier), series = "Seasonally adjusted series with outlier")+ 
    scale_color_manual(
        values = c("gray","blue"),
        breaks = c("Original", "Seasonally adjusted series with outlier 2")
    ) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    ) + guides(color = guide_legend(title.position = "top",
                                    title.hjust = 0.5))


# Question 3: retail data --------------------------------------------------------------
rm(list = ls())

retaildata <- readxl::read_excel("retail.xlsx", skip = 1)
my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4)) %>%
    window(start = c(1983,1)) ##So it starts in January and ends in December

fit_x11 <- my_retail_series %>%
    seas(x11 = "")

autoplot(fit_x11) + 
    ggtitle("X11 decomposition of retail data") ##notice how it doesn't oversmooth

fit_x11 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

seasonal(fit_x11) %>% as_tibble() %>%
    mutate(date = ymd(seq.Date(from = ymd("1983-01-01"),
                    to = ymd("2013-12-01"),by = "month"))) %>%
    rename(retail = 1) %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(sd = sd(retail)) %>%
    arrange(desc(sd)) ##Obtain standard deviation of seasonal component
