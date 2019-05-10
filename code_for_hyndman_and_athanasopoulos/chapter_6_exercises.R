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

# Question 4: notebook --------------------------------------------------------------

# Question 5: cangas dataset ----------------------------------------------
rm(list = ls())

#a) Plot the data
autoplot(cangas, series = "Monthly Canadian gas production",
         ylab = "Billions of cubic metres",
         x = "Month") + 
    scale_color_manual(values = "steelblue") 

ggsubseriesplot(cangas)
ggseasonplot(cangas)

#b) Perform stl decomposition
stl_1 <- cangas %>%
    stl(t.window = 13, s.window = 7, robust = TRUE) ##Seven year window

autoplot(stl_1)

#c) Perform seats
cangas %>% seas() %>%
    autoplot() + 
    ggtitle("SEATS decomposition of cangas dataset")

#c) Perform x11
cangas %>% seas(x11 = "") %>%
    autoplot() + 
    ggtitle("X11 decomposition of cangas dataset")

seasonal(stl_1)


# Question 6 --------------------------------------------------------------
rm(list = ls())
dev.off()

#a) Perform stl decomposition
stl_bricksq <- bricksq %>% stl(t.window = 5, s.window = 7)

#b)Plot seasonally adjusted data
autoplot(bricksq, series = "Data") + 
    autolayer(seasadj(stl_bricksq), series = "Seasonally adjusted") + 
    scale_color_manual(
        values = c("gray","blue"),
        breaks = c("Data", "Seasonally adjusted")
    ) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    )


#c) Producing naive forecasts of seasonally adjusted data
naive_forecasts <- seasadj(stl_bricksq) %>% naive(h = 9) ##until Q4 1996
autoplot(naive_forecasts)
snaive_forecasts <- seasadj(stl_bricksq) %>% snaive(h = 9)##until Q4 1996
autoplot(snaive_forecasts)

#d) Reseasonalize the forecasts
brick_forecast <- stlf(seasadj(stl_bricksq), h = 9) ##obtain seasonally adjusted data and make forecasts
autoplot(brick_forecast) ##

#e) Check residuals
checkresiduals(brick_forecast)
checkresiduals(naive_forecasts)

#f) roboust stl decomposition
stl_bricksq_robust <- bricksq %>% stl(t.window = 5, s.window = 7,robust = TRUE)
stlf_robust_seasonal <- seasadj(stl_bricksq_robust)

robust_predictions <- stlf(stlf_robust_seasonal)
autoplot(robust_predictions)

#g) Compare forecasts using last two years of data
rm(list = ls())
brick_train <- window(bricksq, end = c(1991,4))
brick_test <- window(bricksq, start = c(1992,1))

#Start with robust decomposition
stl_bricksq_robust <- brick_train %>% stl(t.window = 5, s.window = 7,robust = TRUE)

#Get seasonally adjusted data
seasonally_adjusted_data <- seasadj(stl_bricksq_robust)

#Get forecasts
forecasts_stlf <- stlf(seasonally_adjusted_data, h = 11)
forecasts_snaive <- snaive(seasonally_adjusted_data, h = 11)

accuracy(forecasts_stlf, brick_test) ##Forecast metrics for STLF
accuracy(forecasts_snaive, brick_test) ##Forecast metrics for naive

cowplot::plot_grid(autoplot(forecasts_stlf),
                   autoplot(forecasts_snaive),nrow = 2)

##Plot forecasts with actual data
autoplot(bricksq, alpha =0.5) + 
    autolayer(forecasts_stlf, series = "STLF", PI = FALSE) + 
    autolayer(forecasts_snaive, series = "Snaive", PI = FALSE) 
    

# Question 7 --------------------------------------------------------------
rm(list = ls())

#Plot the writing series
autoplot(writing) + 
    labs(x = "Year", y = "Sales", 
         title = "Industry sales for printing and writing paper") + 
    theme_minimal()

writing_train <- window(writing, end = c(1975,12))
writing_test <- window(writing, start = c(1976,1))

#STFL decomposition for non-transformed series
stl_writing <- stl(writing_train,t.window = 13, s.window = 7,robust = TRUE)
autoplot(stl_writing) ##Check the decomposition

#Obtain seasonally adjusted data of series
writing_stl_seasadj <- seasadj(stl_writing)
autoplot(writing_train, series = "Data") + 
    autolayer(writing_stl_seasadj, series = "Seasonally adjusted") + 
    scale_color_manual(
        values = c("gray","blue"),
        breaks = c("Data", "Seasonally adjusted")
    )

#Produce forecasts
naive_forecasts <- stlf(writing_stl_seasadj,h = 24, method = "naive") #naive method
rwdrift_forecasts <- stlf(writing_stl_seasadj,h = 24, method = "rwdrift") ##Random-walk with drift

accuracy(naive_forecasts, writing_test)
accuracy(rwdrift_forecasts, writing_test)

autoplot(writing) + 
    autolayer(naive_forecasts, PI = FALSE, series = "Naive forecasts") + 
    autolayer(rwdrift_forecasts, PI = FALSE, series = "RWdrift forecasts")


# Question 8 --------------------------------------------------------------
rm(list = ls())

#Plot the fancy series
autoplot(fancy) + 
    labs(x = "Year", y = "Sales", 
         title = "Sales for a souvenir shop") + 
    theme_minimal()

#Obtain box-cox transform
bc_lambda <- BoxCox.lambda(fancy)
bc_fancy <- BoxCox(fancy, bc_lambda)

autoplot(stl(bc_fancy, t.window = 13, s.window = 7,robust = TRUE))

#
bc_fancy_stl_seasadj <- seasadj(stl(bc_fancy, t.window = 13, s.window = 7,robust = TRUE))
autoplot(BoxCox(fancy, bc_lambda), series = "Data (Box-Cox transfored)") + 
    autolayer(bc_fancy_stl_seasadj, series = "Seasonally adjusted") + 
    scale_color_manual(
        values = c("gray","blue"),
        breaks = c("Data", "Seasonally adjusted")
    )

naive_forecasts <- stlf(bc_fancy_stl_seasadj,h = 12, method = "naive") #naive method
rwdrift_forecasts <- stlf(bc_fancy_stl_seasadj,h = 12, method = "rwdrift") ##Random-walk with drift

naive_forecasts
rwdrift_forecasts