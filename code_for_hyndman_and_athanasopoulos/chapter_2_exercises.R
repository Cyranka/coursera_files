remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)

# Chapter 2 - Exercises ---------------------------------------------------

# Question 1 --------------------------------------------------------------
#a) Plotting series
autoplot(gold) + 
    labs(title = "Daily morning gold prices in US$",
         y = "US$",
         x = "Day")

autoplot(woolyrnq) + 
    labs(title = "Quarterly production of woollen yarn in Australia",
         y = "Tonnes",
         x = "Quarter")

autoplot(gas) + 
    labs(title = "Australian monthly gas production",
         y = "Total",
         x = "Month")


#b) Find the frequency of each series
frequency(gold);frequency(woolyrnq);frequency(gas)

#c) Outlier in the gold series
max(gold, na.rm = TRUE) ##Value at peak
which(gold == max(gold, na.rm = TRUE)) ##Position value: 770 observation


# Question 2 --------------------------------------------------------------
#a) Read the file into R
tute1 <- read_csv("tute1.csv") %>%
    rename(quarter = X1)

#b) Transform into a time-series object
mytimeseries <- ts(tute1[,-1], start = 1981, frequency = 4) ##Quarterly data

#c) Construct time series plot of each series
autoplot(mytimeseries, facets = TRUE)

# Question 3: Australian retail data --------------------------------------
#a) Read the data
retaildata <- readxl::read_excel("retail.xlsx", skip = 1)

#b) Subset the data and create a time-series object
my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

#c) Explore the data
autoplot(my_retail_series) + 
    labs(title = "My retail series",
         y = "Total",
         x = "Month") #Basic time-series plot

ggseasonplot(my_retail_series) ##Seasonal plot
ggsubseriesplot(my_retail_series) + 
    labs(title = "My retail series", 
         y = "Total", 
         x = "Month")


# Question 4 --------------------------------------------------------------
#Create time plots
autoplot(bicoal) + 
    labs(title = "Annual bituminous coal production in the USA",
         x = "Year",
         y = "Total")

autoplot(chicken) + 
    labs(title = "Price of chicken in US$: 1924-1993",
         x = "Year", y = "US$")

autoplot(goog) + 
    labs(title = "Closing stock prices of GOOG",
         x = "Day",
         y = "US$")

# Question 5 --------------------------------------------------------------
#i) For writing series
autoplot(writing)
ggseasonplot(writing)
ggsubseriesplot(writing) + 
    labs(title = "Industry sales for printing and writing paper")

#ii) For fancy series
autoplot(fancy) + 
    labs(title = "Monthly sales for a souvenir shop")
ggseasonplot(fancy) ##Much clearer seasonal pattern in december and march
ggsubseriesplot(fancy) + 
    labs(title = "Monthly sales for a souvenir shop")

#iii) For a10 series
autoplot(a10) + 
    labs(title = "Monthly scripts for Pharma products under ATC code 10",
         y = "Total", 
         x = "Month")
ggseasonplot(a10)
ggsubseriesplot(a10) + 
    labs(title = "Monthly scripts for Pharma products under ATC code 10")

# Question 6 --------------------------------------------------------------
#i) For hsales data
autoplot(hsales) + 
    labs(title = "Monthly sales of new one-family houses sold in the USA",
         x = "Month",
         y = "Total")
ggseasonplot(hsales)
ggAcf(hsales)

#ii) For sunspotarea
autoplot(sunspotarea) + 
    labs(title = "Annual averages of daily sunspot areas",
         y = "Total", x = "Year") 
ggseasonplot(sunspotarea) ##Data is not seasonal
ggAcf(sunspotarea, lag.max = 40)


# Question 7: Arrivals dataset --------------------------------------------
autoplot(arrivals, facets = TRUE,colour = TRUE)

ggseasonplot(arrivals[,"NZ"])

# Question 9: Pigs dataset ------------------------------------------------
my_pigs <- window(pigs, start = 1990) ##Window serves to cut time-series
autoplot(my_pigs) + 
    labs(title = "Monthly pigs slaughtered in Victoria, Australia",
         y = "Total", x = "Date") +
    scale_y_continuous(labels = scales::comma) ##Seems to be trending high
ggAcf(my_pigs) + 
    labs(title = "ACF for Pig data")

# Question 10: 292 of consecutive trading ---------------------------------
ddj <- diff(dj) ##First differencing of dow jones data

autoplot(ddj) + 
    labs(title = "First differencing of Dow Jones Index",
         y = "", x = "Date")

ggAcf(ddj) + 
    labs(title = "First differences of Dow Jones Index")

