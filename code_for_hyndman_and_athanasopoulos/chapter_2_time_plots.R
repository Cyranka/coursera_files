remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)

# Create a time-series object ---------------------------------------------
y <- ts(c(123,39,78,52,110),start = 2012)

# Creating a monthly time-series ------------------------------------------
set.seed(120)
z <- rnorm(120)

y1 <- ts(z, start = 2003, frequency = 12) ##Creates monthly data

# Time plots --------------------------------------------------------------
#a) Economy class passengers
autoplot(melsyd[,"Economy.Class"]) + 
    labs(title = "Economy class passengers: Melbourne to Sydney",
         x = "Year",
         y = "Thousands") 

#b) Antidiabetic drug sales
autoplot(a10) + 
    ggtitle("Antidiabetic drug sales") + 
    ylab("$ million") + 
    xlab("Year")

# Season plots ------------------------------------------------------------
ggseasonplot(a10,
             year.labels = TRUE,
             year.labels.left = TRUE) + 
    ylab("$ million") + 
    ggtitle("Seasonal plot: antidiabetic drug sales")

#Create polar plot
ggseasonplot(a10,polar = TRUE,
             year.labels = TRUE,
             year.labels.left = TRUE) + 
    ylab("$ million") + 
    ggtitle("Seasonal plot: antidiabetic drug sales")

# Seasonal subseries plots ------------------------------------------------
ggsubseriesplot(a10) + 
    ylab("$ million") + 
    ggtitle("Seasonal subseries plot: antidiabetic drug sales")


# Scatterplots ------------------------------------------------------------
qplot(Temperature, Demand,
      data = as.data.frame(elecdemand)) + 
    ylab("Demand (GW)") + 
    xlab("Temperature (Celsius)")

# Scatterplot matrices ----------------------------------------------------
autoplot(visnights[,1:5], facets = TRUE) + 
    ylab("Number of visitor nights each quarter (millions)")


# Pairs plot --------------------------------------------------------------
#Useful for displaying relationships between multiple variables
GGally::ggpairs(as.data.frame(visnights[,1:5]))


# Lag plots ---------------------------------------------------------------
beer2 <- window(ausbeer, start = 1992)
gglagplot(beer2)

# Correlogram -------------------------------------------------------------
ggAcf(beer2)

# White noise: time-series with no autocorrelation ------------------------
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + 
    ggtitle("White noise series")

wn_acf <- ggAcf(y)
wn_acf