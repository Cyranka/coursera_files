remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/code_for_hyndman_and_athanasopoulos/")
library(tidyverse);library(fpp2)

# Question 1: Find appropriate box-cox parameter --------------------------

# a) Usnetelec series -----------------------------------------------------
lambda_usnetelec <- BoxCox.lambda(usnetelec) ##
usnetelec_bc <- BoxCox(usnetelec, lambda_usnetelec) ##Transformed usnetelec

dframe1 <- cbind(
    Regular = usnetelec,
    Transformed_usnetelec = usnetelec_bc
)
autoplot(dframe1, facet = TRUE)+
    xlab("Time") + ylab("Billion KWH") + 
    ggtitle("Annual US net electricity generation") ##Autoplot

var(usnetelec);var(usnetelec_bc) ##Notice how the variance shrinks

# USGDP series -------------------------------------------------------------------
lambda_usgdp <- BoxCox.lambda(usgdp)
usgdp_bc <- BoxCox(usgdp, lambda_usgdp) ##Transformed USGDP

dframe2 <- cbind(
    gdp = usgdp,
    box_cox_gdp = usgdp_bc
)

autoplot(dframe2, facet = TRUE)+
    xlab("Time") + ylab("") + 
    ggtitle("Quarterly US GDP") ##Autoplot

var(usgdp);var(usgdp_bc) ##Notice how the variance shrinks

# Copper series -----------------------------------------------------------
lambda_copper <- BoxCox.lambda(copper)
copper_bc <- BoxCox(copper, lambda_copper) ##Transformed Copper

dframe3 <- cbind(
    copper = copper,
    copper_bc = copper_bc
)

autoplot(dframe3, facet = TRUE)+
    xlab("Time") + ylab("") + 
    ggtitle("Yearly copper prices") ##Autoplot

var(copper);var(copper_bc)

# Enplanements ------------------------------------------------------------
lambda_enplanements <- BoxCox.lambda(enplanements)
enplanements_bc <- BoxCox(enplanements, lambda_enplanements)

dframe4 <- cbind(
    enplanements,
    enplanements_bc
)

autoplot(dframe4, facet = TRUE)+
    xlab("Time") + ylab("") + 
    ggtitle("Monthly domestic enplanements") ##Autoplot

var(enplanements);var(enplanements_bc)

# Question 2: Cangas dataset and box-cox ----------------------------------
autoplot(cangas) + 
    xlab("Time") + ylab("Billions of cubic metres") + 
    ggtitle("Monthly Canadian gas production")

lambda_cangas <- BoxCox.lambda(cangas)
cangas_bc <- BoxCox(cangas, lambda_cangas)

dframe5 <- cbind(
    cangas,
    cangas_bc
)
autoplot(dframe5, facet = TRUE)+
    xlab("Time") + ylab("") + 
    ggtitle("Monthly Canadian gas production") ##Autoplot

var(cangas);var(cangas_bc)

# Question 3: Box-Cox for retail data -------------------------------------
remove(list = ls())
retaildata <- readxl::read_excel("retail.xlsx", skip = 1)

my_retail_series <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4))

autoplot(my_retail_series) ##Notice how the seasonal variation increases with time
retail_lambda <- BoxCox.lambda(my_retail_series)
retail_bc <- BoxCox(my_retail_series, retail_lambda)

dframe_retail <- cbind(
    my_retail_series,
    retail_bc
)
autoplot(dframe_retail, facet = TRUE)+
    xlab("Time") + ylab("") + 
    ggtitle("Retail series")

# Question 4 --------------------------------------------------------------
remove(list = ls())

# Dole series -------------------------------------------------------------
autoplot(dole) + 
    ggtitle("Monthly total people on unemployment benefits in Australia")

autoplot(BoxCox(dole, BoxCox.lambda(dole))) ##Box-cox transform
autoplot(log(dole)) ##Log transform

# US deaths series --------------------------------------------------------
autoplot(usdeaths)
autoplot(BoxCox(usdeaths,BoxCox.lambda(usdeaths)))

# Bricksq series ----------------------------------------------------------
autoplot(bricksq)
autoplot(BoxCox(bricksq,BoxCox.lambda(bricksq)))

# Question 5: Checking the residuals of ausbeer data ----------------------
rm(list = ls())
beer <- window(ausbeer, start=1992) ##Subset the time-series
fc <- snaive(beer) ##Seasonal-naive forecasting

autoplot(fc) ##Plotting the forecasting
res <- residuals(fc) ##Obtain the residuals of naive forecasting
autoplot(res) ##Autoplot of residuals
checkresiduals(fc)

# Question 6: residual checking -------------------------------------------
rm(list =ls())
# Residual checking for WWusage -------------------------------------------
help("WWWusage")

autoplot(WWWusage) ##Data appears to follow a random-walk
naive_wwusage <- naive(WWWusage)
autoplot(naive_wwusage) ##Notice how forecasts are equal to the last value of the series
res <- residuals(naive_wwusage)
checkresiduals(naive_wwusage) ##You pass the model, not the residuals
mean(res ,na.rm = TRUE)

# Residual checking for bricksq -------------------------------------------
autoplot(bricksq) ##
snaive_bricksq <- snaive(bricksq)
autoplot(snaive_bricksq)
checkresiduals(snaive_bricksq)
checkresiduals(naive(bricksq))

# Question 8: retail time-series analysis ---------------------------------
remove(list = ls())

retaildata <- readxl::read_excel("retail.xlsx", skip = 1)

myts <- retaildata %>% pull(A3349335T) %>%
    ts(frequency = 12,start = c(1982,4)) ##Create a time-series object

# Split the data into test and train --------------------------------------
myts_train <- window(myts, end = c(2010,12))
myts_test <- window(myts, start = 2011)

##Check split
autoplot(myts) +
    autolayer(myts_train, series="Training") +
    autolayer(myts_test, series="Test")

##Calculate forecasts from snaive
forecast_snaive <- snaive(myts_train) ##Calculate for next two years

#Check accuracy of seasonal naive method
accuracy(forecast_snaive,myts_test) ##Accuracy of forecasts

checkresiduals(forecast_snaive)

# Question 9: Visnights data ----------------------------------------------------------
rm(list= ls())

#Create datasets
train1 <- window(visnights[, "QLDMetro"], end = c(2015, 4)) ##Omits 2016
train2 <- window(visnights[, "QLDMetro"], end = c(2014, 4)) ##Omits 2015/2016
train3 <- window(visnights[, "QLDMetro"], end = c(2013, 4)) ##Omits 2014/2015/2016

fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

##Check 
model_1 <- accuracy(fc1,window(visnights[, "QLDMetro"],
                       start = c(2016,1),
                       end = c(2016, 4))) %>% 
    broom::tidy() %>%
    select(MAPE)

model_2 <- accuracy(fc2,window(visnights[, "QLDMetro"],
                               start = c(2015,1),
                               end = c(2015, 4))) %>% 
    broom::tidy() %>%
    select(MAPE)

model_3 <- accuracy(fc3,window(visnights[, "QLDMetro"],
                               start = c(2014,1),
                               end = c(2014, 4))) %>% 
    broom::tidy() %>%
    select(MAPE)


bind_rows(model_1, model_2, model_3) %>%
    mutate(dataset = rep(c("Train", "Test"),3),
           number = rep(1:3,each =2)) %>%
    spread(dataset, MAPE)

# Question 10: Forecasting on the DowJones dataset ------------------------
rm(list =ls()) ##Daily data

#Plot the data
autoplot(dowjones) +
    labs(x = "Time",y = "Index", title = "Dow-Jones Index (Daily)")

#Produce forecasts using drift method
drift_forecasts <- rwf(dowjones,13,drift = TRUE)

autoplot(dowjones) + 
    autolayer(drift_forecasts) + 
    labs(x = "Time",y = "Index", title = "Dow-Jones Index with forecasts")

slope <- (121.23 - 110.94)/(77)
forecast_horizon <- 1:13
dowjones[78] + slope*forecast_horizon

##Window the series and use cross-validation
train_dj <- window(dowjones, end = 65)
test_dj <- window(dowjones, start = 66)

# Try naive ----------------------------------------------------
model_1 <- naive(train_dj,h = 13) ## No drift
model_2 <- rwf(train_dj, h = 13, drift = TRUE) ##With drift

accuracy(model_1, test_dj)
accuracy(model_2, test_dj)

autoplot(dowjones) + 
    autolayer(model_1, series = "Naive", PI = FALSE) + 
    autolayer(model_2, series = "Drift", PI = FALSE)

# Question 11: Closing IBM Stock Prices -----------------------------------
rm(list = ls())

autoplot(ibmclose) + 
    labs(tile = "Daily closing IBM stock price")

#Split data
train_ibm <- window(ibmclose, end = 300)
test_ibm <- window(ibmclose, start = 301)

# Forecast methods --------------------------------------------------------
model_1 <- naive(train_ibm,h = 69) ##Naive method (equal to last)
model_2 <- rwf(train_ibm, h = 69, drift = TRUE) ##Drift
model_3 <- meanf(train_ibm, h = 69) ##Mean forecast


autoplot(ibmclose) + 
    autolayer(model_1, series = "Naive", PI = FALSE) + 
    autolayer(model_2, series = "Drift", PI = FALSE) + 
    autolayer(model_3, series = "Mean", PI = FALSE) + 
    labs(tile = "Daily closing IBM stock price")

##Check metrics
check_1 <- accuracy(model_1, test_ibm) %>% 
    broom::tidy() %>%
    select(MAPE)

check_2 <- accuracy(model_2, test_ibm) %>% 
    broom::tidy() %>%
    select(MAPE)

check_3 <- accuracy(model_3, test_ibm) %>% 
    broom::tidy() %>%
    select(MAPE)

bind_rows(check_1, check_2, check_3) %>%
    mutate(dataset = rep(c("Train", "Test"),3),
           number = rep(1:3,each =2)) %>%
    spread(dataset, MAPE)

checkresiduals(model_2)