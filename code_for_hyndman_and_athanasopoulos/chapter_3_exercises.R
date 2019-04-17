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