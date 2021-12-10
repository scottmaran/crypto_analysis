#
# File running SARIMA models on Bitcoin target return time series
#

ls()
rm(list = ls())
setwd("~/crypto_analysis")

library("readr")
library("dplyr")
library(stringr)
library(zoo)
library("astsa")
library("xts")

plot_bool = FALSE

# Read in supplemental train dataset, add POSIX time
supp_train <- read_delim('supplemental_train.csv', ',', col_names=TRUE)
supp_train['posix'] <- as.POSIXct(supp_train$timestamp, origin="1970-01-01 00:00:00.000", tz="UTC")

# Read in main training csv
train <- read_delim('train.csv', ',', col_names=TRUE)
train['posix'] <- as.POSIXct(train$timestamp, origin="1970-01-01 00:00:00.000", tz="UTC")

# Inspect/Create model for Bitcoin
bitcoin_df <- train[train$Asset_ID == "1",]

bitcoin_target.ts = zoo(x=bitcoin_df$Target, order.by=bitcoin_df$posix)
bitindex = index(bitcoin_target.ts)
startDate = bitindex[1]
endDate =  bitindex[length(bitindex)]
minSeq = seq(from=startDate, to=endDate, by="1 min")
# approximate any missing dates
bitcoin_approx.ts = na.approx(bitcoin_target.ts, x = bitindex, xout= minSeq)
#time series is now regular
old_bool <- is.regular(bitcoin_target.ts, strict=TRUE)
assert(is.regular(bitcoin_approx.ts, strict=TRUE) == TRUE)

#########################
####### Plot different series to look at potential models 
#########################
plot(bitcoin_approx.ts)
plot(window(bitcoin_approx.ts, start=as.POSIXct("2018-01-01 00:00:00"), end=as.POSIXct("2018-12-31 0:0:0")))
plot(window(bitcoin_approx.ts, start=as.POSIXct("2018-01-01 00:00:00"), end=as.POSIXct("2018-03-01 0:0:0")))
plot(window(bitcoin_approx.ts, start=as.POSIXct("2018-01-01 00:00:00"), end=as.POSIXct("2018-01-07 0:0:0")))

# looking at bitcoin plots
diff_1 = diff(bitcoin_approx.ts)
if(plot_bool){
  plot(diff_1)  #plotting entire series will take a while
  plot(window(diff_1, start=as.POSIXct("2018-01-01 00:00:00"), end=as.POSIXct("2018-01-07 0:0:0")))
}

diff_15 = diff(bitcoin_approx.ts, 15)
if(plot_bool){
  plot(diff_15)
  plot(window(diff_15, start=as.POSIXct("2018-01-01 00:00:00"), end=as.POSIXct("2018-01-07 0:0:0")))
}
##########################

# define training/test time series
training.ts <- window(bitcoin_approx.ts, start=as.POSIXct("2018-01-01 00:00:00"), end=as.POSIXct("2021-01-01 0:0:0"))
test.ts <- window(bitcoin_approx.ts, start=as.POSIXct("2021-01-01 0:0:0"))

##############################
##### ACF/PACF plots
##############################
# lags at intervals of 60, because observations every 60 seconds, in training.ts
# change index so lags are 1-h
pacf.ts <- training.ts
index(pacf.ts) <- (1:length(pacf.ts))

bitcoin_acf_plot <- acf(pacf.ts, 40)
bitcoin_pacf_plot <- pacf(pacf.ts, 60)
###############################


###############################
#### Fit models
###############################
# ARIMA(3,0,0) x (0,0,1)_15 - no differencing in this model
ar3 <- sarima(training.ts, 3,0,0,0,0,1, 15)

num_out = 30
sarima.for(training.ts, num_out, 3,0,0,0,0,1, 15)
lines(index(test.ts), coredata(test.ts), col="blue")


ar_diff <- sarima(training.ts, 3,1,0,0,1,1, 15)
ar_diff_for <- sarima.for(training.ts, num_out, 3,1,0,0,1,1, 15)
lines(index(test.ts), coredata(test.ts), col="blue")

ar4 <- sarima(training.ts, 4,0,0,0,0,1, 15)
ar4_for <- sarima.for(training.ts, num_out, 4,0,0,0,0,1, 15)
lines(index(test.ts), coredata(test.ts), col="blue")
