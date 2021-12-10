ls()
rm(list = ls())

#########################
##### Correlation stats
#########################

library("readr")
library("xts")
library("moments")

setwd("crypto_analysis")

asset_details <- read.csv("asset_details.csv")
asset_names = asset_details[order(asset_details$Asset_ID),]$Asset_Name

train <- read_delim('train.csv', ',', col_names=TRUE)
train['posix'] <- as.POSIXct(train$timestamp, origin="1970-01-01 00:00:00.000", tz="UTC")

# Creates a time series for an individual currency
create_currency_ts <- function(df, asset_id) {
  df_subset <- df[df$Asset_ID == asset_id,]
  df_target.ts = zoo(x=df_subset$Target, order.by=df_subset$posix)
  
  return(df_target.ts)
}

# creates the time series with all the currencies
create_full_dataset <- function(train) {
  binance_df <- train[train$Asset_ID == "0",]
  binance_target.ts = zoo(x=binance_df$Target, order.by=binance_df$posix)
  
  merged = binance_target.ts
  names <- c('0')
  
  for (asset_id in c(1:13)) {
    new_ts <- create_currency_ts(train, asset_id)
    merged <- merge.zoo(merged, new_ts, all=TRUE, fill=NA, suffixes = c(names, asset_id))
  }
  
  return(merged)
}

# Get the asset id for a given name
get_asset_id <- function(assetName) {
  asset_details <- read.csv("asset_details.csv")
  return(asset_details[asset_details$Asset_Name == assetName, 'Asset_ID'])
}

get_regular_time_series <- function(list_names_to_include) {

  asset_ids <- paste(sapply(list_names_to_include, get_asset_id))
  exclude_low_samples <- merged[, asset_ids]
  trim_ends <- na.trim(exclude_low_samples, is.na="any", sides='both')
  # count number of na
  na_df <- trim_ends[is.na(trim_ends)]
  # make sure is regular time series
  index = index(trim_ends)
  startDate = index[1]
  endDate =  index[length(index)]
  minSeq = seq(from=startDate, to=endDate, by="1 min")
  regular_full.ts <- na.approx(trim_ends, x = index, xout= minSeq)
  
  assert(is.regular(regular_full.ts, strict=TRUE), TRUE)
  
  return(regular_full.ts)
}

merged <- create_full_dataset(train)

# Create time series with and without Dogecoin included
cols_include <- c("Cardano", "Stellar", "Binance Coin", "Litecoin", "Ethereum", "Bitcoin", "Dogecoin")
with_doge.ts <- get_regular_time_series(cols_include)
no_doge_cols <- cols_include[!cols_include %in% "Dogecoin"]
without_doge.ts <- get_regular_time_series(no_doge_cols)

### Create doge DF
dogeindex = index(with_doge.ts)
doge_startDate = dogeindex[1]
doge_endDate =  dogeindex[length(dogeindex)]

# Define dates for the training and test sets
train_start_date = index(without_doge.ts)[1]
train_end_date = train_start_date + 60*60*24*30*5
test_start_date = as.POSIXct("2018-11-12 09:15:00 UTC", tz="UTC")
test_end_date = test_start_date + 60*60*24*30*5

train2_start_date = doge_startDate + 60*60*24*30*8
train2_end_date = train2_start_date + 60*60*24*30*5
test2_start_date = train2_end_date
test2_end_date = test2_start_date + 60*60*24*30*5

train1 <- window(without_doge.ts, start=train_start_date, end=train_end_date)
test1 <- window(without_doge.ts, start=test_start_date, end = test_end_date)
train2 <- window(with_doge.ts, start=train2_start_date, end=train2_end_date)
test2 <- window(with_doge.ts, start=test2_start_date, end = test2_end_date)

train1 <- train1[format(index(train1), format="%M") %in% seq(0,55,5)]
test1 <- test1[format(index(test1), format="%M") %in% seq(0,55,5)]
train2 <- train2[format(index(train2), format="%M") %in% seq(0,55,5)]
test2 <- test2[format(index(test2), format="%M") %in% seq(0,55,5)]

colnames(train1) <- no_doge_cols
colnames(test1) <- no_doge_cols
colnames(train2) <- cols_include
colnames(test2) <- cols_include

##################
## Calculate correlations
##################
corr_mat <- round(cor(regular_full.ts), digits=3)
colnames(corr_mat) <- asset_names[cols_include]
rownames(corr_mat) <- asset_names[cols_include]

