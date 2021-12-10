################
# File to look at moments of each currency
################
library("readr")
library("xts")
library("moments")

setwd("crypto_analysis")

asset_details <- read.csv("asset_details.csv")
train <- read_delim('train.csv', ',', col_names=TRUE)
train['posix'] <- as.POSIXct(train$timestamp, origin="1970-01-01 00:00:00.000", tz="UTC")

# create bitcoin dataframe
bitcoin_df <- train[train$Asset_ID == "1",]
bitcoin_target.ts = zoo(x=bitcoin_df$Target, order.by=bitcoin_df$posix)
bitindex = index(bitcoin_target.ts)
startDate = bitindex[1]
endDate =  bitindex[length(bitindex)]
minSeq = seq(from=startDate, to=endDate, by="1 min")
bitcoin_approx.ts = na.approx(bitcoin_target.ts, x = bitindex, xout= minSeq)

# create histograms of bitcoin returns
hist(coredata(bitcoin_approx.ts), main = "bitcoin target returns", probability = TRUE)
lines(density(coredata(bitcoin_approx.ts)))
hist(coredata(bitcoin_approx.ts), main = "bitcoin target returns", probability = TRUE, breaks=100)

# plot number of outliers
outliers = which(coredata(bitcoin_approx.ts) < quantile(coredata(bitcoin_approx.ts), 0.0) | coredata(bitcoin_approx.ts) > quantile(coredata(bitcoin_approx.ts), 0.975) )
length(outliers)
outlier_percentage = length(outliers)/length(coredata(bitcoin_approx.ts))*100

boxplot(coredata(bitcoin_approx.ts), main="boxplot")
quantile(bitcoin_approx.ts)

###################
## get stats on all series
##################
stats_table = matrix(0,4,14)

for (asset_id in c(0:13)) {
  df <- train[train$Asset_ID == asset_id,]
  
  mu = mean(df$Target, na.rm=TRUE)
  sig = var(df$Target, na.rm=TRUE)
  skew = skewness(df$Target, na.rm=TRUE)
  kurt = kurtosis(df$Target, na.rm=TRUE)
  
  stats_table[1,asset_id + 1] = mu
  stats_table[2,asset_id + 1] = sig
  stats_table[3,asset_id + 1] = skew
  stats_table[4,asset_id + 1] = kurt
}

stats_df = data.frame(stats_table)
colnames(stats_df) = asset_details[order(asset_details$Asset_ID),]$Asset_Name
rownames(stats_df) = c("Mean", "Variance", "Skewness", "Kurtosis")
