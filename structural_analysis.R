ls()
rm(list = ls())

library(vars)
library(tsDyn)
library(tseries)
library(Spillover)

# calculate stationarity
p_values <- c()

for (col in colnames(train1)) {
  p_value <- adf.test(train1[, col], k=15)$p.value
  p_values <- c(p_values, p_value)
}
# all tests indicate stationarity

# determine optimal lags
coeffs_info <- VARselect(train1, lag.max=12, type="const")
# try 20 next
model <- VAR(train1, p = 3, season=15)
#est_coeffs <- coef(model)
# generalized forecast error variance decomposition
model.gfevd <- g.fevd(model, n.ahead=10) # normalized


# calculate var models without and with doge
no_doge = VAR(train1, p = 3)
no_doge.summary <- summary(no_doge)
doge = VAR(train2, p = 3)
doge.summary <- summary(doge)

# calculate the orthogonalized impulse responses
ortho_no_doge <- t(chol(no_doge.summary$covres))
ortho_doge <- t(chol(doge.summary$covres))
# plot the impulse response for Ethereum from a shock to Ethereum
ir_plot <- irf(doge, impulse = "Ethereum", response = "Ethereum", n.ahead = 10, ortho = TRUE)
plot(ir_plot)

# calculate the generalized forecast error variance decomposition
errors_no_doge <- g.fevd(no_doge, n.ahead=10) # normalized
errors_doge <- g.fevd(doge, n.ahead=10) # normalized

# test Granger causality
bitcoin_cause <- causality(no_doge, cause="Bitcoin")


######
#predictions with tsDyn package
######
tsdyn_no_doge <- tsDyn::lineVar(train1, 3)
num_predictions <- 3000
no_doge_pred <- predict_rolling(tsdyn_no_doge, newdata = test1, nroll=num_predictions, n.ahead=1)

asset_to_plot = "Bitcoin"
forecast_plot <- window(train1[,asset_to_plot], start=index(train1)[length(index(train1))-num_predictions-10] , end=index(train1)[length(index(train1))])
index(forecast_plot) = seq(1:length(index(forecast_plot)))
plot(forecast_plot, type="l", ylab="no doge", title=paste("Predictions for ", asset_to_plot, " pre-Doge", sep=""))
preds_roll_ts <- ts(no_doge_pred$pred, start= index(forecast_plot)[length(index(forecast_plot))-num_predictions])
lines(preds_roll_ts[,asset_to_plot], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

# compute accuracy
no_doge_acc <- accuracy_stat(no_doge_pred$pred, no_doge_pred$true)


tsdyn_doge <- tsDyn::lineVar(train2, 3)
num_predictions <- 3000
doge_pred <- predict_rolling(tsdyn_doge, newdata = test2, nroll=num_predictions, n.ahead=1)

asset_to_plot = "Bitcoin"
forecast_plot <- window(train1[,asset_to_plot], start=index(train1)[length(index(train1))-num_predictions-10] , end=index(train1)[length(index(train1))])
index(forecast_plot) = seq(1:length(index(forecast_plot)))
plot(forecast_plot, type="l", ylab="no doge", title=paste("Predictions for ", asset_to_plot, " pre-Doge", sep=""))
preds_roll_ts <- ts(no_doge_pred$pred, start= index(forecast_plot)[length(index(forecast_plot))-num_predictions])
lines(preds_roll_ts[,asset_to_plot], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

# compute accuracy
doge_acc <- accuracy_stat(doge_pred$pred, doge_pred$true)


#########
## AR(3) model
#########
ar3_model <- tsDyn::linear(coredata(train1[,"Bitcoin"]), m=3)
ar3_pred <- predict_rolling(ar3_model, newdata = coredata(test1[,"Bitcoin"]), nroll=num_predictions, n.ahead=1)
ar3_acc <- accuracy_stat(ar3_pred$pred, ar3_pred$true)

plot(ar3_pred$true[1:15,], type="l", ylab="no doge", main=paste("AR(3) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(ar3_pred$pred[1:15,], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

######### VAR(3) model
var3_model <- tsDyn::lineVar(coredata(train1), lag=3)
var3_pred <- predict_rolling(var3_model, newdata = coredata(test1[,"Bitcoin"]), nroll=num_predictions, n.ahead=1)
var3_acc <- accuracy_stat(var3_pred$pred, var3_pred$true)

plot(var3_pred$true[1:15,"Bitcoin"], type="l", ylab="no doge", main=paste("VAR(3) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(var3_pred$pred[1:15,"Bitcoin"], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

###### Sarima on Bitcoin ######
sarima_bit <- sarima(coredata(train1[,"Bitcoin"]), 3,0,0,0,0,1,15, xreg=NULL)

##############
# Non-linear models
##############

aar_model <- aar(coredata(train1[,"Bitcoin"]), m=3)
aar_pred <- predict_rolling(aar_model, newdata = test1[,"Bitcoin"], nroll=num_predictions, n.ahead=1)
aar_acc <- accuracy_stat(aar_pred$pred, aar_pred$true)

plot(aar_pred$true[1:15, ], type="l", ylab="no doge", main=paste("AAR(3) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(aar_pred$pred[1:15, ], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

aar6_model <- aar(coredata(train1[,"Bitcoin"]), m=6)
aar6_pred <- predict_rolling(aar6_model, newdata = test1[,"Bitcoin"], nroll=num_predictions, n.ahead=1)
aar6_acc <- accuracy_stat(aar6_pred$pred, aar6_pred$true)

plot(aar6_pred$true[1:15, ], type="l", ylab="no doge", main=paste("AAR(6) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(aar6_pred$pred[1:15, ], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

aar12_model <- aar(coredata(train1[,"Bitcoin"]), m=12)
aar12_pred <- predict_rolling(aar12_model, newdata = test1[,"Bitcoin"], nroll=num_predictions, n.ahead=1)
aar12_acc <- accuracy_stat(aar12_pred$pred, aar12_pred$true)

plot(aar12_pred$true[1:15, ], type="l", ylab="no doge", main=paste("AAR(12) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(aar12_pred$pred[1:15, ], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

#### Neural net ####
nn_model <- tsDyn::nnetTs(coredata(train1[,"Bitcoin"]), m=3, size=5)
nn_pred <- predict_rolling(nn_model, newdata = test1[,"Bitcoin"], nroll=num_predictions, n.ahead=1)
nn_acc <- accuracy_stat(nn_pred$pred, nn_pred$true)

plot(nn_pred$true[1:15, ], type="l", ylab="no doge", main=paste("NN(12) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(nn_pred$pred[1:15, ], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

setar_model <- setar(coredata(train1[,"Bitcoin"]), m=3, mL=1, mH=1, thDelay=1)
setar_pred <- predict_rolling(setar_model, newdata = coredata(test1[,"Bitcoin"]), nroll=num_predictions, n.ahead=1)
setar_acc <- accuracy_stat(setar_pred$pred, setar_pred$true)

plot(setar_pred$true[1:15,], type="l", ylab="no doge", main=paste("Setar(1) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(setar_pred$pred[1:15,], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

#########
#   m = embedding dimension
#   thDelay = time_delay, want zero
#####
nsetar_model <- setar(coredata(train1[,"Bitcoin"]), m=3, d=1)
nsetar_pred <- predict_rolling(nsetar_model, newdata = coredata(test1[,"Bitcoin"]), nroll=num_predictions, n.ahead=1)
nsetar_acc <- accuracy_stat(nsetar_pred$pred, nsetar_pred$true)

plot(nsetar_pred$true[1:15,], type="l", ylab="no doge", main=paste("Setar(3) ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(nsetar_pred$pred[1:15,], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))

###########
# Lstar model, logistic smoothing
###########
lstar.model.3 <- lstar(coredata(train1[,"Bitcoin"]), m=3 )
lstar.model.3.pred <- tsDyn::predict_rolling(lstar.model.3, newdata = coredata(test1[,"Bitcoin"]), nroll=num_predictions, n.ahead=1)
lstar.model.3.acc <- accuracy_stat(lstar.model.3.pred$pred, lstar.model.3.pred$true)

plot(lstar.model.3.pred$true[1:15,], type="l", ylab="no doge", main=paste("Lstar ", asset_to_plot, " model (pre-Doge)", sep=""))
lines(lstar.model.3.pred$pred[1:15,], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))


plot_forecasts <- function(asset_to_plot, predictions) {
  forecast_plot <- window(train1[,asset_to_plot], start=index(train1)[length(index(train1))-num_predictions-10] , end=index(train1)[length(index(train1))-num_predictions+10])
  index(forecast_plot) = seq(1:length(index(forecast_plot)))
  plot(forecast_plot, type="l", ylab="no doge", main=paste("Predictions for ", asset_to_plot, " pre-Doge", sep=""))
  preds_roll_ts <- ts(predictions$pred, start= index(forecast_plot)[length(index(forecast_plot))-num_predictions])
  lines(preds_roll_ts[,asset_to_plot], col=2, lty=2)
  legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))
}

plot_forecasts("Bitcoin", lstar.model.3.pred)
