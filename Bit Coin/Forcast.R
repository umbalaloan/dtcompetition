# Suggest ARIMA 
eacf(diff.log.bc) # Suggest ARIMA(0,1,0) ARIMA(0,1,1) ARIMA(1,1,1) 
arima_model <- armasubsets(y=diff.log.bc,nar=7,nma=7,y.name='test',ar.method='ols') 
plot(arima_model, main =" Arima model suggestion by armasubsets",)
# => ARIMA(2,1,5)(remove this due to EACF check),  ARIMA(2,1,6) ARIMA(6,1,6)
set_models <- list(c(0,1,0), c(0,1,1), c(1,1,1), c(2,1,6))
modelEst <- myCandidate(log.bc, orderList = set_models, methodType = "ML")

# Residual analysis
residual.analysis(model.216)

# best model is ARIMA(2,1,6)
# GARCH

residuals <- model.216$residuals


# APPLY GARCH
res <- model.015$residuals
ab.res <- abs(residuals)
acf(ab.res,  ci.type="ma",main="The sample ACF plot for absolute residual series")
pacf(ab.res, main="The sample PACF plot for absolute residual series")
eacf(ab.res) # ARMA(1,3) ARMA(2,3) => GARCH(3,1) GARCH(3,2)
sq.res <- residuals^2
par(mfrow=c(1,1))
acf(sq.res, ci.type="ma",main="The sample ACF plot for square residual series")
pacf(sq.res, main="The sample PACF plot for square residual series")
eacf(sq.res) # ARMA(3,3) ARMA(1,5) => GARCH(3,3) GARCH(5,1)


# Training model
model31<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 1)), 
                   mean.model = list(armaOrder = c(2, 6), include.mean = FALSE), 
                   distribution.model = "norm")
m.26_31<-ugarchfit(spec = model31, data = diff.log.bc, out.sample = 100)
m.26_31$aic
summary(m.26_11)
plot(m.26_31)
par(mfrow= c(1,2))

model32<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 2)), 
                    mean.model = list(armaOrder = c(2, 6), include.mean = FALSE), 
                    distribution.model = "norm")
m.26_32 <- ugarchfit(spec = model32, data = diff.log.bc, out.sample = 100)
m.26_32
plot(m.26_32)


model33<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 3)), 
                    mean.model = list(armaOrder = c(2, 6), include.mean = FALSE), 
                    distribution.model = "norm")
m.26_33 <- ugarchfit(spec = model33, data = diff.log.bc, out.sample = 100)
plot(m.26_33)
# => final m.26_33

model51<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(5, 1)), 
                    mean.model = list(armaOrder = c(2, 6), include.mean = FALSE), 
                    distribution.model = "norm")
m.26_51 <- ugarchfit(spec = model51, data = diff.log.bc, out.sample = 100)
AIC(m.26_51)

# Forecasting
forc.26_33 = ugarchforecast(m.26_33, data = diff.log.bc, n.ahead = 10, n.roll = 10)
plot(forc.26_33, which = "all")

# Model ARIMA(0,1,5) or ARIMA(1,1,1)
xreg=data.frame (constant=seq(bitcoin_ts))
n = length(bitcoin_ts)
n.ahead = 10
newxreq = data.frame(constant = (n+1): (n+n.ahead))
pred <- predict(model.111, n.ahead = 10, newxreg = NULL, se.fit = TRUE)
pred <- predict(m.22_2,n.ahead=10,se.fit = TRUE, newdata = newxreq, plot= TRUE)


# CHANGE GAPSH 11

model11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(2, 6), include.mean = FALSE), 
                      distribution.model = "norm")
m.26_11 <- ugarchfit(spec = model11, data = diff.log.bc, out.sample = 100)
m.26_11
plot(m.26_11)

actual_rs <- read.csv("Bitcoin_Prices_Forecasts.csv")
forc.26_11 = ugarchforecast(m.26_11, data = diff.log.bc, n.ahead = 10, n.roll = 10)
plot(forc.26_11, which = "all")

# Model validation
residual_forc <- read.csv(file = "Residuals_forecast.csv")
LBQPlot(, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)

# Using arimal model for forcasted Series
diff.log.bc
lastdiff.log.bc <- diff.log.bc[2129]
rs_forcast <- function(series_forcasted, lastdiff)
{
#  rsul <- data.frame(matrix(ncol = 1))
  for (i in length(1:series_forcasted))
  {
    fitted <- as.numeric(lastdiff) + as.numeric(series_forcasted[i])
    lastdiff <- as.numeric(fitted)
    print(lastdiff)
    print("&")
    print(fitted)
  }
}

forecast(m.26_11)

# actual resul
actual_rs.trans <- log(actual_rs$Closing.price)
diff.actual.rs <- diff(actual_rs.trans)
diff.actual.rs
forecast(m.22_2, n.ahead=10)
m.26_11

obseved <- actual_rs$Closing.price
log.observed <- log(obseved)
diff.observed <- diff(log.observed)
fitted <- pred$lowerInterval
inverDiff <- diffinv(fitted)
invertLog <- 10^inverDiff
MASE(obseved, fitted)
obseved
predicted

