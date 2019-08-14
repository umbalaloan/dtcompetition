# Load dataset
bitcoin <- read.csv("Bitcoin_Historical_Price.csv")
# Load library
library(TSA)
library(fUnitRoots)
library(lmtest)
library(rugarch)
bitcoin <- read.csv("Bitcoin_Historical_Price.csv")
bitcoin_ts <- data.frame(bitcoin$Close)
rownames(bitcoin_ts) <- bitcoin$Date
#bitcoin_ts$bitcoin.Close <- as.numeric(as.character(bitcoin_ts$bitcoin.Close))
bitcoin_ts <- ts(as.vector(bitcoin_ts), start = c(2013,27,4), frequency = 365)
plot(bitcoin_ts, type= 'o', main="Bit Coin Price Series from 27/04/2013 - 24/02/2019")



# Apply Data Transformation



# ========== Analyze model
# Scater plot
plot(y = bitcoin_ts, x = zlag(bitcoin_ts), main = "Scatter Plot of Bit Coin within Previous Day")

# => The scatter plot shows that there are strong linear relationship between current bit coin prices and its previous time
par(mfrow= c(1,1))
acf(bitcoin_ts, na.action = na.pass, main ="ACF plot of Bit Coin series")
# ACF has decaying pattern 
# => Non - stationary series
# => No seasonality
pacf(bitcoin_ts, na.action = na.pass, main = "PACF plot of Bit Coin Series")


# ============= Data Transformation
# Apply Log transform

log.bc <- log(bitcoin_ts)
acf(log.bc, na.action = na.pass, main = "Log transform of Bit Coin")
pacf(log.bc, na.action = na.pass,  main = "Log transform of Bit Coin")

# Using ADF Test to check whether time series is station or non station
ar(diff(log.bc)) # Order 31
adfTest(log.bc, lags = 31)
# The p-value is 0.9232 > 5% signgicance level => non-stationary

# Test with the first differenc
diff.log.bc <- (diff(log.bc, difference =1)) 

plot(diff.log.bc, type= 'o', main = "First difference of Bit Coin series") # It look like MA, no AR => Suggest IMA model IMA(1,4) IMA(1,5) IMA(1,9) and IMA(1,10)
plot(y = diff.log.bc, x = zlag(diff.log.bc))
par(mfrow = c(1,2))
acf(diff.log.bc , na.action = na.pass, main = "ACF for 1st difference of series")
pacf(diff.log.bc , na.action = na.pass, main = "PACF for 1st difference of series")

# ADF test to check whether time series is non-stationary
ar(diff(diff.log.bc), na.action = na.pass)
# Order is 32
adfTest(diff.log.bc, lags = 32)
# p_value = 0.01 < 0.05 => This model is stationary

# => ARIMA model
# Using EACF
eacf(diff.log.bc, ar.max = 7, ma.max = 7)
# Suggest ARIMA(0,1,0) ARIMA(0,1,1) ARIMA(1,1,1) 
# Using BIC table
bic = armasubsets(y=diff.log.bc,nar=3,nma=3,y.name='test',ar.method='ols')
plot(bic)
# => Suggest: ARIMA(1,1,1) ARIMA(2,1,0) 

# ========= Step 3: Parameter Estimations
# Maximum likelihood method:
model.011 <- arima(bitcoin_ts, order = c(0,1,1), method= 'ML')
model.010 <- arima(bitcoin_ts, order = c(0,1,0), method= 'ML')
model.111 <- arima(bitcoin_ts, order = c(1,1,1), method = 'ML')
model.210 <- arima(bitcoin_ts, order = c(2,1,0), method = 'ML')
#  ARIMA(2,1,6) ARIMA(6,1,6)
model.216 <- arima(bitcoin_ts, order = c(2,1,6), method = 'ML')
#model(6,1,6) <- arima(bitcoin_ts, order = c(6,1,6), method = 'ML')

model_list <- c(list(model.010), list(model.011), list(model.111), list(model.210), list(model.216))
selectBestModel("AIC", model_list)
selectBestModel("BIC", model_list)

Box.test(residuals(model.216), lag = length(model.216$residuals) -1, type =
           "Box-Pierce", fitdf = 0)

# ==> Suggest model arima(2,1,6)
model.015

# Least of Square
model.osl.011 <- arima(bitcoin_ts,order = c(0,1,1), method= 'CSS' )
model.osl.010 <- arima(bitcoin_ts, order = c(0,1,0), method= 'CSS')
model.osl.111 <- arima(bitcoin_ts, order = c(1,1,1), method = 'CSS')
model.osl.210 <- arima(bitcoin_ts, order = c(2,1,0), method = 'CSS')
model.osl.014 <- arima(bitcoin_ts, order = c(0,1,4), method = 'CSS')
model.osl.015 <- arima(bitcoin_ts, order = c(0,1,5), method = 'CSS')

model_lis_osl <-  c(list(model.osl.010), list(model.osl.011), list(model.osl.111), list(model.osl.210), list(model.osl.014), list(model.osl.015))
selectBestModel("AIC", model_lis_osl)
selectBestModel("BIC", model_lis_osl)

# Suggest Model ARIMA(2,1,6)

# ========== MODEL BUILDING =========
plot(rstandard(model.015), type= 'o', ylab="Standardized Residuals", main =
       "Time Series Plot of standardised residuals for Big Coint - Model 015")
abline(h=0)

# Plot QQ
qqnorm(residuals(model.015))
qqline(residuals(model.015), col = 2) # => violated

# Homos
library(car)
ncvTest(model.015)

# Apply AUTO arima

automodel <- auto.arima(bitcoin_ts, trace = TRUE)
qqnorm(residuals(automodel))
qqline(residuals(automodel), col = 2) # => violated
#------

plot(rstandard(model.111), type= 'o', ylab="Standardized Residuals", main =
       "Time Series Plot of standardised residuals for Big Coint - Model 111")
abline(h=0)

qqnorm(residuals(model.111))
qqline(residuals(model.111), col = 2)
shapiro.test(diff.log.bc) # The p-value is < 0.05 => Normality is violated

selectBestModel <- function(method = c('AIC', 'BIC'), list_model)
{
  dt_rs <- data.frame(matrix(ncol = 2))
  colnames(dt_rs) <- c(method, "model") 
  if (method == 'AIC')
  {
  for (i in 1:length(list_model))
  {
    model <- list_model[[i]]
    aic <- AIC(model)
    model_name <- paste(as.character(model$call)[1], as.character(model$call)[3], sep = "_")
    dt_rs <- rbind(dt_rs, c(aic, model_name))
    
  }
    return(dt_rs[order(dt_rs$AIC),])
  }
  else
  {
    for (i in 1: length(list_model))
    {
      model <- list_model[[i]]
      model_name <- paste(as.character(model$call)[1], as.character(model$call)[3], sep = "_")
      bic <- AIC(list_model[[i]], k= log(28))
      dt_rs <- rbind(dt_rs, c(bic, model_name))
    }
    return(dt_rs[order(dt_rs$BIC),])
  }
}


# ============= This time series is heteroscedasticity

library(fGarch)
library(rugarch)
library(CombMSC)
library(forecast)
library(tseries)

# Apply McLeod -Li test to see changing variance
McLeod.Li.test(arima(bitcoin_ts, order = c(0,1,5)))
 
# apply ARIMA(1,1,1) + GARCH 
abs.diff.log.bc <- abs(diff.log.bc)
sq.diff.log.bc <- diff.log.bc^2
par(mfrow=c(1,2))
acf(abs.diff.log.bc, main ="ACF plot for absolute return series")
pacf(abs.diff.log.bc, main ="PACF plot for absolute return series")
eacf(abs.diff.log.bc) # => Suggest ARMA(1,2) and ARMA(2,2), ARMA(1,3) => GARCH(2,2) GARCH(3,1) 

pacf(sq.diff.log.bc, main ="ACF plot for square return series")
acf(sq.diff.log.bc, main ="ACF plot for square return series")
eacf(sq.diff.log.bc) 
m.12 <- garch(diff.log.bc, order = c(1,2))
summary(m.12)
m.12_2 = garchFit(formula =  ~garch(1,2), data = diff.log.bc)
summary(m.12_2)

m.22 <- garch(diff.log.bc, order = c(2,2))
summary(m.22)
m.22_2 = garchFit(formula =  ~garch(2,2), data = diff.log.bc)
summary(m.12_2)
imputed_resi <- baggedImpute(m.22$residuals, interpolation = "Kalman", bootrep = 1000)
acf(imputed_resi)
pacf(imputed_resi)
qqnorm(imputed_resi)
qqline(imputed_resi, col = 2)
LBQPlot(imputed_resi, lag.max = 30, StartLag = 0 + 1, k = 0, SquaredQ = FALSE)

par(mfrow=c(1,1))
# ===== PREDICTION
fGarch::predict(m.22_2,n.ahead=10,trace=FALSE,plot=TRUE)
predic

forcast = ugarchforecast(m.22_2, data = diff.log.bc, n.ahead = 10, n.roll = 10)


model<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                  mean.model = list(armaOrder = c(2, 6), include.mean = FALSE), 
                  distribution.model = "norm")
m.26_11<-ugarchfit(spec=model,data=diff.log.bc)
forcast = ugarchforecast(m.26_11, data = diff.log.bc, n.ahead = 10)
forecasts <- forcast@forecast$seriesFor

# Model 2:

# Inverse difference 
invDiffForcast <- diffinv(forecasts)
actual_Values <- exp(invDiffForcast)


# Deal with missing values
library(imputeTS)
library(baggedImpute)

m.33 <- garch(residuals, order = c(3,3), TRACE = FALSE)
summary(m.23)
m.31 <- garch(residuals, order = c(3,1), TRACE = FALSE)
m.51 <-garch(residuals, order = c(5,1), TRACE = FALSE)
m.32 <- garch(residuals, order = c(3,2), TRACE = FALSE)
m.23_2 = garchFit(formula =  ~garch(2,3), data = diff.log.bc)
m.11_2 <- garch(residuals, order = c(3,1), TRACE = FALSE)
summary(m.23_2)
# => best GARCH m33



residual.analysis(m.22, class= "GARCH", start = 2)


m.23$residuals

sc.AIC <- AIC(m.33, m.31, m.32, m.51, m.11_2) # m22 suggest
