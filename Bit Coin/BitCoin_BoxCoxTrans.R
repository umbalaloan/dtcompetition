bitcoin <- read.csv("Bitcoin_Historical_Price.csv")
# Load library
library(TSA)
library(fUnitRoots)
library(lmtest)
library(rugarch)
library(forecast)

# Box Cox Transform
box.bitc <- BoxCox(bitcoin_ts, lambda = "auto")
