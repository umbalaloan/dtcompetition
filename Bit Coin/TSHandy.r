# TS Handy: Handy Tools for Univariate Time Series Analysis
# Author(s): Yong Kai, Wong
# Version Number: 0.2
# Version Controls
# -----------------------------------------------------------
# Version No  | Descriptions
# -----------------------------------------------------------
# 0.1         |- myCandidate to return a list of fitted models
#             |- ARIMADiagnostic: 6 diagnostic plots 
# 0.2         |- Add Information Criteria Table in myCandidate
#             |- Remove includeConstant from myCandidate
#             |- Add "argument" passing to Arima function
#             |- Add OpenPlot in ARIMADiagnostic
#             |- Add "Significant test result" in myCandidate
#             |- Check if the user has some packages pre-installed

# Objective: To automate repetitive processes in time series analysis

## Install packages and load libraries
pkgs <- c('TSA', 'fUnitRoots', 'forecast', 'lmtest')

new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs)
invisible(lapply(pkgs, require, character.only = T))

myCandidate <- function(timeSeries, orderList,
                        methodType = c("CSS-ML", "ML", "CSS")[1],
                        fixedList = NULL, icSortBy = c("AIC", "AICc","BIC")[1],
                        ...){
  
  # timeSeries = the time series (a ts object)
  # orderList = a list object of c(p, d, q)
  # methodType = estimation method; default = "CSS-ML"
  # fixedList = a list object of free/fixed coefficient
  # icSortBy = information criterion (IC) to be used to sort the IC table
  #         : default value: by AIC
  # ... Additional arguments to be passed to Arima
  myCandidateEst <- list()
  n <- length(orderList)
  for(i in 1:n){
    order <- sapply(orderList,function(x) unlist(x))[,i]
    myCandidateEst[[i]] <- Arima(y = timeSeries, order = order, method = methodType)
  }
  significanceTest <- list()               # a list for significance tests
  ICTable <- matrix(NA, nrow = n, ncol = 6) # create a matrix to store IC
  for(i in 1:n){
    for(j in 1:3){
      ICTable[i,j] <- orderList[[i]][j]       # return the ARIMA orders
    }
      ICTable[i,4] <- myCandidateEst[[i]]$aic # 
      ICTable[i,5] <- myCandidateEst[[i]]$aicc
      ICTable[i,6] <- myCandidateEst[[i]]$bic
      
      significanceTest[[i]]<- coeftest(myCandidateEst[[i]])
  }
  
  ICTable <- data.frame(ICTable)
  names(ICTable) <- c('p', 'd', 'q', 'AIC', 'AICc', 'BIC')
  
  if(icSortBy == "AIC"){
    ICTable <- ICTable[order(ICTable$AIC),] # sort the table by AIC
  }else if(icSortBy == "AICc"){
    ICTable <- ICTable[order(ICTable$AICc),] # sort the table by AICc
  }else if(icSortBy == "BIC"){
    ICTable <- ICTable[order(ICTable$BIC),]
  }else{
    stop("Incorrect Information Criterion")
  }
  
  
  myCandidateEst <- list(model = myCandidateEst, IC = ICTable,
                         significanceTest = significanceTest,
                         orderList = orderList)
  return(myCandidateEst)
}

openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    tryInfo = try( X11( width=width*mag , height=height*mag , type="cairo" , 
                        ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off() 
      X11( width=width*mag , height=height*mag , type="cairo" , ... )
    }
  } else { # Windows OS
    tryInfo = try( windows( width=width*mag , height=height*mag , ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off() 
      windows( width=width*mag , height=height*mag , ... )    
    }
  }
}


ARIMAdiagnostic <- function(model, lagNumber, 
                            openPlot = c(TRUE, FALSE)[2]){
  
  e <- residuals(model)   # residuals
  er <- rstandard(model)   # standardized residuals
  if(openPlot == TRUE){
    openGraph(width=7 , height=7 , mag=1.0)
    par(mfrow = c(3,2))
  }
  
  # QQ Plot
  qqnorm(er, main = "", ylab = "QQ of Residuals")
  qqline(er, col = "red")
  
  # Standardised residual plot
  
  plot(e, type = "n", main = "", ylab = "Standardized Residuals")
  abline(h=c(-3,0,3),col = c("red","black", "red"), lty = c("dotted", "solid", "dotted"))
  points(e, pch = 1, cex = 0.5)
  
  # ACF/PACF Graphs
  
  Acf(e, main = "", lag.max = lagNumber)
  Pacf(e, main = "", lag.max =lagNumber)
  
  # Histogram
  
  hist(er, breaks = "FD", freq  = FALSE, col = "gray", border = "white",
       main = "", ylab = "Density Function", xlab = "Standardized Residuals")
  x <- seq(min(er),max(er), by = 0.001)  # add the theoretical z-distribution
  lines(x, dnorm(x), col = "red")
  legend("topleft", legend = c("Sample","Theoretical"), col = c("gray", "red"),
         pch = 15, bty = "n")
  
  # P-Value for Ljung-Box
  
  pValue <- rep(0, lagNumber)
  for(j in 0:lagNumber){
    pValue[j] <- Box.test(e, lag = j, type = "Ljung-Box", fitdf = 0)$p.value
  }
  plot(pValue, ylim = c(0,1), type = "n", ylab = "P-values",
       xlab = "Lags")
  points(pValue)
  abline(h = 0.05, col = "red", lty = "dotted")
  
  par(mfrow = c(1,1))
  
}
