res111 <- model.111$residuals
ab.res11 <- abs(res111)
eacf(ab.res11) # ARMA(1,3) and ARMA(2,3) ARMA(1,4) => GARCH(3,1) GARCH(3,2) GARCH(4,1)
sq.res11 <- res111^2
eacf(sq.res11) # ARMA(1,4) ARMA(2,4) = GARCH(4,1) GARCH(4,2)

m31_v2<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 1)), 
                    mean.model = list(armaOrder = c(1, 1), include.mean = FALSE), 
                    distribution.model = "norm")
m.11_31<-ugarchfit(spec = m31_v2, data = diff.log.bc, out.sample = 100)
m.11_31
plot(m.11_31)

m32_v2<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 2)), 
                   mean.model = list(armaOrder = c(1, 1), include.mean = FALSE), 
                   distribution.model = "norm")
m.11_32<-ugarchfit(spec = m32_v2, data = diff.log.bc, out.sample = 100)
m.11_32
plot(m.11_32)
forc.11_32 = ugarchforecast(m.11_32, data = diff.log.bc, n.ahead = 10, n.roll = 10)
plot(forc.11_32, which = "all")

#### 
