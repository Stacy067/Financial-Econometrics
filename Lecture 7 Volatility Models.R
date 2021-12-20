rm(list=ls())
returnsdata<-read.table("snp.txt",header=TRUE)
attach(returnsdata)
acf(Return)
acf(Return^2)
pacf(Return^2)

# Testing ARCH effect
Box.test(Return,lag=10,type='Ljung')
Box.test(Return^2,lag=10,type='Ljung')

# Estimation of GARCH(1,1) model
library(fGarch)
m1<-garchFit(~arma(1,0)+garch(1,1),data=Return,trace=F)
summary(m1)
plot(m1)

predict(m1,10) # predicting volatilities
v1<-volatility(m1) # Save volatilities
resi=residuals(m1,standardize=T) # Save standardized residuals

# Estimation of Threshold GARCH model
library(rugarch)
GJR.spec = ugarchspec(variance.model=list(model="gjrGARCH",
                                          garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(1,0)))

GJRfit = ugarchfit(data = Return, spec = GJR.spec)
GJRfit
plot(GJRfit,which="all")
vol<-sigma(GJRfit)
forc = ugarchforecast(GJRfit, n.ahead=100)


