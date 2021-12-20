rm(list=ls())


# OLS with I(0) Independent Variables

dy<-rnorm(1000)
dx<-rnorm(1000)
stationary<-lm(dy~dx)
summary(stationary)


# OLS with I(1) Independent Variables

dy<-rnorm(1000)
dx<-rnorm(1000)
y<-cumsum(dx)
x<-cumsum(dy)
spurious<-lm(y~x)
summary(spurious)

# Unit Root Test
y.ex<-arima.sim(n=1000,list(ar=c(0.4,-0.1),ma=c(0.01)))
dx<-rnorm(1000)
x<-cumsum(dx)
library("tseries")
adf.test(y.ex)
adf.test(dx)
adf.test(x)

# Unit root test with another package

data<-read.csv("MacroData.csv",header=TRUE)
attach(data)
names(data)
install.packages("fUnitRoots")
library("fUnitRoots")

dint<-ar(diff(interest),method='mle')
dint$order
adfTest(interest,lags=12,type="c")

dinf<-ar(diff(inflation),method='mle')
dinf$order
adfTest(inflation,lags=12,type="c")

dout<-ar(diff(output),method='mle')
dout$order
adfTest(output,lags=12,type="c")

