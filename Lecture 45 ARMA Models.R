# Lecture 4

rm(list=ls())

# AR(1) Model
y<-arima.sim(n=500,list(ar=0.9))  
plot(y)
acf(y)
pacf(y)

# MA(2) Model
y<-arima.sim(n=500,list(ma=c(0.8,0.5)))
plot(y)
acf(y)
pacf(y)


# Lecture 5

# Simulation of the ARMA Models with R
y<-arima.sim(n=500,list(ar=c(0.9,-0.4)))
plot(y,xlab="",main="Time Series Plot")
abline(h=0,col="red")
acf(y, main="ACF of y")
pacf(y, main="PACF of y")

arma20<-arima(y, order=c(2,0,0), include.mean=FALSE)
arma20


# Forecasting with R
oildata<-read.table("inflation&oil.txt",header=TRUE)
attach(oildata)
inflation<-ts(inflation, start=2003+11/12,frequency=12)
plot(inflation,xlab="",main="Time Series Plot")
abline(h=0,col="red")

acf(inflation, main="ACF of Inflation")
pacf(inflation, main="PACF of Inflation")

inflation.arma<-arima(inflation, order=c(1,0,0),include.mean=TRUE)
predictations<-predict(inflation.arma,n.ahead=100)
attach(predictations)

U = pred + 2*se
L = pred - 2*se

minx=min(inflation,L)
maxx=max(inflation,U)
ts.plot(inflation,pred,col=1:2, ylim=c(minx,maxx))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed") 

predictations
names(predictations)
# The End
