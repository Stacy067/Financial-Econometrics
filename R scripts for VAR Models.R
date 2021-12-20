rm(list=ls())

# State space representation with R Q1 in PS 5

PHI<-matrix(c(0.1,1,0,0.2,0,1,0.4,0,0),nrow=3)
PHI

SIGMA<-matrix(c(1,0,0,0,0,0,0,0,0),nrow=3)
SIGMA

INI<-matrix(c(1,0,-1),ncol=1)

N<-10

Fore<-rep(0,N)
Fore.Var<-rep(0,N)

Con.Ex<-PHI%*%INI
Con.Var<-SIGMA
Fore[1]<-Con.Ex[1,1]
Fore.Var[1]<-Con.Var[1,1]

for(i in 2:N){
Con.Ex<-PHI%*%Con.Ex
Con.Var<-SIGMA + PHI%*%Con.Var%*%t(PHI)
Fore[i]<-Con.Ex[1,1]
Fore.Var[i]<-Con.Var[1,1]
}
Fore
Fore.Var


# Forecasting with R

library(vars)
oildata<-read.table("inflation&oil.txt",header=TRUE)
attach(oildata)
inflation.ts<-ts(inflation, start=2003+11/12,frequency=12)
WTI.ts<-ts(WTI, start=2003+11/12,frequency=12)

vardata<-data.frame(WTI.ts,inflation.ts)
infocrit<-VARselect(vardata,lag.max=6,type="const")
infocrit
varoutcome<-VAR(vardata,p=3,type="const")
summary(varoutcome)
predictions<-predict(varoutcome, n.ahead=24)
plot(predictions)
fanchart(predictions)
fanchart(predictions,plot.type="single")

# Impulse response analysis
library(vars)
oildata<-read.table("inflation&oil.txt",header=TRUE)
attach(oildata)
vardata<-data.frame(WTI,inflation)

varoutcome<-VAR(vardata,p=3,type="const")

irf.WTI.WTI<-irf(varoutcome, impulse="WTI",response="WTI",n.ahead=24,ortho=TRUE,cumulative=FALSE,boot=TRUE,seed=12345)
irf.WTI.inflation<-irf(varoutcome, impulse="WTI",response="inflation",n.ahead=24,ortho=TRUE,cumulative=FALSE,boot=TRUE,seed=12345)
irf.inflation.WTI<-irf(varoutcome, impulse="inflation",response="WTI",n.ahead=24,ortho=TRUE,cumulative=FALSE,boot=TRUE,seed=12345)
irf.inflation.inflation<-irf(varoutcome, impulse="inflation",response="inflation",n.ahead=24,ortho=TRUE,cumulative=FALSE,boot=TRUE,seed=12345)

plot(irf.WTI.WTI)
plot(irf.WTI.inflation)
plot(irf.inflation.WTI)
plot(irf.inflation.inflation)


# Granger Causality Test

library(lmtest)
grangertest(WTI,inflation, order=3)
grangertest(inflation,WTI, order=3)


