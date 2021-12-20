# 1. Introdution and Sample Session

install.packages("AER")
library("AER")

objects()
data("Journals", package="AER")
dim(Journals)
names(Journals)

plot(log(subs)~log(price/citations),data=Journals)
j_lm<-lm(log(subs)~log(price/citations),data=Journals)
abline(j_lm)
summary(j_lm)

# Simulating Data
n=100
x1<-rnorm(n, mean = 0, sd = 1)
x2<-rnorm(n, mean = 4, sd = 4)
e<-rnorm(n, mean = 0, sd = 2)
y<-2*x1+e

# Check the simulated data
x1
x2
hist(x1)
hist(x2)

# Estimation
OLS1<-lm(y~x1)
summary(OLS1)
OLS2<-lm(y~x1+x2)
summary(OLS2)
OLS3<-lm(y~x1-1)
summary(OLS3)

# 2. R as a calculator

1+1
2^3
x<-c(1.8, 3.14, 4, 88.169, 13)
length(x)
2*x+3
5:1*x+1:5
log(x)
x[c(1,4)]
x[-c(2,3,5)]
ones<-rep(1,10)
even<-seq(from=2,to=20,by=2)
trend<-1981:2005
ones
even
trend

# 3. Matrix Operations

A<-matrix(1:6, nrow=2)
A
B<-matrix(1:6, ncol=3)
B
t(A)
dim(A)
nrow(A)
ncol(A)

A1<-A[1:2,c(1,3)]
A1

det(A1)
eigen(A1)
solve(A1)
A1%*%solve(A1)

diag(4)
diag(A1)
cbind(1,A1)
rbind(A1,diag(4,2))

# 4. R as a Programming Language
x<-c(1.8, 9, 20)
mode(x)
x>3
names(x)<-c("a","b","c")
x
x[2:3]
x[c("b","c")]
x[x>4]

mylist<-list(sample=rnorm(5),family="normal distribution",parameters=list(mean=0,sd=1))
mylist

mylist[[1]]
mylist[["sample"]]
mylist$sample

x<-c(1.8,3.14,4,88.169,13)
x>3 & x<=4
which(x>3 & x<=4)
which.min(x)
min(x)
which.max(x)
max(x)
all(x>3)
any(x>3)

is.numeric(x)
is.character(x)
as.character(x)

set.seed(123)
rnorm(2)
rnorm(2)
set.seed(123)
x<-rnorm(2)
x
dnorm(x)
pnorm(x)
qnorm(pnorm(x))

sample(1:5)
sample(c("male","female"),size=5,replace=TRUE, prob=c(0.2,0.8))

x<-c(1.8,3.14,4,88.169,13)
if(rnorm(1)>0) sum(x) else mean(x)

ifelse(x>4, sqrt(x), x^2)

for(i in 2:5){
x[i]<-x[i]-x[i-1]
}

x[-1]

while(sum(x)<100){
x<-2*x
}
x

# Simpler one
x<-c(1,3,5,100,10)
if(rnorm(1)>0) sum(x) else mean(x)

ifelse(x>4, sqrt(x), x^2)


y<-rep(0,5)
for(i in 1:5){
              y[i]<-x[i]^2
             }

y


x<-c(1,3,5,100,10)
y<-rep(0,5)
for(i in 2:5){
              y[i]<-log(x[i])-log(x[i-1])
             }
y[-1]

for(i in 2:5){
x[i]<-x[i]-x[i-1]
}

x[-1]

while(sum(x)<100){
x<-2*x
}
x


sumvector<-function(Y){
           sum(Y)
 

cmeans2<-function(X){
rval<-rep(0,ncol(X))
for(j in 1:ncol(X)) rval[j]<-mean(X[,j])
return(rval)
}

X<-matrix(1:20, ncol=2)
cmeans2(X)

# 5. Data Management in R

mydata<-data.frame(one=1:5, two=6:10, three=11:15)

mydata<-as.data.frame(matrix(1:15,ncol=3))
names(mydata)<-c("one","two","three")

mydata$two
mydata[,"two"]
mydata[,2]
mean(two)
attach(mydata)
mean(two)
detach(mydata)

mydata.sub<-subset(mydata,two<=8,select=-two)
mydata.sub
write.table(mydata,file="mydata.txt",col.names=TRUE)
newdata<-read.table("mydata.txt",header=TRUE)
newdata

pdf("myfile.pdf",height=5,width=6)
plot(1:20,pch=1:20, col=1:20, cex=2)
dev.off()

newdata<-read.table("mydata.txt",header=TRUE)
data<-read.table("inflation&oil.txt",header=TRUE)
attach(data)
summary(inflation)
mean(inflation)
median(inflation)
var(inflation)
sd(inflation)

hist(inflation, freq =FALSE)
lines(density(inflation), col=4)
