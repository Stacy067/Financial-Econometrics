
# Generating a matrix
A<-matrix(c(4,9,2,1), nrow=2)
A

A<-matrix(c(4,2,9,1), nrow=2)
B<-matrix(c(2,0,0,7), nrow=2)
A+B
A-B

A<-matrix(c(3,0,-1,5), nrow=2)
2*A

A<-matrix(c(1,3,2,4), nrow=2)
B<-matrix(c(5,7,6,8), nrow=2)
A%*%B

X<-matrix(1:3,ncol=1)
t(X)

A<-matrix(c(1,1,6,2,2,2,1,2,4), nrow=3)
sum(diag(A))

A<-matrix(c(1,3,2,4), nrow=2)
solve(A)
A%*%solve(A)

A<-matrix(c(1,3,2,4), nrow=2)
b<-matrix(c(2,3), ncol=1)
solve(A,b)
A%*%solve(A,b)