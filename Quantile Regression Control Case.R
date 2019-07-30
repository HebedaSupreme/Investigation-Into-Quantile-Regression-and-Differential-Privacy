#File name: Differential Privacy and Quantile Regression Control Case
#Purpose: Generating median regression estimate data with quantile regression model without noise
#Description:
#Author:
#Date:


ControlCase <- function(ss=1000, sigma=0.25, niter=10000) {
  
  p = 5
  result = matrix(0, niter, p)
  
  for (i in 1:niter) {
    x=matrix(rnorm(ss*p, 0, 1), nrow=ss)
    y=x[,1]+x[,2]-x[,3]+2*x[,4]-x[,5]+rnorm(nrow(x), 0, sigma)
    rqmodel <- rq(y~x, 0.5)
    result[i,] = summary(rqmodel)$coefficients[2:6,1]
  }
  
  return(result)
  
}


res1k25 = ControlCase(ss=1000, sigma=0.25, niter=10000)
res1k50 = ControlCase(ss=1000, sigma=.5, niter=10000)
res1k100 = ControlCase(ss=1000, sigma=1, niter=10000)
res1k200 = ControlCase(ss=1000, sigma=2, niter=10000)
res1k500 = ControlCase(ss=1000, sigma=5, niter=10000)
res1k1000 = ControlCase(ss=1000, sigma=10, niter=10000)

res5k25 = ControlCase(ss=5000, sigma=0.25, niter=10000)
res5k50 = ControlCase(ss=5000, sigma=.5, niter=10000)
res5k100 = ControlCase(ss=5000, sigma=1, niter=10000)
res5k200 = ControlCase(ss=5000, sigma=2, niter=10000)
res5k500 = ControlCase(ss=5000, sigma=5, niter=10000)
res5k1000 = ControlCase(ss=5000, sigma=10, niter=10000)

res20k25 = ControlCase(ss=20000, sigma=0.25, niter=10000)
res20k50 = ControlCase(ss=20000, sigma=.5, niter=10000)
res20k100 = ControlCase(ss=20000, sigma=1, niter=10000)
res20k200 = ControlCase(ss=20000, sigma=2, niter=10000)
res20k500 = ControlCase(ss=20000, sigma=5, niter=10000)
res20k1000 = ControlCase(ss=20000, sigma=10, niter=10000)

beta = c(1, 1, -1, 2, -1)
ErrorComp <- function(datain) {
  error = sqrt((datain[,1] - beta[1])^2 + (datain[,2] - beta[2])^2 + (datain[,3] - beta[3])^2 + (datain[,4] - beta[4])^2 + (datain[,5] - beta[5])^2 )
  return(error)  
}

error1k25 = ErrorComp(datain=res1k25)
error1k50 = ErrorComp(datain=res1k50)
error1k100 = ErrorComp(datain=res1k100)
error1k200 = ErrorComp(datain=res1k200)
error1k500 = ErrorComp(datain=res1k500)
error1k1000 = ErrorComp(datain=res1k1000)

error5k25 = ErrorComp(datain=res5k25)
error5k50 = ErrorComp(datain=res5k50)
error5k100 = ErrorComp(datain=res5k100)
error5k200 = ErrorComp(datain=res5k200)
error5k500 = ErrorComp(datain=res5k500)
error5k1000 = ErrorComp(datain=res5k1000)

error20k25 = ErrorComp(datain=res20k25)
error20k50 = ErrorComp(datain=res20k50)
error20k100 = ErrorComp(datain=res20k100)
error20k200 = ErrorComp(datain=res20k200)
error20k500 = ErrorComp(datain=res20k500)
error20k1000 = ErrorComp(datain=res20k1000)

par(mfrow=c(2,3))
hist(error1k25, main="N=1000, sig=0.25", xlab="")
hist(error1k50, main="N=1000, sig=0.5", xlab="")
hist(error1k100, main="N=1000, sig=1", xlab="")
hist(error1k200, main="N=1000, sig=2", xlab="")
hist(error1k500, main="N=1000, sig=5", xlab="")
hist(error1k1000, main="N=1000, sig=10", xlab="")

par(mfrow=c(2,3))
hist(error5k25, main="N=5000, sig=0.25", xlab="")
hist(error5k50, main="N=5000, sig=0.5", xlab="")
hist(error5k100, main="N=5000, sig=1", xlab="")
hist(error5k200, main="N=5000, sig=2", xlab="")
hist(error5k500, main="N=5000, sig=5", xlab="")
hist(error5k1000, main="N=5000, sig=10", xlab="")

par(mfrow=c(2,3))
hist(error20k25, main="N=20000, sig=0.25", xlab="")
hist(error20k50, main="N=20000, sig=0.5", xlab="")
hist(error20k100, main="N=20000, sig=1", xlab="")
hist(error20k200, main="N=20000, sig=2", xlab="")
hist(error20k500, main="N=20000, sig=5", xlab="")
hist(error20k1000, main="N=20000, sig=10", xlab="")

