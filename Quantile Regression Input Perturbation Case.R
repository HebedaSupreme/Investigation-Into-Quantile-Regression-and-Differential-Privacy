#File name: Differential Privacy and Quantile Regression Input Perturbation Case
#Purpose: Generating median regression estimate data with quantile regression model without noise
#Description:
#Author:
#Date:


InPerbCase <- function(ss=1000, sigma=0.25, niter=10000, privbud=0.01) {
  
  p = 5
  result = matrix(0, niter, p)
  
  for (i in 1:niter) {
    x=matrix(rnorm(ss*p, 0, 1), nrow=ss)
    y=x[,1]+x[,2]-x[,3]+2*x[,4]-x[,5]+rnorm(nrow(x), 0, sigma)
    maxi=0
    for(m in 1:ss) {
      for(j in 1:ss) {
        normval=abs(x[m,1]-x[j,1])+abs(x[m,2]-x[j,2])+abs(x[m,3]-x[j,3])+abs(x[m,4]-x[j,4])+abs(x[m,5]-x[j,5])+abs(y[m]-y[j])
        if(normval>maxi){
          maxi=normval
        }
      }
    }
    senvty=2*maxi
    z=matrix(rlaplace(ss*p, senvty, privbud),nrow=ss)
    w=(rlaplace(nrow(x), senvty, privbud))
    
    x=x+z
    y=y+w
    rqmodel <- rq(y~x, 0.5)
    result[i,] = summary(rqmodel)$coefficients[2:6,1]
  }
  
  return(result)
  
}


res1k25001 = InPerbCase(ss=1000, sigma=0.25, niter=10000, privbud=0.01)
res1k25010 = InPerbCase(ss=1000, sigma=0.25, niter=10000, privbud=0.1)
res1k25050 = InPerbCase(ss=1000, sigma=0.25, niter=10000, privbud=0.5)
res1k25100 = InPerbCase(ss=1000, sigma=0.25, niter=10000, privbud=1)
res1k25150 = InPerbCase(ss=1000, sigma=0.25, niter=10000, privbud=1.5)
res1k25200 = InPerbCase(ss=1000, sigma=0.25, niter=10000, privbud=2)

res1k50001 = InPerbCase(ss=1000, sigma=.5, niter=10000, privbud=0.01)
res1k50010 = InPerbCase(ss=1000, sigma=.5, niter=10000, privbud=0.1)
res1k50050 = InPerbCase(ss=1000, sigma=.5, niter=10000, privbud=0.5)
res1k50100 = InPerbCase(ss=1000, sigma=.5, niter=10000, privbud=1)
res1k50150 = InPerbCase(ss=1000, sigma=.5, niter=10000, privbud=1.5)
res1k50200 = InPerbCase(ss=1000, sigma=.5, niter=10000, privbud=2)

res1k100001 = InPerbCase(ss=1000, sigma=1, niter=10000, privbud=0.01)
res1k100010 = InPerbCase(ss=1000, sigma=1, niter=10000, privbud=0.1)
res1k100050 = InPerbCase(ss=1000, sigma=1, niter=10000, privbud=0.5)
res1k100100 = InPerbCase(ss=1000, sigma=1, niter=10000, privbud=1)
res1k100150 = InPerbCase(ss=1000, sigma=1, niter=10000, privbud=1.5)
res1k100200 = InPerbCase(ss=1000, sigma=1, niter=10000, privbud=2)

res1k200001 = InPerbCase(ss=1000, sigma=2, niter=10000, privbud=0.01) #we're on this one
res1k200010 = InPerbCase(ss=1000, sigma=2, niter=10000, privbud=0.1)
res1k200050 = InPerbCase(ss=1000, sigma=2, niter=10000, privbud=0.5)
res1k200100 = InPerbCase(ss=1000, sigma=2, niter=10000, privbud=1)
res1k200150 = InPerbCase(ss=1000, sigma=2, niter=10000, privbud=1.5)
res1k200200 = InPerbCase(ss=1000, sigma=2, niter=10000, privbud=2)

res1k500001 = InPerbCase(ss=1000, sigma=5, niter=10000, privbud=0.01)
res1k500010 = InPerbCase(ss=1000, sigma=5, niter=10000, privbud=0.1)
res1k500050= InPerbCase(ss=1000, sigma=5, niter=10000, privbud=0.5)
res1k500100 = InPerbCase(ss=1000, sigma=5, niter=10000, privbud=1)
res1k500150 = InPerbCase(ss=1000, sigma=5, niter=10000, privbud=1.5)
res1k500200 = InPerbCase(ss=1000, sigma=5, niter=10000, privbud=2)

res1k1000001 = InPerbCase(ss=1000, sigma=10, niter=10000, privbud=0.01)
res1k1000010 = InPerbCase(ss=1000, sigma=10, niter=10000, privbud=0.1)
res1k1000050 = InPerbCase(ss=1000, sigma=10, niter=10000, privbud=0.5)
res1k1000100 = InPerbCase(ss=1000, sigma=10, niter=10000, privbud=1)
res1k1000150 = InPerbCase(ss=1000, sigma=10, niter=10000, privbud=1.5)
res1k1000200 = InPerbCase(ss=1000, sigma=10, niter=10000, privbud=2)


#res5k25 = InPerbCase(ss=5000, sigma=0.25, niter=10000)
#res5k50 = InPerbCase(ss=5000, sigma=.5, niter=10000)
#res5k100 = InPerbCase(ss=5000, sigma=1, niter=10000)
#res5k200 = InPerbCase(ss=5000, sigma=2, niter=10000)
#res5k500 = InPerbCase(ss=5000, sigma=5, niter=10000)
#res5k1000 = InPerbCase(ss=5000, sigma=10, niter=10000)

#res20k25 = InPerbCase(ss=20000, sigma=0.25, niter=10000)
#res20k50 = InPerbCase(ss=20000, sigma=.5, niter=10000)
#res20k100 = InPerbCase(ss=20000, sigma=1, niter=10000)
#res20k200 = InPerbCase(ss=20000, sigma=2, niter=10000)
#res20k500 = InPerbCase(ss=20000, sigma=5, niter=10000)
#res20k1000 = InPerbCase(ss=20000, sigma=10, niter=10000)

beta = c(1, 1, -1, 2, -1)
ErrorComp <- function(datain) {
  error = sqrt((datain[,1] - beta[1])^2 + (datain[,2] - beta[2])^2 + (datain[,3] - beta[3])^2 + (datain[,4] - beta[4])^2 + (datain[,5] - beta[5])^2 )
  return(error)  
}

error1k25001 = ErrorComp(datain=res1k25001)
error1k25010 = ErrorComp(datain=res1k25010)
error1k25050 = ErrorComp(datain=res1k25050)
error1k25100 = ErrorComp(datain=res1k25100)
error1k25150 = ErrorComp(datain=res1k25150)
error1k25200 = ErrorComp(datain=res1k25200)

error1k50001 = ErrorComp(datain=res1k50001)
error1k50010 = ErrorComp(datain=res1k50010)
error1k50050 = ErrorComp(datain=res1k50050)
error1k50100 = ErrorComp(datain=res1k50100)
error1k50150 = ErrorComp(datain=res1k50150)
error1k50200 = ErrorComp(datain=res1k50200)

error1k100001 = ErrorComp(datain=res1k100001)
error1k100010 = ErrorComp(datain=res1k100010)
error1k100050 = ErrorComp(datain=res1k100050)
error1k100100 = ErrorComp(datain=res1k100100)
error1k100150 = ErrorComp(datain=res1k100150)
error1k100200 = ErrorComp(datain=res1k100200)

error1k200001 = ErrorComp(datain=res1k200001) # we're also up to here
error1k200010 = ErrorComp(datain=res1k200010)
error1k200050 = ErrorComp(datain=res1k200050)
error1k200100 = ErrorComp(datain=res1k200100)
error1k200150 = ErrorComp(datain=res1k200150)
error1k200200 = ErrorComp(datain=res1k200200)

error1k500001 = ErrorComp(datain=res1k500001)
error1k500010 = ErrorComp(datain=res1k500010)
error1k500050 = ErrorComp(datain=res1k500050)
error1k500100 = ErrorComp(datain=res1k500100)
error1k500150 = ErrorComp(datain=res1k500150)
error1k500200 = ErrorComp(datain=res1k500200)

error1k1000001 = ErrorComp(datain=res1k1000001)
error1k1000010 = ErrorComp(datain=res1k1000010)
error1k1000050 = ErrorComp(datain=res1k1000050)
error1k1000100 = ErrorComp(datain=res1k1000100)
error1k1000150 = ErrorComp(datain=res1k1000150)
error1k1000200 = ErrorComp(datain=res1k1000200)

#error5k25 = ErrorComp(datain=res5k25)
#error5k50 = ErrorComp(datain=res5k50)
error5k100 = ErrorComp(datain=res5k100)
#error5k200 = ErrorComp(datain=res5k200)
#error5k500 = ErrorComp(datain=res5k500)
#error5k1000 = ErrorComp(datain=res5k1000)

#error20k25 = ErrorComp(datain=res20k25)
#error20k50 = ErrorComp(datain=res20k50)
#error20k100 = ErrorComp(datain=res20k100)
#error20k200 = ErrorComp(datain=res20k200)
#error20k500 = ErrorComp(datain=res20k500)
#error20k1000 = ErrorComp(datain=res20k1000)

par(mfrow=c(2,3))
hist(error1k25001, main="N=1000, sig=0.25, pb=0.01", xlab="")
hist(error1k25010, main="N=1000, sig=0.25, pb=0.1", xlab="")
hist(error1k25050, main="N=1000, sig=0.25, pb=0.5", xlab="")
hist(error1k25100, main="N=1000, sig=0.25, pb=1", xlab="")
hist(error1k25150, main="N=1000, sig=0.25, pb=1.5", xlab="")
hist(error1k25200, main="N=1000, sig=0.25, pb=2", xlab="")

par(mfrow=c(2,3))
hist(error1k50001, main="N=1000, sig=0.5, pb=0.01", xlab="")
hist(error1k50010, main="N=1000, sig=0.5, pb=0.1", xlab="")
hist(error1k50050, main="N=1000, sig=0.5, pb=0.5", xlab="")
hist(error1k50100, main="N=1000, sig=0.5, pb=1", xlab="")
hist(error1k50150, main="N=1000, sig=0.5, pb=1.5", xlab="")
hist(error1k50200, main="N=1000, sig=0.5, pb=2", xlab="")

par(mfrow=c(2,3))
hist(error1k100001, main="N=1000, sig=1, pb=0.01", xlab="")
hist(error1k100010, main="N=1000, sig=1, pb=0.1", xlab="")
hist(error1k100050, main="N=1000, sig=1, pb=0.5", xlab="")
hist(error1k100100, main="N=1000, sig=1, pb=1", xlab="")
hist(error1k100150, main="N=1000, sig=1, pb=1.5", xlab="")
hist(error1k100200, main="N=1000, sig=1, pb=2", xlab="")

par(mfrow=c(2,3))
hist(error1k200001, main="N=1000, sig=2, pb=0.01", xlab="") #also here
hist(error1k200010, main="N=1000, sig=2, pb=0.1", xlab="")
hist(error1k200050, main="N=1000, sig=2, pb=0.5", xlab="")
hist(error1k200100, main="N=1000, sig=2, pb=1", xlab="")
hist(error1k200150, main="N=1000, sig=2, pb=1.5", xlab="")
hist(error1k200200, main="N=1000, sig=2, pb=2", xlab="")

par(mfrow=c(2,3))
hist(error1k500001, main="N=1000, sig=5, pb=0.01", xlab="")
hist(error1k500010, main="N=1000, sig=5, pb=0.1", xlab="")
hist(error1k500050, main="N=1000, sig=5, pb=0.5", xlab="")
hist(error1k500100, main="N=1000, sig=5, pb=1", xlab="")
hist(error1k500150, main="N=1000, sig=5, pb=1.5", xlab="")
hist(error1k500200, main="N=1000, sig=5, pb=2", xlab="")

par(mfrow=c(2,3))
hist(error1k1000001, main="N=1000, sig=10, pb=0.01", xlab="")
hist(error1k1000010, main="N=1000, sig=10, pb=0.1", xlab="")
hist(error1k1000050, main="N=1000, sig=10, pb=0.5", xlab="")
hist(error1k1000100, main="N=1000, sig=10, pb=1", xlab="")
hist(error1k1000150, main="N=1000, sig=10, pb=2", xlab="")
hist(error1k1000200, main="N=1000, sig=10, pb=5", xlab="")

#hist(error5k25)
#hist(error5k50)
#hist(error5k100)
#hist(error5k200)
#hist(error5k500)
#hist(error5k1000)
#hist(error20k25)
#hist(error20k50)
#hist(error20k100)
#hist(error20k200)
#hist(error20k500)
#hist(error20k1000)

