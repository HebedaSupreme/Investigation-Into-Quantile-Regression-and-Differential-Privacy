#File name: Differential Privacy and Quantile Regression Sensitivity of Estimator (Normal Case)
#Purpose: Measuring amount of change of quantile regression estimator by adding a data point (sensitivity of statistic)
#Description:
#Author:
#Date:

ErrorComp <- function(data1, data2, q) {
  sum=0
  
  for(j in 1:q) {
   sum=sum+(data1[j]-data2[j])^2
  }
  
  return(sqrt(sum))  
}

SensEst <- function(ss=1000, sigma=0.25, niter=10000, h=100) {
  
  p = 5
  resultdiff = matrix(0, niter, 1)
  
  for (i in 1:niter) {
    x=matrix(rnorm(ss*p, 0, 1), nrow=ss)
    #print(dim(x))
    y=x[,1]+x[,2]-x[,3]+2*x[,4]-x[,5]+rnorm(nrow(x), 0, sigma)
    #print(length(y))
    rqmodel <- rq(y~x, 0.5)
    result = summary(rqmodel)$coefficients[2:6,1]
    y0=h+h-h+2*h-h+rnorm(1, 0, sigma)
    #print(length(y0))
    y1=c(y,y0)
    x0=matrix(h, 1, 5)
    x1=rbind(x, x0)
    #print(dim(x1))
    #print(length(y1))
    rqmodel1 <- rq(y1~x1, 0.5)
    result1 = summary(rqmodel1)$coefficients[2:6,1]
    
    resultdiff[i]= ErrorComp(result, result1, p)
  }
  
  
  return(resultdiff)
  
}


res1k25h0= SensEst(ss=1000, sigma=0.25, niter=10000, h=0)
res1k50h0= SensEst(ss=1000, sigma=.5, niter=10000,h=0)
res1k100h0= SensEst(ss=1000, sigma=1, niter=10000, h=0)
res1k200h0= SensEst(ss=1000, sigma=2, niter=10000, h=0)
res1k500h0= SensEst(ss=1000, sigma=5, niter=10000, h=0)
res1k1000h0= SensEst(ss=1000, sigma=10, niter=10000,h=0)

res1k25h2 = SensEst(ss=1000, sigma=0.25, niter=10000, h=100)
res1k50h2 = SensEst(ss=1000, sigma=.5, niter=10000,h=100)
res1k100h2 = SensEst(ss=1000, sigma=1, niter=10000, h=100)
res1k200h2 = SensEst(ss=1000, sigma=2, niter=10000, h=100)
res1k500h2 = SensEst(ss=1000, sigma=5, niter=10000, h=100)
res1k1000h2 = SensEst(ss=1000, sigma=10, niter=10000,h=100)

res1k25h2n = SensEst(ss=1000, sigma=0.25, niter=10000, h=-100)
res1k50h2n = SensEst(ss=1000, sigma=.5, niter=10000,h=-100)
res1k100h2n = SensEst(ss=1000, sigma=1, niter=10000, h=-100)
res1k200h2n = SensEst(ss=1000, sigma=2, niter=10000, h=-100)
res1k500h2n = SensEst(ss=1000, sigma=5, niter=10000, h=-100)
res1k1000h2n = SensEst(ss=1000, sigma=10, niter=10000,h=-100)

res1k25h1an = SensEst(ss=1000, sigma=0.25, niter=10000, h=-25)
res1k50h1an = SensEst(ss=1000, sigma=.5, niter=10000,h=-25)
res1k100h1an = SensEst(ss=1000, sigma=1, niter=10000, h=-25)
res1k200h1an = SensEst(ss=1000, sigma=2, niter=10000, h=-25)
res1k500h1an = SensEst(ss=1000, sigma=5, niter=10000, h=-25)
res1k1000h1an = SensEst(ss=1000, sigma=10, niter=10000,h=-25)

res1k25h1bn = SensEst(ss=1000, sigma=0.25, niter=10000, h=-50)
res1k50h1bn = SensEst(ss=1000, sigma=.5, niter=10000,h=-50)
res1k100h1bn = SensEst(ss=1000, sigma=1, niter=10000, h=-50)
res1k200h1bn = SensEst(ss=1000, sigma=2, niter=10000, h=-50)
res1k500h1bn = SensEst(ss=1000, sigma=5, niter=10000, h=-50)
res1k1000h1bn = SensEst(ss=1000, sigma=10, niter=10000,h=-50)

res1k25h1cn = SensEst(ss=1000, sigma=0.25, niter=10000, h=-75)
res1k50h1cn = SensEst(ss=1000, sigma=.5, niter=10000,h=-75)
res1k100h1cn = SensEst(ss=1000, sigma=1, niter=10000, h=-75)
res1k200h1cn = SensEst(ss=1000, sigma=2, niter=10000, h=-75)
res1k500h1cn = SensEst(ss=1000, sigma=5, niter=10000, h=-75)
res1k1000h1cn = SensEst(ss=1000, sigma=10, niter=10000,h=-75)

res1k25h2a = SensEst(ss=1000, sigma=0.25, niter=10000, h=300)
res1k50h2a = SensEst(ss=1000, sigma=.5, niter=10000,h=300)
res1k100h2a = SensEst(ss=1000, sigma=1, niter=10000, h=300)
res1k200h2a = SensEst(ss=1000, sigma=2, niter=10000, h=300)
res1k500h2a = SensEst(ss=1000, sigma=5, niter=10000, h=300)
res1k1000h2a = SensEst(ss=1000, sigma=10, niter=10000,h=300)

res1k25h2b = SensEst(ss=1000, sigma=0.25, niter=10000, h=200)
res1k50h2b = SensEst(ss=1000, sigma=.5, niter=10000,h=200)
res1k100h2b = SensEst(ss=1000, sigma=1, niter=10000, h=200)
res1k200h2b = SensEst(ss=1000, sigma=2, niter=10000, h=200)
res1k500h2b = SensEst(ss=1000, sigma=5, niter=10000, h=200)
res1k1000h2b = SensEst(ss=1000, sigma=10, niter=10000,h=200)

res1k25h1a = SensEst(ss=1000, sigma=0.25, niter=10000, h=25)
res1k50h1a = SensEst(ss=1000, sigma=.5, niter=10000,h=25)
res1k100h1a = SensEst(ss=1000, sigma=1, niter=10000, h=25)
res1k200h1a = SensEst(ss=1000, sigma=2, niter=10000, h=25)
res1k500h1a = SensEst(ss=1000, sigma=5, niter=10000, h=25)
res1k1000h1a = SensEst(ss=1000, sigma=10, niter=10000,h=25)

res1k25h1b = SensEst(ss=1000, sigma=0.25, niter=10000, h=50)
res1k50h1b = SensEst(ss=1000, sigma=.5, niter=10000,h=50)
res1k100h1b = SensEst(ss=1000, sigma=1, niter=10000, h=50)
res1k200h1b = SensEst(ss=1000, sigma=2, niter=10000, h=50)
res1k500h1b = SensEst(ss=1000, sigma=5, niter=10000, h=50)
res1k1000h1b = SensEst(ss=1000, sigma=10, niter=10000,h=50)

res1k25h1c = SensEst(ss=1000, sigma=0.25, niter=10000, h=75)
res1k50h1c = SensEst(ss=1000, sigma=.5, niter=10000,h=75)
res1k100h1c = SensEst(ss=1000, sigma=1, niter=10000, h=75)
res1k200h1c = SensEst(ss=1000, sigma=2, niter=10000, h=75)
res1k500h1c = SensEst(ss=1000, sigma=5, niter=10000, h=75)
res1k1000h1c = SensEst(ss=1000, sigma=10, niter=10000,h=75)

res1k25h3 = SensEst(ss=1000, sigma=0.25, niter=10000, h=1000)
res1k50h3 = SensEst(ss=1000, sigma=.5, niter=10000,h=1000)
res1k100h3 = SensEst(ss=1000, sigma=1, niter=10000, h=1000)
res1k200h3 = SensEst(ss=1000, sigma=2, niter=10000, h=1000)
res1k500h3 = SensEst(ss=1000, sigma=5, niter=10000, h=1000)
res1k1000h3 = SensEst(ss=1000, sigma=10, niter=10000,h=1000)

res1k25h4 = SensEst(ss=1000, sigma=0.25, niter=10000, h=10000)
res1k50h4 = SensEst(ss=1000, sigma=.5, niter=10000,h=10000)
res1k100h4 = SensEst(ss=1000, sigma=1, niter=10000, h=10000)
res1k200h4 = SensEst(ss=1000, sigma=2, niter=10000, h=10000)
res1k500h4 = SensEst(ss=1000, sigma=5, niter=10000, h=10000)
res1k1000h4 = SensEst(ss=1000, sigma=10, niter=10000,h=10000)

par(mfrow=c(2,3))
hist(res1k25h2n, main="N=1000, sig=0.25, h=-100", xlab="")
hist(res1k50h2n, main="N=1000, sig=0.5, h=-100", xlab="")
hist(res1k100h2n, main="N=1000, sig=1, h=-100", xlab="")
hist(res1k200h2n, main="N=1000, sig=2, h=-100", xlab="")
hist(res1k500h2n, main="N=1000, sig=5, h=-100", xlab="")
hist(res1k1000h2n, main="N=1000, sig=10, h=-100", xlab="")

par(mfrow=c(2,3))
hist(res1k25h1an, main="N=5000, sig=0.25, h=-25", xlab="")
hist(res1k50h1an, main="N=5000, sig=0.5, h=-25", xlab="")
hist(res1k100h1an, main="N=5000, sig=1, h=-25", xlab="")
hist(res1k200h1an, main="N=5000, sig=2, h=-25", xlab="")
hist(res1k500h1an, main="N=5000, sig=5, h=-25", xlab="")
hist(res1k1000h1an, main="N=5000, sig=10, h=-25", xlab="")

par(mfrow=c(2,3))
hist(res1k25h1bn, main="N=20000, sig=0.25, h=-50", xlab="")
hist(res1k50h1bn, main="N=20000, sig=0.5, h=-50", xlab="")
hist(res1k100h1bn, main="N=20000, sig=1, h=-50", xlab="")
hist(res1k200h1bn, main="N=20000, sig=2, h=-50", xlab="")
hist(res1k500h1bn, main="N=20000, sig=5, h=-50", xlab="")
hist(res1k1000h1bn, main="N=20000, sig=10, h=-50", xlab="")

par(mfrow=c(2,3))
hist(res1k25h1cn, main="N=20000, sig=0.25, h=-75", xlab="")
hist(res1k50h1cn, main="N=20000, sig=0.5, h=-75", xlab="")
hist(res1k100h1cn, main="N=20000, sig=1, h=-75", xlab="")
hist(res1k200h1cn, main="N=20000, sig=2, h=-75", xlab="")
hist(res1k500h1cn, main="N=20000, sig=5, h=-75", xlab="")
hist(res1k1000h1cn, main="N=20000, sig=10, h=-75", xlab="")


par(mfrow=c(2,3))
hist(res1k25h0, main="N=1000, sig=0.25, h=0", xlab="")
hist(res1k50h0, main="N=1000, sig=0.5, h=0", xlab="")
hist(res1k100h0, main="N=1000, sig=1, h=0", xlab="")
hist(res1k200h0, main="N=1000, sig=2, h=0", xlab="")
hist(res1k500h0, main="N=1000, sig=5, h=0", xlab="")
hist(res1k1000h0, main="N=1000, sig=10, h=0", xlab="")

par(mfrow=c(2,3))
hist(res1k25h2, main="N=1000, sig=0.25, h=100", xlab="")
hist(res1k50h2, main="N=1000, sig=0.5, h=100", xlab="")
hist(res1k100h2, main="N=1000, sig=1, h=100", xlab="")
hist(res1k200h2, main="N=1000, sig=2, h=100", xlab="")
hist(res1k500h2, main="N=1000, sig=5, h=100", xlab="")
hist(res1k1000h2, main="N=1000, sig=10, h=100", xlab="")

par(mfrow=c(2,3))
hist(res1k25h1a, main="N=5000, sig=0.25, h=25", xlab="")
hist(res1k50h1a, main="N=5000, sig=0.5, h=25", xlab="")
hist(res1k100h1a, main="N=5000, sig=1, h=25", xlab="")
hist(res1k200h1a, main="N=5000, sig=2, h=25", xlab="")
hist(res1k500h1a, main="N=5000, sig=5, h=25", xlab="")
hist(res1k1000h1a, main="N=5000, sig=10, h=25", xlab="")

par(mfrow=c(2,3))
hist(res1k25h1b, main="N=20000, sig=0.25, h=50", xlab="")
hist(res1k50h1b, main="N=20000, sig=0.5, h=50", xlab="")
hist(res1k100h1b, main="N=20000, sig=1, h=50", xlab="")
hist(res1k200h1b, main="N=20000, sig=2, h=50", xlab="")
hist(res1k500h1b, main="N=20000, sig=5, h=50", xlab="")
hist(res1k1000h1b, main="N=20000, sig=10, h=50", xlab="")

par(mfrow=c(2,3))
hist(res1k25h1c, main="N=20000, sig=0.25, h=75", xlab="")
hist(res1k50h1c, main="N=20000, sig=0.5, h=75", xlab="")
hist(res1k100h1c, main="N=20000, sig=1, h=75", xlab="")
hist(res1k200h1c, main="N=20000, sig=2, h=75", xlab="")
hist(res1k500h1c, main="N=20000, sig=5, h=75", xlab="")
hist(res1k1000h1c, main="N=20000, sig=10, h=75", xlab="")

par(mfrow=c(2,3))
hist(res1k25h2b, main="N=1000, sig=0.25, h=200", xlab="")
hist(res1k50h2b, main="N=1000, sig=0.5, h=200", xlab="")
hist(res1k100h2b, main="N=1000, sig=1, h=200", xlab="")
hist(res1k200h2b, main="N=1000, sig=2, h=200", xlab="")
hist(res1k500h2b, main="N=1000, sig=5, h=200", xlab="")
hist(res1k1000h2b, main="N=1000, sig=10, h=200", xlab="")

par(mfrow=c(2,3))
hist(res1k25h2a, main="N=1000, sig=0.25, h=300", xlab="")
hist(res1k50h2a, main="N=1000, sig=0.5, h=300", xlab="")
hist(res1k100h2a, main="N=1000, sig=1, h=300", xlab="")
hist(res1k200h2a, main="N=1000, sig=2, h=300", xlab="")
hist(res1k500h2a, main="N=1000, sig=5, h=300", xlab="")
hist(res1k1000h2a, main="N=1000, sig=10, h=300", xlab="")

par(mfrow=c(2,3))
hist(res1k25h3, main="N=1000, sig=0.25, h=1000", xlab="")
hist(res1k50h3, main="N=1000, sig=0.5, h=1000", xlab="")
hist(res1k100h3, main="N=1000, sig=1, h=1000", xlab="")
hist(res1k200h3, main="N=1000, sig=2, h=1000", xlab="")
hist(res1k500h3, main="N=1000, sig=5, h=1000", xlab="")
hist(res1k1000h3, main="N=1000, sig=10, h=1000", xlab="")

par(mfrow=c(2,3))
hist(res1k25h4, main="N=1000, sig=0.25, h=10000", xlab="")
hist(res1k50h4, main="N=1000, sig=0.5, h=10000", xlab="")
hist(res1k100h4, main="N=1000, sig=1, h=10000", xlab="")
hist(res1k200h4, main="N=1000, sig=2, h=10000", xlab="")
hist(res1k500h4, main="N=1000, sig=5, h=10000", xlab="")
hist(res1k1000h4, main="N=1000, sig=10, h=10000", xlab="")
