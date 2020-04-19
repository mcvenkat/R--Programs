#Benford Distribution
install.packages("VGAM")
library(VGAM)
dbenf(c(1:9))

#Bernoulli Distribution
n <- 1000
x <- sample(c(0,1), n, replace=T)
x <- sample(c(0,1), n, replace=T, prob=c(0.3,0.7))
x <- runif(n) > 0.3
x <- rbinom(n, size=1, prob=0.2)
x

#Binomial Distribution
x<-rbinom(n=100,size=10,prob=0.5)
x

#Hypergeometric Distribution
x <- rhyper(n=1000, 15, 5, 5)
x

#Geometric Distribution
n<-10000
x<- rgeom(n, .5)
x
x<- rgeom(n, .01)
x

#Multinomial Distribution
sample(1:6, 100, replace=T, prob= rep(1/6,6))

#Negative binomial distribution
n <- 100000
x <- rnbinom(n, 10, .25)
x

#Poisson distribution
x <- rpois(n=100, lambda=3)
x

#Zipf's law
library(VGAM)
dzipf(x=2, N=1000, s=2)

#Continuous Distributions
#Beta and Dirichlet Distributions
#Dirichlet Distribution
install.packages("gtools")
install.packages("MCMCpack")
install.packages("bayesm")
library(gtools)
?rdirichlet
library(bayesm)
?rdirichlet
library(MCMCpack)
?Dirichlet

density<- ddirichlet(c(.1,.2,.7), c(1,1,1))
draws <- rdirichlet(20, c(1,1,1) )
draws

#Beta
x <- seq(0, 1, length = 21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)

## Visualization, including limit cases:
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}
pl.beta(3,1)

pl.beta(2,4)
pl.beta(3,7)
pl.beta(3,7,asp=1)

pl.beta(0, 0)   ## point masses at  {0, 1}

pl.beta(0, 2)   ## point mass at 0 ; the same as
pl.beta(1, Inf)

pl.beta(Inf, 2) ## point mass at 1 ; the same as
pl.beta(3, 0)

pl.beta(Inf, Inf)# point mass at 1/2

#Cauchy Distribution
x <- rcauchy(n=100, location=0, scale=1)
x

#Chi Square Distribution
qchisq(.95,1)
qchisq(.95,10)
qchisq(.95,100)

#Exponential Distribution
x<-rexp(n=100,rate=1)
x

#Fisher-Snedecor Distribution
par(mar=c(3,3,1,1))
x <- seq(0,5,len=1000)
plot(range(x),c(0,2),type="n")
grid()
lines(x,df(x,df1=1,df2=1),col="black",lwd=3)
lines(x,df(x,df1=2,df2=1),col="blue",lwd=3)
lines(x,df(x,df1=5,df2=2),col="green",lwd=3)
lines(x,df(x,df1=100,df2=1),col="red",lwd=3)
lines(x,df(x,df1=100,df2=100),col="grey",lwd=3)
legend(2,1.5,legend=c("n1=1, n2=1","n1=2, n2=1","n1=5, n2=2","n1=100, n2=1","n1=100, n2=100"),col=c("black","blue","green","red","grey"),lwd=3,bty="n")

#Gamma Distribution
x<-rgamma(n=10, scale=1, shape=0.4)
x
x<-rgamma(n=100, scale=1, shape=0.8)
x
plot(x)

#Levy Distribution
x <- rlevy(n=100,s=1)
x

#Log Normal Distribution
x <- rlnorm(n=100, meanlog=0, sdlog=1)
x

#Normal and related distributions
x <- rnorm(n=100, mean=0, sd=1)
x
qnorm(0.95)
qnorm(.975)
qnorm(.99)

library(mvtnorm)
sig <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
r <- rmvnorm(1000, sigma = sig)
cor(r)

