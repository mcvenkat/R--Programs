#Exercise 1

#1) Use rnorm() to generate 100 points, then plot those points in an histogram.

set.seed(42)
data.1<-rnorm(100,mean=,sd=1)
hist(data.1)

#2) Repeat exercise 1, but this time with, 500, 1000 and 10000 points.
set.seed(42)
data.2<-rnorm(500,mean=,sd=1)
hist(data.2)

set.seed(42)
data.3<-rnorm(1000,mean=,sd=1)
hist(data.3)

set.seed(42)
data.4<-rnorm(10000,mean=,sd=1)
hist(data.4)

#3) Use the dnorm() function to plot the density function of a normal distribution of mean 0 and 
#standard deviation of 1 and add it to the last histogram you plot.

x <- seq(-4, 4, 0.01)
hist(data.4, prob=TRUE)
curve(dnorm(x, mean=, sd=1),add=TRUE)

#4) Plot the cumulative histogram of 10000 points from a standard normal distribution, 
#then add the ECDF curve to the plot by using the pnorm() function.

hist.cum.sum<-hist( data.4)
hist.cum.sum$counts<-cumsum(hist.cum.sum$counts)
plot(hist.cum.sum)
curve(pnorm(x, mean=, sd=1),add=FALSE)
curve(pnorm(x, mean=, sd=1),add=TRUE)

#5) plot the density function of those common functions:
#.	Exponential with a rate of 0.5
#.	Exponential with a rate of 1
#.	Exponential with a rate of 2
#.	Exponential with a rate of 10
#.	Gamma with a shape of 1 and a scale equal to 2
#.	Gamma with a shape of 2 and a scale equal to 2
#.	Gamma with a shape of 5 and a scale equal to 2
#.	Gamma with a shape of 5 and a scale equal to 0.5
#.	Student with 10 degree of freedom
#.	Student with 5 degree of freedom
#.	Student with 2 degree of freedom
#.	Student with 1 degree of freedom

interval.exp<-seq(, 10, 0.01)
plot(interval.exp,dexp(interval.exp,rate=0.5))
lines(interval.exp,dexp(interval.exp,rate=1),col="red")
lines(interval.exp,dexp(interval.exp,rate=2),col="blue")
lines(interval.exp,dexp(interval.exp,rate=10),col="green")
#Gamma
interval.gamma<-seq(, 20, 0.01)
plot(interval.gamma,dgamma(interval.gamma,shape=1,scale=2))
lines(interval.gamma,dgamma(interval.gamma,shape=2,scale=2),co
      l="red")
lines(interval.gamma,dgamma(interval.gamma,shape=5,scale=2),co
      l="blue")
lines(interval.gamma,dgamma(interval.gamma,shape=5,scale=0.5),
      col="green")
#Student
interval.t<-seq(-10,10, 0.01)
plot(interval.t,dt(interval.t,10))
lines(interval.t,dt(interval.t,5),col="red")
lines(interval.t,dt(interval.t,2),col="blue")
lines(interval.t,dt(interval.t,1),col="green")

#6) Repeat the steps of exercise 5, but plot the ECDF instead.
#Exponential
interval.exp<-seq(, 10, 0.01)
plot(interval.exp,pexp(interval.exp,rate=0.5))
lines(interval.exp,pexp(interval.exp,rate=1),col="red")
lines(interval.exp,pexp(interval.exp,rate=2),col="blue")
lines(interval.exp,pexp(interval.exp,rate=10),col="green")
#Gamma
interval.gamma<-seq(, 20, 0.01)
plot(interval.gamma,pgamma(interval.gamma,shape=1,scale=2))
lines(interval.gamma,pgamma(interval.gamma,shape=2,scale=2),co
      l="red")
lines(interval.gamma,pgamma(interval.gamma,shape=5,scale=2),co
      l="blue")
lines(interval.gamma,pgamma(interval.gamma,shape=5,scale=0.5),
      col="green")
#Student
interval.t<-seq(-10,10, 0.01)
plot(interval.t,pt(interval.t,10))
lines(interval.t,pt(interval.t,5),col="red")
lines(interval.t,pt(interval.t,2),col="blue")
lines(interval.t,pt(interval.t,1),col="green")

#7) Histograms
hist(df[,1])
hist(df[,2])

#8) ECDF of histograms
ecdf.v1<-hist(df[,1])
ecdf.v1$counts<-cumsum(ecdf.v1$counts)
plot(ecdf.v1)

ecdf.v2<-hist(df[,2])
ecdf.v2$counts<-cumsum(ecdf.v2$counts)
plot(ecdf.v2)

#9) Plot the empirical cumulative distribution function (ECDF)
plot(ecdf(df[,1]),col="red")
plot(ecdf(df[,2]),col="blue",add=TRUE)

#10) Kolmogorov-Smirnov test 
ks.test(df[,1],df[,2])
