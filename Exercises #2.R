#1) Draw the density of a standard normal distribution and add to the plot a vertical line to indicate the mean of this distribution. Then, 
#draw another plot, but this time of an exponential distribution with a rate of 1 and his mean
x.norm <- seq(-4, 4, 0.01)
plot(x.norm,dnorm(x.norm, mean=, sd=1))
abline(v=mean(rnorm(1000, mean=, sd=1)))

x.exp <- seq(, 5, 0.01)
plot(x.exp ,dexp(x.exp , rate=1))
abline(v=mean(rexp(1000 ,rate=1)))

#2) Generate 500 points from an exponential distribution with a rate of 0.5. 
#Draw the histogram of the sample and compute the sample mean of this distribution. 
#Then write a function that repeat this process for n iterations, 
#store the sample mean in a vector and return this vector. 
#Use this function to compute 10,000 sample means, 
#plot the histogram of the sample means and compute the mean of those estimations.
set.seed(42)
hist(rexp(500,rate=0.5))
data.2.2<-rexp(500,rate=0.5)
print(mean(data.2.2))
sample.mean.exp<-function(n)
{
  sample.mean<-matrix(NA)
  for (i in 1:n)
  {
    sample.mean<-c(sample.mean,mean(rexp(500,rate=0.5)))
  }
  return(sample.mean)
}
set.seed(42)
data.mean.exp<-sample.mean.exp(10000)
hist(data.mean.exp)
mean(data.mean.exp,na.rm=TRUE)

#3) Use the quantile() function to compute the 2.5 and 97.5 percentile from the sample of 
#estimations of the mean, then use the t.test() function to compute the confidence interval 
#with a level of 95% of the original distribution and compare those values.
quantile(data.mean.exp,c(0.025,0.975),na.rm=TRUE)
t.test(data.2.2)

#4) Load dataset and use the t.test() function to compute the confidence interval of 
#the mean for both variables with a level of 95%.
norm.data <-
  read.csv("http://www.r-exercises.com/wp-content/uploads/2017/06/hack_stat_2_norm.csv")
str(norm.data)
t.test(norm.data[,1])
t.test(norm.data[,2])

#5) Draw the density of a standard normal distribution and of a normal distribution of mean 
#equal to zero and with a standard deviation of 5 to see the effect of a change of variance
#on a density. 
x.norm<-seq(-10,10,0.1)
plot(x.norm,dnorm(x.norm))
lines(x.norm,dnorm(x.norm,sd=2),col="red")

#6) Use var.test() function on the dataset of exercise 4 three time, once with the alternative
#parameter set to "two.sided", then to "less" and finally to "greater". What is the signification
#of the three test?
var.test(norm.data[,1],norm.data[,2],alternative ="two.sided")
var.test(norm.data[,1],norm.data[,2],alternative ="less")           
var.test(norm.data[,1],norm.data[,2],alternative ="greater")

#7) Generate 200 points from a log-normal distribution with a parameter meanlog = 0 and 
#sdlog = 0.5. Then plot the histogram of those points and represent the mean and the median
#of this sample by two vertical lines.
log.normal.data<-rlnorm(200,meanlog=,sdlog=0.5)
hist(log.normal.data)
abline(v=mean(log.normal.data),col="red")
abline(v=median(log.normal.data),col="blue")

#8) Compute the median, the quantile and the 5 and 95% percentile on the variables of the dataset
#of exercise 4. Then compute the interquartile range which is the difference between the 25% and
#the 75% quartile. Does those statistics suggest that the two samples have the same distribution?
quantile(norm.data[,1],0.5)
quantile(norm.data[,2],0.5)
quantile(norm.data[,1],c(0.25,0.5,0.75))
quantile(norm.data[,2],c(0.25,0.5,0.75))
quantile(norm.data[,1],c(0.05,0.95))
quantile(norm.data[,2],c(0.25,0.5,0.75))
quantile(norm.data[,1],0.75)-quantile(norm.data[,1],0.25)
quantile(norm.data[,2],0.75)-quantile(norm.data[,2],0.25)

#9) Load the moment package and use the skewness() function to compute the skewness 
#of three samples you must create:
#.	150 points sample from a standard normal distribution
#.	1000 points sample from a standard normal distribution
#.	200 points sample from a exponential distribution with a rate of 5
install.packages("moments")
library(moments)
data.9.1<-rnorm(150)
skewness(data.9.1)
data.9.2<-rnorm(1000)
skewness(data.9.2)
data.9.3<-rexp(200,rate=5)
skewness(data.9.3)

#10) Use the kurtosis() function to compute the kurtosis of those samples:
#.	500 points sample from a standard normal distribution
#.	500 points sample from a exponential distribution with a rate of 5
#.	500 points sample from uniform distribution
data.10.1<-rnorm(500)
kurtosis(data.10.1)
data.10.2<-rexp(500,rate=5)
kurtosis(data.10.2)
data.10.3<-runif(500)
kurtosis(data.10.3)