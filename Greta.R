install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
install.packages("greta")
install.packages("DiagrammeR")
install.packages("bayesplot")
library(rethinking)
library(greta)
library(DiagrammeR)
library(bayesplot)
library(ggplot2)

# Example from section 8.3 Statistical Rethinking
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]
dd_trim <- dd[ , c("log_gdp","rugged","cont_africa")]
head(dd_trim)

set.seed(1234)

#Set up Tensorflow structures
#data
g_log_gdp <- as_data(dd_trim$log_gdp)
g_rugged <- as_data(dd_trim$rugged)
g_cont_africa <- as_data(dd_trim$cont_africa)

# Variables and Priors

a <- normal(0, 100)
bR <- normal(0, 10)
bA <- normal(0, 10)
bAR <- normal(0,10)
sigma <- cauchy(0,2,truncation=c(0,Inf))

mu <- a + bR*g_rugged + bA*g_cont_africa + bAR*g_rugged*g_cont_africa
dim(mu)

# likelihood
distribution(g_log_gdp) = normal(mu, sigma)

#The model() function does all of the work. It fits the model and produces a fairly complicated object organized as three lists that contain, respectively, the R6 class, TensorFlow structures, and the various greta data arrays.

# defining the model
mod <- model(a,bR,bA,bAR,sigma)

str(mod,give.attr=FALSE,max.level=1)

#Plotting mod produces the TensorFlow flow diagram that shows the structure of the underlying TensorFlow model, which is simple for this model and easily interpretable.

# plotting
plot(mod)

#Next, we use the greta function mcmc() to sample from the posterior distributions defined in the model.

# sampling
draws <- mcmc(mod, n_samples = 1000)
summary(draws)

mat <- data.frame(matrix(draws[[1]],ncol=5))
names(mat) <- c("a","bR","bA","bAR","sigma")

ggplot(mat, aes(x=bAR)) + 
  geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  

mcmc_trace(draws)

mcmc_intervals(draws)
