# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Continuous distributions: Normal ----
m = 3 # Mean parameter for Normal distribution
s = 2 # Standard Deviation for Normal distribution

# rnorm(n, mean=0, sd=1) samples n times from the distribution
#   the default mean is 0 and standard deviation is 1
# dnorm(x, mean=0, sd=1) evaluates the distribution (probability density function) 
#   x is the value(s) that you want to compute the distribution for
# ... this pattern (of d for distribution and r for random sample) will extend to other probability models

samples_norm_500 <- rnorm(500, mean=m, sd=s)
samples <- data.frame(samples=samples_norm_500)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.3) +
  scale_x_continuous(breaks=c((m-3*s):(m+3*s))) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Normal Distribution")

# plot the same thing, but with the theoretical distribution on top
ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.3) +
  stat_function(fun=dnorm, args=list(mean=m, sd=s), color="red", size=1) +
  scale_x_continuous(breaks=c((m-3*s):(m+3*s))) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Normal Distribution with Theoretical Density")


# ---- 1a. What happens when we increase the sample size? ----
samples_norm_10000 <- rnorm(10000, mean=m, sd=s)
samples <- data.frame(samples=samples_norm_10000)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.3) +
  stat_function(fun=dnorm, args=list(mean=m, sd=s), color="red", size=1) +
  scale_x_continuous(breaks=c((m-3*s):(m+3*s))) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Normal Distribution")

  
# ---- 1b. What happens when we decrease the bin size? ----

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.1) +
  stat_function(fun=dnorm, args=list(mean=m, sd=s), color="red", size=1) +
  scale_x_continuous(breaks=c((m-3*s):(m+3*s))) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Normal Distribution")


# ---- 2. Continuous distributions: Exponential ----
r = 2 # rate parameter for Exponential distribution

samples_exp_500 <- rexp(500, rate=r)
samples <- data.frame(samples=samples_exp_500)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  stat_function(fun=dexp, args=list(rate=r), color="red", size=1) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Exponential Distribution")

# Why does the 0 bin look small, even though the theoretical density is highest?
# --- the bin definition includes impossible negative values!
# replot, but with bins that don't include negative values

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), boundary = 0, alpha=0.6) +
  stat_function(fun=dexp, args=list(rate=r), color="red", size=1) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Exponential Distribution")


# ---- 2a. What happens when we increase the sample size? ----
samples_exp_10000 <- rexp(10000, r)
samples <- data.frame(samples=samples_exp_10000)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), boundary=0, alpha=0.6) +
  stat_function(fun=dexp, args=list(rate=r), color="red", size=1) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Exponential Distribution")

# ---- 2b. What happens when we decrease the bin size? ----
ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), boundary=0, binwidth=0.01, alpha=0.6) +
  stat_function(fun=dexp, args=list(rate=r), color="red", size=1) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Exponential Distribution")


# ---- 3. Continuous distributions: Uniform ----
lb = 50 # min parameter (lower bound) for uniform distribution
ub = 150 # max parameter (upper bound) for uniform distribution

samples_uni_500 <- runif(500, min=lb, max=ub)
samples <- data.frame(samples=samples_uni_500)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  stat_function(fun=dunif, args=list(min=lb, max=ub), color="red", size=1) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Uniform Distribution")

# What's happening outside of the range (lb, ub)?
ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  stat_function(fun=dunif, args=list(min=lb, max=ub), color="red", size=1, xlim=c(lb-10, ub+10)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Uniform Distribution")

# ---- 3a. What happens when we increase the sample size? ----
samples_uni_10000 <- runif(10000, min=lb, max=ub)
samples <- data.frame(samples=samples_uni_10000)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  stat_function(fun=dunif, args=list(min=lb, max=ub), color="red", size=1, xlim=c(lb-10, ub+10)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Uniform Distribution")
