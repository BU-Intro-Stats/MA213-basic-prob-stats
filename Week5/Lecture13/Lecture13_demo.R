# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Binomial Distribution ----

samples1 <- rbinom(n=100, size=10, prob=0.5)
head(samples1)
mean(samples1)
sd(samples1)**2

# What do the parameters mean? What underlying Bernoulli distribution do they
# correspond to?

ggplot(data=data.frame(samples1), aes(x=samples1)) +
  geom_histogram() +
  xlab("samples")


# ---- 2. Small n ----

samples2 <- rbinom(n=6, size=10, prob=0.5)
head(samples2)
mean(samples2)
sd(samples2)**2

ggplot(data=data.frame(samples2), aes(x=samples2)) +
  geom_histogram() +
  xlab("samples")


# ---- 3. Normal approximation ----

samples3 <- rbinom(n=10000, size=10, prob=0.5)
head(samples3)
mean(samples3)
sd(samples3)**2

# How would you find the mean and variance for the normal approximation?

mean <- 10*0.5  # E[X] = np
var <- 10*0.5*(1-0.5)  # var(X) = np(1-p)

# Note that the histogram and density here have different areas (not scaled)...
ggplot(data=data.frame(samples3), aes(x=samples3)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=0.5) + 
  stat_function(fun=dnorm, args=list(mean=mean, sd=sqrt(var)), color='steelblue') +
  xlab("x")

# ... but if the bin width is set to 1, they line up better ...
ggplot(data=data.frame(samples3), aes(x=samples3)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=1) + 
  stat_function(fun=dnorm, args=list(mean=mean, sd=sqrt(var)), color='steelblue') +
  xlab("x")
# ... but we're still limited to x values between 0 and 10

# What if we change the size parameter?
samples4 <- rbinom(n=10000, size=100, prob=0.5)
mean <- 100*0.5  # E[X] = np
var <- 100*0.5*(1-0.5)  # var(X) = np(1-p)
ggplot(data=data.frame(samples4), aes(x=samples4)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=1) + 
  stat_function(fun=dnorm, args=list(mean=mean, sd=sqrt(var)), color='steelblue') +
  xlab("samples")
