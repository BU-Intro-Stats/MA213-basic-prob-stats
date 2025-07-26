# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Binomial distribution with large n and small p ----

samples1 <- rbinom(n=1000, size=1000, prob=0.01)

# What do the parameters mean? What underlying Bernoulli distribution do they
# correspond to?

ggplot(data=data.frame(samples1), aes(x=samples1)) +
  geom_histogram() +
  xlab("samples")


# ---- 2. Poisson limiting behavior ----

# How would you calculate the lambda parameter for the Poisson distribution,
# given the Binomial samples we took above?

lambda = 1000*0.01

ggplot(data=data.frame(samples1), aes(x=samples1)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=0.5) +
  stat_function(fun=dpois, args=list(lambda=lambda), color='steelblue') +
  xlab("samples")

# FIXME: Poisson density doesn't show on histogram
