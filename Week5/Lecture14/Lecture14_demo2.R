# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Sampling from the Poisson distribution ----

samples1 <- rpois(n=100, lambda=3)  # Sample with given parameters

# Plot the same distribution
ggplot(data.frame(x=c(0:100)), aes(x)) +
  geom_point(aes(y=dpois(x, lambda=3)))  # dpois gives the density of the values in x

ggplot(data.frame(x=c(0:100)), aes(x)) +
  geom_point(aes(y=dpois(x, lambda=50)))

ggplot(data.frame(x=c(0:100)), aes(x)) +
  geom_point(aes(y=dpois(x, lambda=100)))
