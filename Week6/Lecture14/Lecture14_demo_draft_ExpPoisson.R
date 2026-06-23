# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Exponential samples -> Poisson process ---- 

# First, define the parameters for the exponential distribution
lambda <- 0.5
tmax <- 100  # set a maximum time interval

# Find the number of exponential rv's that can occur within the interval
n <- qpois(1 - 1e-8, lambda = lambda * tmax)

# Sample from the Exponential distribution, which simulates interarrival times
# of events within the interval (0,tmax)

X <- rexp(n = n, rate = lambda)
S <- c(0, cumsum(X))  # take the cumulative sum of all the realizations

# What does the cumulative sum represent?

ggplot(data.frame(S), aes(x=S, y=c(0:n))) +
  geom_step() +
  ylab("") +
  xlab("Inter-arrivals") +
  xlim(0, tmax)

# TODO: Lucia questions: does this adequately simulate the Exponential <> Poisson
# relationship? And, how would you want to add the Poisson distribution to this?