# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Calculate standard normal distribution probabilities ----

m = 3 # Mean 
s = 2 # Standard Deviation 

# Previously, we used dnorm() to generate the normal probability density.
x <- seq(-5,11,0.1)  # xaxis values

ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=m, sd=s), color='steelblue')

# Now, we can calculate probabilities using pnorm(), the distribution function
# for the Normal. 
# What is more likely (has a higher probability): X <= 8, or X <= 0.1?

pnorm(8, mean=m, sd=s)    # P(X <= 8)
pnorm(0.1, mean=m, sd=s)  # P(X <= 0.1)

# What else is likely (or unlikely)?

# What is the pnorm function doing?
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=pnorm, args=list(mean=m, sd=s), color='steelblue')

# ---- 2. Calculate other normal distribution probabilities ----

# Let's look at an extreme example:
m = 10
s = 1
pnorm(-5, mean=m, sd=s)  # P(X <= -5) when X is tightly concentrated around +10

x <- seq(-10,20,0.1)
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=m, sd=s), color='steelblue') +
  geom_vline(xintercept=-5, color='red')

