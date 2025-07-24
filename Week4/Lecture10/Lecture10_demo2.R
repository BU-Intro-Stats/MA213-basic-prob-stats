# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Calculate standard normal distribution probabilities ----

# Previously, we used dnorm() to generate the normal probability density.
# Now, we can calculate probabilities using pnorm(), the distribution function
# for the Normal. 

# Imagine we have a standard normal distribution (mean=0, sd=1). What is more
# likely (has a higher probability): X <= 8, or X <= 0.1?

pnorm(8)    # P(X <= 8)
pnorm(0.1)  # P(X <= 0.1)

# What else is likely (or unlikely)?


# ---- 2. Calculate other normal distribution probabilities ----

# Let's look at an extreme example:
pnorm(-5, mean=10, sd=1)  # P(X <= -5) when X is tightly concentrated around +10

x <- seq(-10,20,0.1)
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=10, sd=1), color='steelblue') +
  geom_vline(xintercept=-5, color='red')

