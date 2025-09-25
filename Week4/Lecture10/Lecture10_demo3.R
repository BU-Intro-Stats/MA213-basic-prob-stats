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
  stat_function(fun=dnorm, args=list(mean=m, sd=s), n=1000, color='steelblue')

# Now, we can calculate probabilities using pnorm(), the distribution function
# for the Normal. 
# What is more likely (has a higher probability): X <= 8, or X <= 0.1?

pnorm(8, mean=m, sd=s)    # P(X <= 8)
pnorm(0.1, mean=m, sd=s)  # P(X <= 0.1)

# What else is likely (or unlikely)?

# What is the pnorm function doing?
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=pnorm, args=list(mean=m, sd=s), n=1000, color='steelblue')


###### Heinz Quality Control problem
m = 36
s = 0.11

# What percent of bottles have less than 35.8 oz of Ketchup?
pnorm(35.8, m, s)
# OR, compute the percentile using the z score
pnorm((35.8-m)/s, 0, 1)
pnorm(-1.82, 0, 1)

# Pass if 35.8<X<36.2
# What percent of bottles pass the quality control inspection?
pnorm(36.2, m, s) - pnorm(35.8, m, s)

