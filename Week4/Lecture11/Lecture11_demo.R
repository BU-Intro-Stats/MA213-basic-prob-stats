# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. 68-95-99.5 (Empirical) Rule for Standard Normal ----

# Areas under the curve
pnorm(1) - pnorm(-1)  # standard normal, +- 1 sd (~.68)

pnorm(2) - pnorm(-2)  # standard normal, +- 2 sd's (~0.95)

pnorm(3) - pnorm(-3)  # standard normal, +- 3 sd's (~0.99)

x <- seq(-5,5,0.1)
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue') +
  geom_vline(xintercept=1, color='red', linetype='dotted') +
  geom_vline(xintercept=-1, color='red', linetype='dotted') +
  geom_vline(xintercept=2, color='red', linetype='dotted') +
  geom_vline(xintercept=-2, color='red', linetype='dotted') +
  geom_vline(xintercept=3, color='red', linetype='dotted') +
  geom_vline(xintercept=-3, color='red', linetype='dotted') +
  scale_x_continuous(breaks=c(-5:5))


# ---- 2. Adding Variability ----

# Now let's change the sd and figure out the new cutoffs:
# If we have N(mean=0, sd=3), then +- 1 sd is at +-3;
# +- 2 sd's is at +- 6; and +- 3 sd's is at +-9

# Areas under the curve
pnorm(3, mean=0, sd=3) - pnorm(-3, mean=0, sd=3)  # ~0.68
pnorm(6, mean=0, sd=3) - pnorm(-6, mean=0, sd=3)  # ~0.95
pnorm(9, mean=0, sd=3) - pnorm(-9, mean=0, sd=3)  # ~0.99

x <- seq(-10,10,0.1)
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=3), color='steelblue') +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='gray', alpha=0.8) +
  geom_vline(xintercept=3, color='red', linetype='dotted') +
  geom_vline(xintercept=-3, color='red', linetype='dotted') +
  geom_vline(xintercept=6, color='red', linetype='dotted') +
  geom_vline(xintercept=-6, color='red', linetype='dotted') +
  geom_vline(xintercept=9, color='red', linetype='dotted') +
  geom_vline(xintercept=-9, color='red', linetype='dotted') +
  scale_x_continuous(breaks=c(-10:10))

