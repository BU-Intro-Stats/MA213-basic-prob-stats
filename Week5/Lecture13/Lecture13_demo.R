# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Binomial Distribution ----

samples1 <- rbinom(n=100, size=10, prob=0.5)
head(samples1)
mean(samples1)
sd(samples1)**2

# What do the parameters mean? What underlying Bernoulli distribution do they
# correspond to?