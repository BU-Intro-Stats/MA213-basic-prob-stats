# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

# ---- 1. Geometric Distribution ----

# Sample from Geom(n=100, p=0.5)
samples1 <- rgeom(n=100, p=0.5)
head(samples1)
mean(samples1)
sd(samples1)**2

# What do the parameters mean? What underlying Bernoulli distribution do they
# correspond to?

# What if we decrease the success probability - what do you think will happen
# to the sample outcomes?

samples2 <- rgeom(n=100, p=0.0001)
head(samples2)
mean(samples2)
sd(samples2)**2
