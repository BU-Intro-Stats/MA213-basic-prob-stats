# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

# ---- 1. Geometric Distribution ----

# Just like with the Normal distribution we had rnorm (to sample) and dnorm (the 
# distribution or probability *density* function)
# For the Geometric Distribution we have rgeom (to sample) and dgeom (the 
# distribution or probability *mass* function)

# Sample from Geom(n=100, p=0.5)
samples1 <- rgeom(n=100, p=0.5)
head(samples1)
mean(samples1)
sd(samples1)

# What do the parameters mean? 

# What if we decrease the success probability - what do you think will happen
# to the sample outcomes?

samples2 <- rgeom(n=100, p=0.0001)
head(samples2)
mean(samples2)
sd(samples2)

# Plot the probability mass function using dgeom
p = 0.5
x <- 0:(round(1/p)*10)
y <- dgeom(x, prob = p)
df <- data.frame(x = x, y = y)

ggplot(df, aes(x = x, y = y)) +
  geom_col(fill = "steelblue") +
  labs(
    title = paste("Geometric Distribution PMF (p =", p, ")"),
    x = "Number of Failures before First Success",
    y = "Probability"
  )