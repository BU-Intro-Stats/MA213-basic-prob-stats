# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Confidence intervals for a proportion ----
N = 850
p_hat = 0.67
survey_results <- c(rep("accurate", p_hat*N), rep("not accurate", (1-p_hat)*N))

# View the data
table(survey_results)

# Set the CI significance level
alpha <- 0.05

# Compute the standard error 
SE <- sqrt(p_hat*(1-p_hat)/N)

# Compute the CI
p_hat + c(-1,1)*1.96*SE


# ---- 2. Simulate repeating the survey many times ----

samples <- rbinom(n=10000, size=850, prob=0.67)
p_hats <- samples / 850

SEs <- sqrt(p_hats*(1-p_hats)/850)
sim_intervals <- p_hats + c(-1,1)*1.96*SEs

min(sim_intervals)
max(sim_intervals)

# TODO: Was something like this the intention? 
