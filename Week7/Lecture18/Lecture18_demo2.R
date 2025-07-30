# ---- 0. Setup and load libraries, if any ----
set.seed(213)

# ---- 1. Type 1 Errors (False Positives) ----

# Simulate some data with a true p=0.5
data <- rbinom(n=100, size=25, p=0.5)

# Compute a CI with a significance level alpha=0.05

p_hat <- mean(data/50)
alpha <- 0.05  
SE <- sqrt(p_hat*(1-p_hat)/50)
p_hat + c(-1,1)*qnorm(1-alpha/2)*SE

# What can you conclude from the CI?
# What factors do you think affected the outcome of the hypothesis test?

# Now what if we tried changing the alpha value to 0.1?
# How will this affect the width of the CI compared to last time?
# What do you think the confidence level is, given this alpha value?

alpha <- 0.1 
p_hat + c(-1,1)*qnorm(1-alpha/2)*SE

# Has your conclusion from the hypothesis test changed?


# ---- 2. Type 2 Errors (False Negatives) ----
# Simulate some data with a true p=0.2 (quite low! our data will be skewed)

data <- rbinom(n=100, size=100, p=0.2)

# Compute a CI with a significance level alpha=0.05

p_hat <- mean(data/50)
alpha <- 0.05  
SE <- sqrt(p_hat*(1-p_hat)/50)
p_hat + c(-1,1)*qnorm(1-alpha/2)*SE

# What do you conclude this time? 
# What factors do you think led to this outcome?
