# ---- 0. Setup and load libraries, if any ----
set.seed(213)

# ---- 1. Hypothesis Testing Example: Facebook survey ----

N = 850
p_hat = 0.67
survey_results <- c(rep("accurate", p_hat*N), rep("not accurate", (1-p_hat)*N))

# View the data
table(survey_results)

# How would you set up a hypothesis test for this example?

# Recall our confidence interval from last time:
alpha <- 0.05
SE <- sqrt(p_hat*(1-p_hat)/N)
p_hat + c(-1,1)*qnorm(1-alpha/2)*SE

# What can you conclude from the CI?


# ---- 2. Hypothesis test can sometimes give a wrong answer! ----

# Generate some new data with a true p=0.5
data = rbinom(n=10, size=10, p=0.5) 

# Imagine you didn't know what the true p was, and were just observing the data;
# How would you set up a hypothesis test for this example?

p_hat <- mean(data/10)
alpha <- 0.05
SE <- sqrt(p_hat*(1-p_hat)/N)
p_hat + c(-1,1)*qnorm(1-alpha/2)*SE

# What can you conclude from the CI?

