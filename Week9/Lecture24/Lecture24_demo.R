# ---- 0. Setup and load libraries, if any ----

library(ggplot2)


# ---- 1a. Guinness example: testing assumptions & sampling distributions ----

# Let's design a hypothesis test assuming that Xbar~Normal(mu,s/sqrt(n))

# H0: mu=4.5
# HA: mu!=4.5
# Test statistic: z_obs = (Xbar-4.5)/(s/sqrt(n))

# If the normal model is correct, then z_obs~N(0,1) under the null hypothesis
# so let's simulate z_obs under the null hypothesis:

experiment <- function(N, mu, sigma) {
  # Simulate generating samples:
  samples <- rnorm(N, mean=mu, sd=sigma) 
  
  # Compute z_obs
  Xbar <- mean(samples)          # take the sample mean
  s <- sd(samples)               # estimate the standard deviation
  z_obs <- (Xbar - 4.5) / (s/sqrt(N))  # compute the Z score
  
  return(z_obs)
}

# 1b. ---- Run the experiment 50000 times for N=100 ----

data1 = as.data.frame(replicate(n=50000, experiment(100, 4.5, 0.17)))
colnames(data1) <- c("z_obs")

# Does this histogram look familiar?
ggplot(data1, aes(x=z_obs)) +
  geom_histogram(aes(y=after_stat(density)), alpha=0.5, bins=100) +
  stat_function(fun=dnorm, args=list(mean=0.0, sd=1.0), col="blue") +
  xlim(-5, 5)

# 1c. ---- Now run the experiment 50000 times for N=8 ----

data2 = as.data.frame(replicate(n=50000, experiment(8, 4.5, 0.17)))
colnames(data2) <- c("z_obs")

# Now what's wrong with this histogram?
ggplot(data2, aes(x=z_obs)) +
  geom_histogram(aes(y=after_stat(density)), alpha=0.5, bins=100) +
  stat_function(fun=dnorm, args=list(mean=0.0, sd=1.0), col="blue") +
  xlim(-5, 5)

# Looks like we'll need a new distribution!

# -----------------------------------------------------------
# ---- 2. Try graphing the results with the t distribution too

data = as.data.frame(replicate(n=50000, experiment(8, 4.5, 0.17)))
colnames(data) <- c("t")

ggplot(data, aes(x = t)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.5, bins = 100) +
  stat_function(aes(colour = "Normal"), fun = dnorm, args = list(mean = 0, sd = 1)) +
  stat_function(aes(colour = "t"), fun = dt, args = list(df = 7)) +
  scale_colour_manual(name = "", values = c("Normal" = "blue", "t" = "red")) +
  xlim(-5, 5)

# -----------------------------------------------------------
# ---- 3. Finding the p-value for the Guiness data ----

Xbar <- 4.66
SE <- 0.17 / sqrt(8)
t <- (Xbar - 4.5) / SE

# Using the normal distribution:
2 * pnorm(t, lower.tail = FALSE)

# Using the t distribution:
2 * pt(t, df=7, lower.tail=FALSE)

# How does the t distribution change the p-value? 
# How could that change our interpretation (e.g. depending on our alpha)?
