# ---- 0. Setup and load libraries, if any ----

library(ggplot2)


# ---- 1. Guinness example: testing assumptions & sampling distributions ----

# Reminder: what is our null hypothesis in this example?

# Let's create a function to run experiments under the null hypothesis:

experiment <- function(N, mu_0, mu, sigma) {
  # Simulate generating samples under the null hypothesis:
  samples <- rnorm(N, mean=mu, sd=sigma) 
  
  Xbar <- mean(samples)          # take the sample mean
  Z <- (Xbar - mu_0) / sigma**2  # compute the Z score
  return(Z)
}

# 2. ---- Run the experiment 1000 times for N=100, mu_0=mu ----

data1 = as.data.frame(replicate(n=1000, experiment(100, 4.5, 4.5, 0.17)))
colnames(data1) <- c("Z")

# Does this histogram look familiar?
ggplot(data1, aes(x=Z)) +
  geom_histogram(aes(y=after_stat(density)), alpha=0.5, bins=20) +
  stat_function(fun=dnorm, args=list(mean=0.0, sd=1.0), col="blue") +
  scale_x_continuous(breaks=-3:3)

# TODO: ylim's don't seem to match for the histogram and the density -
# need to normalize in another way?


# 3. ---- Now run the experiment 1000 times for N=5, mu_0=mu ----

data2 = as.data.frame(replicate(n=1000, experiment(5, 4.5, 4.5, 0.17)))
colnames(data2) <- c("Z")

# Now what's wrong with this histogram?
ggplot(data2, aes(x=Z)) +
  geom_histogram(aes(y=after_stat(density)), alpha=0.5, bins=20) +
  stat_function(fun=dnorm, args=list(mean=0.0, sd=1.0), col="blue") +
  xlim(-3, 3)

# Looks like we'll need a new distribution!
