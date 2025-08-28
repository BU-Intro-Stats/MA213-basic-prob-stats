# ---- 0. Setup and load libraries, if any ----

library(ggplot2)

# ---- 1. Guinness example: using the t distribution ----

# Recall our experiments from before, using the function to compute Z scores,
# which failed with the Normal distribution when N was small

experiment <- function(N, mu_0, mu, sigma) {
  # Simulate generating samples under the null hypothesis:
  samples <- rnorm(N, mean=mu, sd=sigma) 
  
  Xbar <- mean(samples)          # take the sample mean
  Z <- (Xbar - mu_0) / sigma**2  # compute the Z score
  return(Z)
}

# Now let's try graphing the results with the t distribution instead

data = as.data.frame(replicate(n=1000, experiment(5, 4.5, 4.5, 0.17)))
colnames(data) <- c("t")

ggplot(data, aes(x=t)) +
  geom_histogram(aes(y=after_stat(density)), alpha=0.5, bins=20) +
  stat_function(fun=dt, args=list(df=7), col="blue")

# FIXME: doesn't look quite right

  
# ---- 2. Finding the p-value ----

# Q: (Review) What is the function pt() in R? Hint: what is pnorm()?
# Q: Why do we multiply by 2?
# Q: Why are we computing the upper tail probability?

Xbar <- 4.66
SE <- 0.17 / sqrt(8)
t <- (Xbar - 4.5) / SE

2 * pt(q=t, df=7, lower.tail=FALSE)

# What can we conclude from this p-value?
