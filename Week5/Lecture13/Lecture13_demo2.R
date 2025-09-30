# Demo: Binomial PMF approaches normal as n increases

set.seed(123)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Sample from binomial with increasing n, plot histograms using ggplot2
n_vals <- c(10, 30, 100)
p <- 0.1
size <- 10000

samples_df <- data.frame()
for (n in n_vals) {
  temp <- data.frame(
    successes = rbinom(size, n, p),
    n = factor(n, levels = n_vals)
  )
  samples_df <- rbind(samples_df, temp)
}

ggplot(samples_df, aes(x = successes)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = NA, color = "darkblue") +
  facet_wrap(~ n, scales = "free_x") +
  labs(title = "Binomial samples for increasing n",
       x = "Number of successes", y = "Density")

# 2. Plot binomial PMF and overlay normal PDF for increasing n using ggplot2
pmf_df <- data.frame()
for (n in n_vals) {
  x <- 0:n
  # compute the binomial PMF
  pmf <- dbinom(x, n, p)
  
  # compute the mean and standard deviation of the binomial
  mu <- n * p # Mean of a binomial
  sigma <- sqrt(n * p * (1 - p)) # standard deviation of a binomial
  
  # compute the pdf of a Normal with those mean, stdev
  x_norm <- seq(min(x), max(x), length.out = 200)
  norm_pdf <- dnorm(x_norm, mean = mu, sd = sigma)
  
  # save it all in dataframes
  temp_pmf <- data.frame(
    x = x,
    prob = pmf,
    type = "Binomial PMF",
    n = factor(n, levels = n_vals),
    mu = mu,
    sigma = sigma
  )
  temp_norm <- data.frame(
    x = x_norm,
    prob = norm_pdf,
    type = "Normal PDF",
    n = factor(n, levels = n_vals),
    mu = mu,
    sigma = sigma
  )
  pmf_df <- rbind(pmf_df, temp_pmf, temp_norm)
}

# Plot them
ggplot(pmf_df, aes(x = x, y = prob, color = type, fill = type)) +
  geom_bar(
    data = subset(pmf_df, type == "Binomial PMF"),
    stat = "identity", position = "identity", alpha = 0.7, color = "darkblue", fill = "blue"
  ) +
  geom_line(
    data = subset(pmf_df, type == "Normal PDF"),
    size = 1.2, color = "red"
  ) +
  facet_wrap(~ n, scales = "free_x") +
  labs(title = "Binomial PMF (bars) vs Normal PDF (line) for increasing n",
       x = "Number of successes", y = "Probability") +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()