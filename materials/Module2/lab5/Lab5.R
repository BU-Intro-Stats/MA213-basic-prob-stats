############################################################
# MA 213 - Lab 5 Starter Code
# Sampling Distributions and the Central Limit Theorem
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the missing pieces first.
############################################################

library(dplyr)
library(ggplot2)


############################################################
# Question 1. Build your population
############################################################

set.seed(213)
sigma <- 0.447
N <- 100000

# Choose ONE population and comment out the others.
# pop <- rnorm(N, mean = 2, sd = sigma)
# pop <- rbinom(N, size = 1, prob = 0.3)
# pop <- rpois(N, lambda = sigma^2)
pop <- rexp(N, rate = 1 / sigma)
# pop <- rgamma(N, shape = 4, rate = sqrt(4) / sigma)

population_mean <- mean(pop)
population_sd <- sd(pop)

population_mean
population_sd

ggplot(data.frame(X = pop), aes(x = X)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    x = "Population value",
    y = "Count",
    title = "My hypothetical population"
  )

# Interpretation:
# - Which population did you choose?
# - Describe its shape, center, and spread.
# - How does it differ from your partner's population?


############################################################
# Question 2. Draw one sample
############################################################

n <- 5

my_sample <- sample(pop, size = n)
mu_hat <- mean(my_sample)

mu_hat
population_mean
mu_hat - population_mean

# Interpretation:
# - Does mu_hat equal the population mean exactly?
# - How close was your sample mean to the population mean?
# - How does your mu_hat compare with your partner's?


############################################################
# Question 3. Build a sampling distribution
############################################################

K <- 1000
n <- 5

mu_hat_vector <- rep(0, K)

# Fill in the blanks, then uncomment.

# for (_____ in _____) {
#   my_sample <- sample(pop, size = n)
#   mu_hat_vector[_____] <- mean(my_sample)
# }

head(mu_hat_vector)
mean(mu_hat_vector)
sd(mu_hat_vector)

ggplot(data.frame(mu_hat = mu_hat_vector), aes(x = mu_hat)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = population_mean, color = "red", linetype = "dashed") +
  labs(
    x = "Sample mean",
    y = "Count",
    title = "Sampling distribution of the sample mean",
    subtitle = "Red dashed line shows the population mean"
  )

# Interpretation:
# - Where is the sampling distribution centered? Is it centered near one
#   sample mean, or near the population mean?
# - Does your sampling distribution look the same as your partner's?
# - Describe the shape, center, and spread.


############################################################
# Question 4. Write a reusable simulation function
############################################################

# Fill in the blanks, then uncomment.

# get_mu_hats <- function(pop, n, K) {
#   mu_hat_vector <- rep(0, K)
#
#   # Repeat K times:
#   # take a sample of size n from the population pop
#   # compute its mean
#   # store the result as one element of mu_hat_vector
#   _________ # Fill this in
#
#   return(mu_hat_vector)
# }

result_q4 <- get_mu_hats(pop, n = 5, K = 1000)
head(result_q4)

# Interpretation:
# - How could you check whether result_q4 behaves like the vector from
#   Question 3?


############################################################
# Question 5. What does sample size n change?
############################################################

# Keep K = 1000 fixed and compare n = 5, 20, and 50.

# mu_hats_5 <- get_mu_hats(pop, n = 5, K = 1000)
# mu_hats_20 <- get_mu_hats(pop, n = 20, K = 1000)
# mu_hats_50 <- get_mu_hats(pop, n = 50, K = 1000)
#
# sampling_n <- data.frame(
#   mu_hat = c(mu_hats_5, mu_hats_20, mu_hats_50),
#   sample_size = factor(rep(c(5, 20, 50), each = 1000))
# )
#
# ggplot(sampling_n, aes(x = mu_hat)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   facet_wrap(~ sample_size, scales = "free_y") +
#   geom_vline(xintercept = population_mean, color = "red", linetype = "dashed") +
#   labs(
#     x = "Sample mean",
#     y = "Count",
#     title = "Effect of sample size on the sampling distribution"
#   )

# Interpretation:
# - What happens to the shape as n increases?
# - What happens to the center as n increases?
# - What happens to the spread as n increases?
# - Compare your n = 5 histogram with your partner's. Whose looks more
#   Normal? Why might that be, given your two populations?


############################################################
# Question 6. What does number of repetitions K change?
############################################################

# Keep n = 50 fixed and compare K = 20, 200, and 1000.

# mu_hats_k20 <- get_mu_hats(pop, n = 50, K = 20)
# mu_hats_k200 <- get_mu_hats(pop, n = 50, K = 200)
# mu_hats_k1000 <- get_mu_hats(pop, n = 50, K = 1000)
#
# sampling_k <- data.frame(
#   mu_hat = c(mu_hats_k20, mu_hats_k200, mu_hats_k1000),
#   repetitions = factor(c(rep(20, 20), rep(200, 200), rep(1000, 1000)))
# )
#
# ggplot(sampling_k, aes(x = mu_hat)) +
#   geom_histogram(bins = 25, fill = "coral", color = "white") +
#   facet_wrap(~ repetitions, scales = "free_y") +
#   geom_vline(xintercept = population_mean, color = "red", linetype = "dashed") +
#   labs(
#     x = "Sample mean",
#     y = "Count",
#     title = "Effect of number of repetitions on the picture"
#   )

# Interpretation:
# - Which changes actual sampling variability: n or K?
# - Which mostly changes how clearly we can see the distribution?


############################################################
# Question 7. Compare standard error and simulated spread
############################################################

# n <- 5
# K <- 1000
#
# mu_hats <- get_mu_hats(pop, n = n, K = K)
#
# SE <- population_sd / sqrt(n)
# empirical_sd <- sd(mu_hats)
#
# SE
# empirical_sd
# abs(SE - empirical_sd)

# Interpretation:
# - Are the theoretical standard error and simulated standard deviation close?
# - What happens when the sample size n changes (try n = 5, 50)?


############################################################
# Question 8. Check the empirical rule
############################################################

# n <- 5
# K <- 1000
#
# mu_hats <- get_mu_hats(pop, n = n, K = K)
#
# m <- mean(mu_hats)
# s <- sd(mu_hats)
#
# rate1 <- mean(abs(mu_hats - m) < 1 * s)
# rate2 <- mean(abs(mu_hats - m) < 2 * s)
# rate3 <- mean(abs(mu_hats - m) < 3 * s)
#
# rate1
# rate2
# rate3
#
# abs(c(rate1 - 0.68, rate2 - 0.95, rate3 - 0.997))

# Interpretation:
# - Does your sampling distribution approximately follow the 68-95-99.7 rule?
# - What happens when the sample size changes (try n = 5, 50)?
# - Compare your rates at n = 5 with your partner's. Which population
#   needed a larger n to follow the 68-95-99.7 rule?

# Final reflection:
# - What is a sampling distribution?
# - What changed when n increased?
# - What changed when K increased?
# - Did your population shape make it easy or hard for the CLT to appear?
#   How did that compare with your partner's population?
