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
pop <- rnorm(N, mean = 2, sd = sigma)
# pop <- rbinom(N, size = 1, prob = 0.3)
# pop <- rpois(N, lambda = sigma^2)
# pop <- rexp(N, rate = 1 / sigma)
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


############################################################
# Question 2. Draw one sample
############################################################

n <- 100

my_sample <- sample(pop, size = n)
mu_hat <- mean(my_sample)

mu_hat
population_mean
mu_hat - population_mean

# Interpretation:
# - Does mu_hat equal the population mean exactly?
# - How close was your sample mean to the population mean?


############################################################
# Question 3. Build a sampling distribution
############################################################

K <- 100
n <- 100

mu_hat_vector <- rep(0, K)

for (i in 1:K) {
  my_sample <- sample(pop, size = n)
  mu_hat_vector[i] <- mean(my_sample)
}

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
# - Where is the sampling distribution centered?
# - Describe the shape, center, and spread.


############################################################
# Question 4. Write a reusable simulation function
############################################################

# Fill in the blanks, then uncomment.

# get_mu_hats <- function(pop, n, K) {
#   mu_hat_vector <- rep(0, K)
#
#   for (i in 1:K) {
#     my_sample <- sample(pop, size = ____)
#     mu_hat_vector[i] <- ____
#   }
#
#   return(____)
# }
#
# result_q4 <- get_mu_hats(pop, n = 100, K = 100)
# head(result_q4)

# Interpretation:
# - How could you check whether result_q4 behaves like the vector from
#   Question 3?


############################################################
# Question 5. What does sample size n change?
############################################################

# Keep K = 1000 fixed and compare n = 20, 200, and 1000.

# mu_hats_20 <- get_mu_hats(pop, n = 20, K = 1000)
# mu_hats_200 <- get_mu_hats(pop, n = 200, K = 1000)
# mu_hats_1000 <- get_mu_hats(pop, n = 1000, K = 1000)
#
# sampling_n <- data.frame(
#   mu_hat = c(mu_hats_20, mu_hats_200, mu_hats_1000),
#   sample_size = factor(rep(c(20, 200, 1000), each = 1000))
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


############################################################
# Question 6. What does number of repetitions K change?
############################################################

# Keep n = 1000 fixed and compare K = 20, 200, and 1000.

# mu_hats_k20 <- get_mu_hats(pop, n = 1000, K = 20)
# mu_hats_k200 <- get_mu_hats(pop, n = 1000, K = 200)
# mu_hats_k1000 <- get_mu_hats(pop, n = 1000, K = 1000)
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

# n <- 1000
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


############################################################
# Optional Challenge. Check the empirical rule
############################################################

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

# Final reflection:
# - What is a sampling distribution?
# - What changed when n increased?
# - What changed when K increased?
# - Did your population shape make it easy or hard for the CLT to appear?
