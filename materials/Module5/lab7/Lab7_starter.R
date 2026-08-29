############################################################
# MA 213 - Lab 7 Starter Code
# Bayesian Updating and Statistical Evidence
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the interpretation first.
############################################################


############################################################
# Question 1. A diagnostic test
############################################################

prior_condition <- 0.02
sensitivity <- 0.95
specificity <- 0.90

p_positive_given_condition <- sensitivity
p_positive_given_no_condition <- 1 - specificity

p_positive <- p_positive_given_condition * prior_condition +
  p_positive_given_no_condition * (1 - prior_condition)

posterior_condition <- p_positive_given_condition * prior_condition /
  p_positive

posterior_condition

# Interpretation:
# - What is the probability that a person has the condition after a
#   positive test?
# - Why is this not simply equal to the sensitivity?


############################################################
# Question 2. Build the same result with counts
############################################################

N <- 10000

has_condition <- N * prior_condition
no_condition <- N * (1 - prior_condition)

true_positive <- has_condition * sensitivity
false_positive <- no_condition * (1 - specificity)

positive_total <- true_positive + false_positive
true_positive / positive_total

# Interpretation:
# - Use the counts to explain the posterior probability.


############################################################
# Question 3. Update a prior about a coin
############################################################

p_grid <- seq(0, 1, by = 0.01)

# Flat prior: every value is equally plausible.
prior <- rep(1, length(p_grid))
prior <- prior / sum(prior)

heads <- 8
flips <- 10

likelihood <- dbinom(heads, size = flips, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

bayes_table <- data.frame(
  p = p_grid,
  prior = prior,
  likelihood = likelihood,
  posterior = posterior
)

head(bayes_table)
bayes_table$p[which.max(bayes_table$posterior)]

# Interpretation:
# - What value of p is most plausible after seeing 8 heads in 10 flips?


############################################################
# Question 4. Summarize posterior belief
############################################################

posterior_mean <- sum(bayes_table$p * bayes_table$posterior)
posterior_mean

posterior_cdf <- cumsum(bayes_table$posterior)

lower_90 <- bayes_table$p[min(which(posterior_cdf >= 0.05))]
upper_90 <- bayes_table$p[min(which(posterior_cdf >= 0.95))]

c(lower_90, upper_90)

# Interpretation:
# - Interpret the posterior mean.
# - Interpret the 90% credible interval.


############################################################
# Question 5. Change the prior
############################################################

skeptical_prior <- dbeta(p_grid, shape1 = 20, shape2 = 20)
skeptical_prior <- skeptical_prior / sum(skeptical_prior)

skeptical_posterior <- likelihood * skeptical_prior
skeptical_posterior <- skeptical_posterior / sum(skeptical_posterior)

skeptical_table <- data.frame(
  p = p_grid,
  prior = skeptical_prior,
  posterior = skeptical_posterior
)

skeptical_mean <- sum(skeptical_table$p * skeptical_table$posterior)
skeptical_mode <- skeptical_table$p[which.max(skeptical_table$posterior)]

c(skeptical_mean = skeptical_mean, skeptical_mode = skeptical_mode)

# Interpretation:
# - How did the skeptical prior change the posterior?


############################################################
# Question 6. More data, less prior influence
############################################################

heads_big <- 80
flips_big <- 100

likelihood_big <- dbinom(heads_big, size = flips_big, prob = p_grid)

posterior_flat_big <- likelihood_big * prior
posterior_flat_big <- posterior_flat_big / sum(posterior_flat_big)

posterior_skeptical_big <- likelihood_big * skeptical_prior
posterior_skeptical_big <- posterior_skeptical_big / sum(posterior_skeptical_big)

mean_flat_big <- sum(p_grid * posterior_flat_big)
mean_skeptical_big <- sum(p_grid * posterior_skeptical_big)

c(flat_prior = mean_flat_big, skeptical_prior = mean_skeptical_big)

# Interpretation:
# - What happens to the influence of the prior when the amount of data grows?


############################################################
# Optional Challenge. Create your own prior
############################################################

# my_prior <- rep(1, length(p_grid))
# my_prior <- my_prior / sum(my_prior)
#
# my_likelihood <- dbinom(14, size = 20, prob = p_grid)
# my_posterior <- my_likelihood * my_prior
# my_posterior <- my_posterior / sum(my_posterior)
#
# sum(p_grid * my_posterior)
