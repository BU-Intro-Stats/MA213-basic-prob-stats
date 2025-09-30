library(ggplot2)

# Parameters
p <- 0.35
n_max <- 10
outcomes <- 1:n_max

# Bernoulli PMF
bernoulli_pmf <- data.frame(
  outcome = c(0, 1),
  probabilities = c(1 - p, p)
) 
bern_pmf_plot <- ggplot(data = bernoulli_pmf, aes(x = factor(outcome), y = probabilities)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_discrete(breaks = c(0, 1)) +
  labs(title = "Bernoulli Distribution PMF (p = 0.35)",
       x = "Outcome",
       y = "Probability")
ggsave("Bernoulli_PMF.png", bern_pmf_plot, width = 4, height = 2.5, units = "in")

# Bernoulli CDF
bernoulli_cdf <- data.frame(
  outcome = c(-0.5, 0, 1, 1.5),
  cumulative = c(0, 1 - p, 1, 1)
)
bern_cdf_plot <- ggplot(data = bernoulli_cdf, aes(x = outcome, y = cumulative)) +
  geom_step(direction = "hv", size = 1.2, color = "orange") +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(title = "Bernoulli Distribution CDF (p = 0.35)",
       x = "Outcome",
       y = "Cumulative Probability")  
ggsave("Bernoulli_CDF.png", bern_cdf_plot, width = 4, height = 2.5, units = "in")

# Geometric PMF
pmf <- dgeom(outcomes - 1, prob = p) # R's dgeom uses number of failures before first success
theoretical_pmf <- data.frame(outcomes, probabilities = pmf)

geom_pmf_plot <- ggplot(data = theoretical_pmf, aes(x = outcomes, y = probabilities)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_continuous(breaks = 1:n_max) +
  labs(title = "Geometric Distribution PMF (p = 0.35)",
       x = "Trial of First Success",
       y = "Probability")
ggsave("Geometric_PMF.png", geom_pmf_plot, width = 4, height = 2.5, units = "in")

# Geometric CDF
cdf <- pgeom(outcomes - 1, prob = p)
theoretical_cdf <- data.frame(
  outcomes = c(0, outcomes),
  cumulative = c(0, cdf)
)

geom_cdf_plot <- ggplot(data = theoretical_cdf, aes(x = outcomes, y = cumulative)) +
  geom_step(direction = "hv", size = 1.2, color = "darkorange") +
  scale_x_continuous(breaks = c(0, 1:n_max)) +
  labs(title = "Geometric Distribution CDF (p = 0.35)",
       x = "Trial of First Success",
       y = "Cumulative Probability")
ggsave("Geometric_CDF.png", geom_cdf_plot, width = 4, height = 2.5, units = "in")

# Binomial PMF
n_binom <- 10
binom_outcomes <- 0:n_binom
binom_pmf <- dbinom(binom_outcomes, size = n_binom, prob = p)
binom_pmf_df <- data.frame(
  successes = binom_outcomes,
  probability = binom_pmf
)

binom_pmf_plot <- ggplot(binom_pmf_df, aes(x = successes, y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_continuous(breaks = binom_outcomes) +
  labs(title = paste0("Binomial PMF (n = ", n_binom, ", p = ", p, ")"),
       x = "Number of Successes",
       y = "Probability")
ggsave("Binomial_PMF.png", binom_pmf_plot, width = 4, height = 2.5, units = "in")

# Binomial CDF
binom_cdf <- pbinom(binom_outcomes, size = n_binom, prob = p)
binom_cdf_df <- data.frame(
  successes = c(-0.5, binom_outcomes, n_binom + 0.5),
  cumulative = c(0, binom_cdf, 1)
)
binom_cdf_plot <- ggplot(binom_cdf_df, aes(x = successes, y = cumulative)) +
  geom_step(direction = "hv", size = 1.2, color = "darkorange") +
  scale_x_continuous(breaks = binom_outcomes) +
  labs(title = paste0("Binomial CDF (n = ", n_binom, ", p = ", p, ")"),
       x = "Number of Successes",
       y = "Cumulative Probability")
ggsave("Binomial_CDF.png", binom_cdf_plot, width = 4, height = 2.5, units = "in") 