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

# Binomial PMF with Normal on top
n_binom2 <- 30
binom_outcomes2 <- 0:n_binom2
binom_pmf2 <- dbinom(binom_outcomes2, size = n_binom2, prob = p)
binom_pmf_df2 <- data.frame(
  successes = binom_outcomes2,
  probability = binom_pmf2
)

mu <- n_binom2 * p
sigma <- sqrt(n_binom2 * p * (1 - p))
x_vals <- seq(-0.5, n_binom2 + 0.5, length.out = 1000)
normal_curve <- dnorm(x_vals, mean = mu, sd = sigma)

binom_pmf_normal_plot <- ggplot(binom_pmf_df2, aes(x = successes, y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  stat_function(fun = function(x) dnorm(x, mean = mu, sd = sigma),
                color = "black", size = 1.2, n = 1000) +
  scale_x_continuous(
    breaks = binom_outcomes2[binom_outcomes2 %% 2 == 0]
  ) +
  labs(title = paste0("Binomial PMF with Normal Approximation\n(n = ", n_binom2, ", p = ", p, ")"),
       x = "Number of Successes",
       y = "Probability / Density")

ggsave("Binomial_PMF_Normal.png", binom_pmf_normal_plot, width = 4, height = 2.5, units = "in")

# Poisson PMF
lambda <- 3.5
n_pois <- 12
pois_outcomes <- 0:n_pois

pois_pmf <- dpois(pois_outcomes, lambda = lambda)
pois_pmf_df <- data.frame(
  outcomes = pois_outcomes,
  probability = pois_pmf
)

pois_pmf_plot <- ggplot(pois_pmf_df, aes(x = outcomes, y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_continuous(breaks = pois_outcomes) +
  labs(title = paste0("Poisson PMF (λ = ", lambda, ")"),
       x = "Number of Events",
       y = "Probability")
ggsave("Poisson_PMF.png", pois_pmf_plot, width = 4, height = 2.5, units = "in")

# Poisson CDF
pois_cdf <- ppois(pois_outcomes, lambda = lambda)
pois_cdf_df <- data.frame(
  outcomes = c(-0.5, pois_outcomes, n_pois + 0.5),
  cumulative = c(0, pois_cdf, 1)
)

pois_cdf_plot <- ggplot(pois_cdf_df, aes(x = outcomes, y = cumulative)) +
  geom_step(direction = "hv", size = 1.2, color = "darkorange") +
  scale_x_continuous(breaks = pois_outcomes) +
  labs(title = paste0("Poisson CDF (λ = ", lambda, ")"),
       x = "Number of Events",
       y = "Cumulative Probability")
ggsave("Poisson_CDF.png", pois_cdf_plot, width = 4, height = 2.5, units = "in")