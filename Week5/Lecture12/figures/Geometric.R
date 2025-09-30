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
theoretical_cdf <- data.frame(outcomes, cumulative = cdf)

geom_cdf_plot <- ggplot(data = theoretical_cdf, aes(x = outcomes, y = cumulative)) +
  geom_step(direction = "hv", size = 1.2, color = "darkorange") +
  scale_x_continuous(breaks = 1:n_max) +
  labs(title = "Geometric Distribution CDF (p = 0.35)",
       x = "Trial of First Success",
       y = "Cumulative Probability")
ggsave("Geometric_CDF.png", geom_cdf_plot, width = 4, height = 2.5, units = "in")