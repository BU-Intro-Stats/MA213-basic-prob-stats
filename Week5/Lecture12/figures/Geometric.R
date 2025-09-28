library(ggplot2)

# Parameters
p <- 0.35
n_max <- 10
outcomes <- 1:n_max

# Geometric PMF
pmf <- dgeom(outcomes - 1, prob = p) # R's dgeom uses number of failures before first success
theoretical_pmf <- data.frame(outcomes, probabilities = pmf)

ggplot(data = theoretical_pmf, aes(x = outcomes, y = probabilities)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_continuous(breaks = 1:n_max) +
  labs(title = "Geometric Distribution PMF (p = 0.35)",
       x = "Trial of First Success",
       y = "Probability")

# Geometric CDF
cdf <- pgeom(outcomes - 1, prob = p)
theoretical_cdf <- data.frame(outcomes, cumulative = cdf)

ggplot(data = theoretical_cdf, aes(x = outcomes, y = cumulative)) +
  geom_step(direction = "hv", size = 1.2, color = "darkorange") +
  scale_x_continuous(breaks = 1:n_max) +
  labs(title = "Geometric Distribution CDF (p = 0.35)",
       x = "Trial of First Success",
       y = "Cumulative Probability")