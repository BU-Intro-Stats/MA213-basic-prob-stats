# ---- 0. Setup and load libraries, if any ----

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# ---- Binomial vs Poisson Approximation Demo ----

# Function to generate a single Binomial plot
plot_binom <- function(n, p, title, xvals) {
  binom_probs <- dbinom(xvals, size = n, prob = p)
  df <- data.frame(x = xvals, Probability = binom_probs)
  mean_text <- paste0("n*p = ", n * p)
  ggplot(df, aes(x = x, y = Probability)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "steelblue", alpha = 0.7) +
    labs(title = title, x = "x", y = "P(X = x)") +
    annotate("text", x = 12, y = 0.3, label = mean_text, size = 5, color = "steelblue") +
    ylim(0, 0.65) +
    theme_minimal()
}

# Function to generate a single Poisson plot
plot_pois <- function(lambda, title, xvals) {
  pois_probs <- dpois(xvals, lambda = lambda)
  df <- data.frame(x = xvals, Probability = pois_probs)
  mean_text <- bquote(lambda == .(lambda))
  ggplot(df, aes(x = x, y = Probability)) +
    geom_bar(stat = "identity", fill = "black", color = "black", alpha = 0.7) +
    labs(title = title, x = "x", y = "P(X = x)") +
    annotate("text", x = 12, y = 0.3, label = mean_text, size = 5, color = "black") +
    ylim(0, 0.65) +
    theme_minimal()
}

# First row: np = lambda = 0.5
xvals <- 0:20
plots1 <- list(
  plot_binom(5, 0.1, "Binomial(5, 0.1)", xvals),
  plot_binom(10, 0.05, "Binomial(10, 0.05)", xvals),
  plot_binom(100, 0.005, "Binomial(100, 0.005)", xvals),
  plot_pois(0.5, "Poisson(0.5)", xvals)
)

# Second row: np = lambda = 3
plots2 <- list(
  plot_binom(5, 0.6, "Binomial(5, 0.6)", xvals),
  plot_binom(10, 0.3, "Binomial(10, 0.3)", xvals),
  plot_binom(100, 0.03, "Binomial(100, 0.03)", xvals),
  plot_pois(3, "Poisson(3)", xvals)
)

# Show plots in a 2x4 grid
grid.arrange(grobs = c(plots1,plots2), nrow = 2)
