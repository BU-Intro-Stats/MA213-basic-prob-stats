# ---- 0. Setup and load libraries ----

library(ggplot2)  # load the graphing library
library(gridExtra)  # for arranging ggplot plots

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Reminder: rnorm, dnorm, pnorm ----

# rnorm: generate random samples from a normal distribution
samples <- rnorm(1000, mean = 0, sd = 1)

# dnorm: probability density function (height of the curve at x)
x <- seq(-4, 4, length.out = 200)
densities <- dnorm(x, mean = 0, sd = 1)

df_density <- data.frame(x = x, density = densities)

p1 <- ggplot(df_density, aes(x = x, y = density)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(title = "dnorm: Normal Density", y = "Density", x = "x") +
  theme_minimal()

# pnorm: cumulative probability up to x (P(X < x))
cumulative <- pnorm(x, mean = 0, sd = 1)

df_cumulative <- data.frame(x = x, cumulative = cumulative)

p2 <- ggplot(df_cumulative, aes(x = x, y = cumulative)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_hline(yintercept = c(0, 1), color = "gray", linetype = "dashed") +
  labs(title = "pnorm: Cumulative Probability", y = "P(X < x)", x = "x") +
  theme_minimal()

grid.arrange(p1, p2, nrow = 2)

# pnorm starts at 0 (far left), ends at 1 (far right)
# It gives the probability that a normal random variable is less than x

# ---- 3. What if we want to solve for x in P(X < x) = q? ----

# This is the inverse problem: given a probability q, what x satisfies P(X < x) = q?
# qnorm(q, mean, sd) gives the value x such that P(X < x) = q

# Example: Body temperatures of healthy humans are distributed nearly normally 
# with mean 98.2 degrees F and standard deviation 0.73 degrees F. What is the 
# cutoff for the highest 10% of human body temperatures?

# we can compute it using the nonstandard Normal(98.2, 0.73)
x = qnorm(0.9, mean = 98.2, sd = 0.73)  # About 99.1
print(x)

# or we can compute it using the standard Normal(0,1) 
# but then we have to un-z-score it by multiplying by the std and adding the mean
z = qnorm(0.9)
print(z*.73+98.2)

# ---- 4. Plot qnorm and visually relate to pnorm ----

# Plot qnorm for a sequence of probabilities
probs <- seq(0, 1, length.out = 200)
qnorm_x <- qnorm(probs, mean = 0, sd = 1)

df_qnorm <- data.frame(prob = probs, qnorm_x = qnorm_x)
df_lines <- data.frame(
  prob = c(0.25, 0.5, 0.75),
  x = qnorm(c(0.25, 0.5, 0.75))
)

p3 <- ggplot(df_cumulative, aes(x = x, y = cumulative)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_hline(yintercept = df_lines$prob, color = "gray", linetype = "dotted") +
  geom_vline(xintercept = df_lines$x, color = "red", linetype = "dashed") +
  labs(title = "pnorm: P(X < x) Cumulative Distribution Function", y = "Probability", x = "x") +
  theme_minimal()

p4 <- ggplot(df_qnorm, aes(x = prob, y = qnorm_x)) +
  geom_line(color = "purple", size = 1.2) +
  geom_vline(xintercept = df_lines$prob, color = "gray", linetype = "dotted") +
  geom_hline(yintercept = df_lines$x, color = "red", linetype = "dashed") +
  labs(title = "qnorm: Inverse CDF", y = "x", x = "Probability (q)") +
  theme_minimal()

grid.arrange(p3, p4, nrow = 2)


# The qnorm curve is the inverse of the pnorm curve.
# For a given probability q, qnorm(q) gives the x value such that P(X < x) = q.