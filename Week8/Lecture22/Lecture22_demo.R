# ---- 0. Setup and load libraries, if any ----

library(ggplot2)


# ---- 1. Simulate Labby's experiment many times ----

set.seed(42)  # for reproducibility

n_rolls <- 26306    # number of times 12 dice are rolled
n_dice <- 12        # number of dice per roll
n_sim <- 100        # number of simulations

expected <- rep(n_rolls * n_dice / 6, 6)

chisq_stats <- numeric(n_sim)
for (i in 1:n_sim) {
  # Simulate all dice rolls (matrix: n_rolls x n_dice)
  rolls <- matrix(sample(1:6, n_rolls * n_dice, replace = TRUE), ncol = n_dice)
  # Tabulate all outcomes
  observed <- table(factor(rolls, levels = 1:6))
  # Compute chi-square statistic
  chisq_stats[i] <- sum((observed - expected)^2 / expected)
}

# Plot histogram of simulated chi-square statistics
# Prepare data for ggplot
chisq_df <- data.frame(chisq = chisq_stats)

# Create a data frame for the theoretical chi-square density
x_theory <- seq(min(chisq_stats), max(chisq_stats), length.out = 1000)
theory_df <- data.frame(
  chisq = x_theory,
  density = dchisq(x_theory, df = 5)
)

# Plot with ggplot2
ggplot(chisq_df, aes(x = chisq)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "white") +
  geom_line(data = theory_df, aes(x = chisq, y = density), color = "black", size = 1.2) +
  labs(
    title = "Simulated Chi-square Statistics\nLibby's Experiment (Fair Dice)",
    x = expression(chi^2),
    y = "Density"
  )


# ---- 2. Plotting the chi-squared distribution ----

# Set an x-axis over which to evaluate the chi-squared distribution
npoints <- 1000
x <- seq(1, 50, length.out=npoints)

# Which function do you think we should use to compute Pr(X=x) for each x above:
# dchisq, pchisq, qchisq, or rchisq?

df05 <- dchisq(x, df=0.5)  
df1 <- dchisq(x, df=1)
df5 <- dchisq(x, df=5)
df10 <- dchisq(x, df=10)
df50 <- dchisq(x, df=30)

data <- data.frame(rep(x, 5), 
                   c(df05, df1, df5, df10, df50), 
                   c(rep(0.5, npoints), rep(1, npoints), rep(5, npoints), rep(10, npoints), 
                     rep(30, npoints)))
colnames(data) <- c("x", "density", "df")

ggplot(data=data, aes(x=x, y=density, color=factor(df))) +
  geom_line() +
  labs(color="df") +
  ggtitle("Chi-squared distribution with different degrees of freedom (df)")

# ---- 3. Computing probabilities ----

pchisq(q = 30, df = 6, lower.tail = FALSE)
