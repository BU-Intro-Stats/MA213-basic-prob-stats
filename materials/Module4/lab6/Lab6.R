############################################################
# MA 213 - Lab 6 Starter Code
# Confidence Intervals and Hypothesis Tests
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the missing pieces first.
############################################################


############################################################
# Getting Started. One Proportion
############################################################

x <- 127
n <- 200
p_hat <- x / n

p_hat


############################################################
# Question 1. Construct and interpret a confidence interval
############################################################

alpha <- 0.05
# Fill in the blank with pnorm or qnorm.
# z_star <- ____(1 - alpha / 2)

SE <- sqrt(p_hat * (1 - p_hat) / n)
CI <- p_hat + c(-1, 1) * z_star * SE

z_star
SE
CI

# Interpretation:
# - Interpret this 95% confidence interval in context.
# - Avoid saying there is a 95% probability that this one interval
#   contains the true proportion.


############################################################
# Question 2. Test a hypothesis using the same sample
############################################################

# H0: p = 0.50
# Ha: p != 0.50

p0 <- 0.50

SE_null <- sqrt(p0 * (1 - p0) / n)
z_stat <- (p_hat - p0) / SE_null
# Fill in the blank with pnorm or qnorm.
# p_value <- 2 * (1 - ____(abs(z_stat)))

z_stat
p_value

# Interpretation:
# - At alpha = 0.05, do you reject H0?
# - Write the conclusion in context.


############################################################
# Question 3. Simulate confidence interval coverage
############################################################

set.seed(213)

N <- 675000
pop <- c(rep("Cellphone", N * 0.9), rep("No Cellphone", N * 0.1))
true_prop <- mean(pop == "Cellphone")

true_prop

# Run the simulation. It builds a table with one row per simulated sample.

alpha <- 0.05
n <- 200
K <- 1000

prop_est <- rep(0, K)
lower <- rep(0, K)
upper <- rep(0, K)
included <- rep(FALSE, K)

for (k in 1:K) {
  sampled_data <- sample(pop, size = n)
  p_hat <- mean(sampled_data == "Cellphone")
  SE <- sqrt(p_hat * (1 - p_hat) / n)
  CI <- p_hat + c(-1, 1) * qnorm(1 - alpha / 2) * SE

  prop_est[k] <- p_hat
  lower[k] <- CI[1]
  upper[k] <- CI[2]
  included[k] <- CI[1] <= true_prop & true_prop <= CI[2]
}

df_table <- data.frame(
  prop_est = prop_est,
  lower = lower,
  upper = upper,
  included = included
)

head(df_table)

# Fill in the blanks, then uncomment and run.

# Coverage rate: the proportion of intervals that contain true_prop.
# coverage_rate <- mean(df_table$____)
# coverage_rate

# Typical interval width: the average width of the confidence intervals.
# widths <- df_table$____ - df_table$____
# typical_width <- mean(widths)
# typical_width

# Then change alpha to 0.01 and rerun the simulation above.
# Compare the 95% and 99% results:
# - coverage rate
# - typical interval width


############################################################
# Question 4. Build a two-way table
############################################################

titanic <- read.csv("Titanic.csv", row.names = 1)

tbl <- table(titanic$Survived, titanic$Class)
tbl
addmargins(tbl)

# Interpretation:
# - What would independence mean in this setting?


############################################################
# Question 5. Calculate expected counts
############################################################

row_totals <- rowSums(tbl)
col_totals <- colSums(tbl)
total <- sum(tbl)

expected_tbl <- outer(row_totals, col_totals) / total
expected_tbl

# all(expected_tbl >= ____)

# Interpretation:
# - Do expected counts meet the chi-square condition?


############################################################
# Question 6. Compute the chi-square statistic and p-value
############################################################

chi_stat <- sum((tbl - expected_tbl)^2 / expected_tbl)
df <- (nrow(tbl) - 1) * (ncol(tbl) - 1)
p_value <- 1 - pchisq(chi_stat, df = df)

chi_stat
df
p_value

# If R prints a p-value of 0, the p-value is too small to display.
# Report it as p < 0.001.

# Interpretation:
# - At alpha = 0.05, do you reject H0?
# - Write the conclusion in context.


############################################################
# Question 7. Write a reusable chi-square function
############################################################

# Fill in the function body, then uncomment and run.

# manual_chisq <- function(tbl) {
#   # Expected counts.
#
#   # Chi-square statistic.
#
#   # Degrees of freedom.
#
#   # P-value.
#
#   list(expected = expected_tbl, chi_stat = chi_stat, df = df, p_value = p_value)
# }

# Check: the results should match your answers to Questions 5-6.
# manual_chisq(tbl)

# Use your function on sex and survival.
# tbl2 <- table(titanic$Survived, titanic$Sex)
# tbl2
# manual_chisq(tbl2)

# Interpretation:
# - Does survival appear independent of sex in the Titanic data?
