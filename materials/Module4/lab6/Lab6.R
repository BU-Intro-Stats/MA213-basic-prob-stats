############################################################
# MA 213 - Lab 6 Starter Code
# Confidence Intervals and Hypothesis Tests
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the missing pieces first.
############################################################

library(dplyr)
library(ggplot2)


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
z_star <- qnorm(1 - alpha / 2)

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
p_value <- 2 * (1 - pnorm(abs(z_stat)))

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

# Fill in the blank in sample(), then run.

# alpha <- 0.05
# n <- 200
# K <- 1000
#
# num_of_simulation <- rep(0, K)
# prop_est <- rep(0, K)
# lower <- rep(0, K)
# upper <- rep(0, K)
# included <- rep(FALSE, K)
#
# for (k in 1:K) {
#   sampled_data <- sample(pop, size = ____)
#   p_hat <- mean(sampled_data == "Cellphone")
#   SE <- sqrt(p_hat * (1 - p_hat) / n)
#   CI <- p_hat + c(-1, 1) * qnorm(1 - alpha / 2) * SE
#
#   num_of_simulation[k] <- k
#   prop_est[k] <- p_hat
#   lower[k] <- CI[1]
#   upper[k] <- CI[2]
#   included[k] <- CI[1] <= true_prop & true_prop <= CI[2]
# }
#
# df_table <- data.frame(
#   num_of_simulation = num_of_simulation,
#   prop_est = prop_est,
#   lower = lower,
#   upper = upper,
#   included = included
# )
#
# head(df_table)
# mean(df_table$included)

# Interpretation:
# - About what proportion of 95% CIs contained the true proportion?


############################################################
# Question 4. What changes when confidence level changes?
############################################################

# Change alpha to 0.01 and rerun the simulation above.
# Compare:
# - coverage rate
# - typical interval width


############################################################
# Question 5. Build a two-way table
############################################################

titanic_df <- as.data.frame(Titanic)

class_survival <- titanic_df %>%
  group_by(Class, Survived) %>%
  summarize(count = sum(Freq), .groups = "drop")

class_survival

tbl <- xtabs(count ~ Survived + Class, data = class_survival)
tbl
addmargins(tbl)

# Interpretation:
# - What would independence mean in this setting?


############################################################
# Question 6. Calculate expected counts
############################################################

row_totals <- rowSums(tbl)
col_totals <- colSums(tbl)
total <- sum(tbl)

expected_tbl <- outer(row_totals, col_totals) / total
expected_tbl

all(expected_tbl >= 5)

# Interpretation:
# - Do expected counts meet the chi-square condition?


############################################################
# Question 7. Compute the chi-square statistic and p-value
############################################################

chi_stat <- sum((tbl - expected_tbl)^2 / expected_tbl)
df <- (nrow(tbl) - 1) * (ncol(tbl) - 1)
p_value <- 1 - pchisq(chi_stat, df = df)

chi_stat
df
p_value

# Interpretation:
# - At alpha = 0.05, do you reject H0?
# - Write the conclusion in context.


############################################################
# Question 8. Try another pair of variables
############################################################

sex_survival <- titanic_df %>%
  group_by(Sex, Survived) %>%
  summarize(count = sum(Freq), .groups = "drop")

tbl2 <- xtabs(count ~ Survived + Sex, data = sex_survival)
tbl2

# Fill in the rest of the chi-square workflow.

# expected_tbl2 <- ____
# chi_stat2 <- ____
# df2 <- ____
# p_value2 <- ____

# Interpretation:
# - Does survival appear independent of sex in the Titanic data?


############################################################
# Optional Challenge. Reusable chi-square function
############################################################

# manual_chisq <- function(tbl) {
#   # Your code here.
# }
