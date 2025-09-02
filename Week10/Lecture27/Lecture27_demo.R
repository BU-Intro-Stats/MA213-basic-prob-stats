# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Set up the parameters ----
alpha = 0.05  # significance level
n1 = 30       # sample size for group 1
n2 = 30       # sample size for group 2
sd1 = 10      # standard deviation for group 1
sd2 = 10      # standard deviation for group 2
null_mean = 0  # mean difference under the null hypothesis
alt_mean = 5 # mean of the alternative hypothesis

# compute parameters for power calculation
effect_size = abs(alt_mean-null_mean)  # difference in means we want to detect
se = sqrt((sd1^2 / n1) + (sd2^2 / n2)) # standard error of the difference in means
critical_value = qnorm(1 - alpha / 2) * se  # Critical value for two-tailed test

# ---- 2. Calculate power for the two-sample z-test ----

# Compute z-scores for the critical values under the alternative hypothesis
z_upper = (critical_value - effect_size) / se
z_lower = (-critical_value - effect_size) / se

# Power is the probability of rejecting H0 when H1 is true:
# P(X > critical_value | H1) + P(X < -critical_value | H1)
power = pnorm(z_lower) + (1 - pnorm(z_upper))

cat(sprintf("Power of the test: %.3f\n", power))

# ---- 3. Plot the null hypothesis the alt hypothesis, with the power shaded ----

# Calculate the distributions
x = seq(-4 * se + min(null_mean, alt_mean), 
        4 * se + max(null_mean, alt_mean), length.out = 1000)
null_dist = dnorm(x, mean = null_mean, sd = se)
alt_dist = dnorm(x, mean = alt_mean, sd = se)

# Combine into a single data frame
plot_df = rbind(
  data.frame(x = x, y = null_dist, dist = "Null"),
  data.frame(x = x, y = alt_dist, dist = "Alternative")
)


# Plot both distributions and annotate with power
ggplot(plot_df, aes(x = x, y = y, color = dist)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Null" = "blue", "Alternative" = "darkgreen")) +
  geom_vline(xintercept = c(-critical_value, critical_value), linetype = "dashed", color = "blue") +
  geom_area(
    data = subset(plot_df, dist=="Alternative" & x >= critical_value), 
    aes(y = y), 
    fill = 'lightgreen', alpha = 0.5) +
  geom_area(
    data = subset(plot_df, dist=="Alternative" & x <= -critical_value), 
    aes(y = y), 
    fill = 'lightgreen', alpha = 0.5) +
  labs(title = sprintf("Power = %.3f", power),
       x = "Difference in Sample Means",
       y = "Density",
       color = "Distribution")


