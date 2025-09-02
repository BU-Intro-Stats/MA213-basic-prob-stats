# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Set up the baseline parameters ----
alpha <- 0.05  # significance level
se <- 3.3
null_mean <- 0  # mean difference under the null hypothesis
alt_mean <- 1 # mean of the alternative hypothesis

x <- seq(-4 * se + min(null_mean, alt_mean), 
         4 * se + max(null_mean, alt_mean), length.out = 1000)

# ---- 2. Plot the null hypothesis the alt hypothesis, with the power shaded ----

plot_power <- function(alpha, se, null_mean, alt_mean) {
  # compute parameters for power calculation
  effect_size <- abs(alt_mean-null_mean)  # difference in means we want to detect
  critical_value <- qnorm(1 - alpha / 2) * se  # Critical value for two-tailed test

  # Calculate the distributions
  null_dist <- dnorm(x, mean = null_mean, sd = se)
  alt_dist <- dnorm(x, mean = alt_mean, sd = se)

  # Combine into a single data frame
  plot_df <- rbind(
    data.frame(x = x, y = null_dist, dist = "Null"),
    data.frame(x = x, y = alt_dist, dist = "Alternative")
  )

  # Plot both distributions and annotate with power
  plot <- ggplot(plot_df, aes(x = x, y = y, color = dist)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Null" = "steelblue", "Alternative" = "darkgreen")) +
    geom_vline(xintercept = c(-critical_value, critical_value), linetype = "dashed", color = "steelblue") +
    geom_area(
      data = subset(plot_df, dist=="Alternative" & x >= critical_value), 
      fill = 'darkgreen', alpha = 0.5) +
    geom_area(
      data = subset(plot_df, dist=="Alternative" & x <= -critical_value), 
      fill = 'darkgreen', alpha = 0.5) +
    theme_minimal(base_size = 15)

  return(plot)
}

p_sexratio <- plot_power(alpha, se, null_mean, alt_mean)
p_sexratio <- p_sexratio +
  labs(x = "Difference in Sex Ratios (%)",
       y = "Density",
       color = "Distribution")
print(p_sexratio)
ggsave("whatif_sexratio.png", p_sexratio, width = 8, height = 4, dpi = 300)

# ---- 3. Compute the number of subjects needed to achieve a power of 0.8 ----
alpha <- 0.05  # significance level
se <- 3.3 # Calculated standard *error*
null_mean <- 0  # mean difference under the null hypothesis
alt_mean <- 1 # mean of the alternative hypothesis
n <- 2972 # number of subjects

# Compute the standard *deviation* from the standard error, since se = sd/sqrt(n)
sd = se * sqrt(n)

power.t.test(delta = abs(alt_mean - null_mean), sd = sd, sig.level = alpha, power = 0.8, type = "one.sample", alternative = "two.sided")


