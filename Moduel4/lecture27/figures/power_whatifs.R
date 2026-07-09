# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Set up the baseline parameters ----
alpha <- 0.05  # significance level
se <- 1
null_mean <- 0  # mean difference under the null hypothesis
alt_mean <- 2.5 # mean of the alternative hypothesis

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
    labs(x = "Difference in Sample Means",
         y = "Density",
         color = "Distribution") + 
    theme_minimal(base_size = 15)

  return(plot)
}

# Baseline plot
p_baseline <- plot_power(alpha, se, null_mean, alt_mean)
print(p_baseline)
ggsave("whatif_baseline.png", p_baseline, width = 8, height = 4, dpi = 300)

# Increase the effect size
p_effect <- plot_power(alpha, se, null_mean, alt_mean*1.5)
p_effect <- p_effect +
  annotate("segment", x = alt_mean, xend = alt_mean*1.5, y = -0.01, yend = -0.01, arrow = arrow(ends = "last", length = unit(0.4, "cm")), color = "darkgreen", linewidth = 2)
print(p_effect)
ggsave("whatif_effect.png", p_effect, width = 8, height = 4, dpi = 300)

# Decrease the alpha level
p_alpha <- plot_power(0.01, se, null_mean, alt_mean)
p_alpha <- p_alpha +
  annotate("segment", x=qnorm(1-alpha/2)*se, xend=qnorm(1-0.01/2)*se, y = -0.01, yend = -0.01, arrow = arrow(ends = "last", length = unit(0.4, "cm")), color = "steelblue", linewidth = 2) +
  annotate("segment", x=-qnorm(1-alpha/2)*se, xend=-qnorm(1-0.01/2)*se, y = -0.01, yend = -0.01, arrow = arrow(ends = "last", length = unit(0.4, "cm")), color = "steelblue", linewidth = 2)
print(p_alpha)
ggsave("whatif_alpha.png", p_alpha, width = 8, height = 4, dpi = 300)

# Increase the sample size
p_samplesize <- plot_power(alpha, se/2, null_mean, alt_mean)
print(p_samplesize)
ggsave("whatif_samplesize.png", p_samplesize, width = 8, height = 4, dpi = 300)


if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined_1 <- p_baseline / p_effect
  print(combined_1)
  ggsave("whatif_baseline_effect.png", combined_1, width = 8, height = 7, dpi = 300)

  combined_2 <- p_baseline / p_alpha
  print(combined_2)
  ggsave("whatif_baseline_alpha.png", combined_2, width = 8, height = 7, dpi = 300)
  
  combined_3 <- p_baseline / p_samplesize
  print(combined_3)
  ggsave("whatif_baseline_samplesize.png", combined_3, width = 8, height = 7, dpi = 300)
}

# For lecture 28: very small effect size
p_lowpower <- plot_power(alpha, se, 0, 0.3)
print(p_lowpower)
ggsave("whatif_lowpower.png", p_lowpower, width = 8, height = 4, dpi = 300)

