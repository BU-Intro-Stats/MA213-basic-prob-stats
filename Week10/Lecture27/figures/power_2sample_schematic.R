# Power of 2-sample tests: Schematic figure for slides

library(ggplot2)  # load the graphing library


# Parameters for the schematic
null_mean <- 0
alt_mean <- 2.5
se <- 1
alpha <- 0.05
critical_value <- qnorm(1 - alpha) * se  # one-sided for schematic

x <- seq(null_mean - 4*se, alt_mean + 4*se, length.out = 1000)
null_dist <- dnorm(x, mean = null_mean, sd = se)
alt_dist <- dnorm(x, mean = alt_mean, sd = se)

plot_df <- rbind(
  data.frame(x = x, y = null_dist, dist = "Null"),
  data.frame(x = x, y = alt_dist, dist = "Alternative")
)

# For shading: rejection region under H0, power and beta under Ha
rejection_region <- x >= critical_value
power_region <- x >= critical_value
beta_region <- x < critical_value

# Null distribution plot (top)
  # Means
p_null<-ggplot(plot_df, aes(x = x, y = y, color = dist)) +
  geom_line(data = subset(plot_df, dist == "Null"), color = "steelblue", linewidth = 1) +
  geom_area(data = subset(plot_df, dist == "Null" & x >= critical_value), fill = "orange", color = "steelblue", alpha = 0.4) +
  geom_vline(xintercept = critical_value, linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = null_mean, linetype = "dotted", color = "steelblue") +
  annotate("text", x = null_mean - 3.4*se, y = max(null_dist)*0.1, label = expression(f[0]), color = "steelblue", size = 5) +
  annotate("text", x = critical_value + 0.5, y = max(null_dist)*0.25, label = expression(alpha), color = "orange", size = 5) +
  annotate("text", x = critical_value - 1, y = max(null_dist)*1.3, label = expression("Fail to reject "*H[0]), color = "darkred", size = 5, hjust = 1) +
  annotate("segment", x = critical_value-0.1, xend = critical_value-1, y = max(null_dist)*1.2, yend = max(null_dist)*1.2, arrow = arrow(ends = "last", length = unit(0.2, "cm")), color = "darkred", linewidth = 1) +
  annotate("text", x = critical_value + 1, y = max(null_dist)*1.3, label = expression("Reject "*H[0]), color = "darkred", size = 5, hjust = 0) +
  annotate("segment", x = critical_value+0.1, xend = critical_value+1, y = max(null_dist)*1.2, yend = max(null_dist)*1.2, arrow = arrow(ends = "last", length = unit(0.2, "cm")), color = "darkred", linewidth = 1) +
  scale_x_continuous(breaks = c(null_mean), labels = c(expression(mu[0]))) +
  theme_minimal(base_size = 18) +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10)) +
  labs(x = NULL, y = NULL) +
  annotate("text", x = max(x), y = -0.12*max(null_dist), label = expression(bar(x)[1] - bar(x)[2]), size = 6, hjust = 1)

# Alternative distribution plot (bottom)
p_alt<-ggplot(plot_df, aes(x = x, y = y, color = dist)) +
  geom_line(data = subset(plot_df, dist == "Alternative"), aes(x = x, y = y), color = "darkgreen", linewidth = 1) +
  geom_area(data = subset(plot_df, dist == "Alternative" & x >= critical_value), aes(x = x, y = y), color="darkgreen", fill = "darkgreen", alpha = 0.3) +
  geom_vline(xintercept = critical_value, linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = null_mean, linetype = "dotted", color = "steelblue") +
  geom_vline(xintercept = alt_mean, linetype = "dotted", color = "darkgreen") +
  annotate("text", x = null_mean - 3.4*se, y = max(null_dist)*0.1, label = expression(f[a]), color = "darkgreen", size = 5) +
  annotate("text", x = critical_value + 0.25, y = max(alt_dist)*0.1, label = "Power", color = "darkgreen", size = 5, hjust=0) +
  annotate("text", x = critical_value - 0.25, y = max(alt_dist)*0.1, label = expression(beta), color = "gray40", size = 5, hjust=1) +
  annotate("segment", x = null_mean, xend = alt_mean, y = -0.05, yend = -0.05, arrow = arrow(ends = "both", length = unit(0.2, "cm")), color = "black", linewidth = 1) +
  annotate("text", x = (null_mean + alt_mean)/2, y = -0.1, label = "Effect Size", color = "black", size = 5) +
  scale_x_continuous(breaks = c(null_mean, alt_mean), labels = c(expression(mu[0]), expression(mu[a]))) +
  theme_minimal(base_size = 18) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(10, 10, 30, 10)) +
  labs(x = NULL, y = NULL) +
  annotate("text", x = max(x), y = -0.12*max(alt_dist), label = expression(bar(x)[1] - bar(x)[2]), size = 6, hjust = 1)

# Combine the two plots vertically using patchwork (if available)
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined_plot <- p_null / p_alt
  print(combined_plot)
  ggsave("power_2sample_schematic.png", combined_plot, width = 4, height = 6, dpi = 300)
} else {
  print(p_null)
  print(p_alt)
}

