# BP example: required sample size for 80% power (two-sided, alpha = 0.05)
# Null: mu_trmt - mu_ctrl = 0; Alternative: mu_trmt - mu_ctrl = -3
# SE is chosen so that power is exactly 80%: SE = 3 / (z_0.975 + z_0.80)

library(ggplot2)

null_mean <- 0
alt_mean  <- -3
alpha     <- 0.05
power     <- 0.80

z_alpha <- qnorm(1 - alpha / 2)   # 1.96
z_power <- qnorm(power)           # 0.84
se      <- abs(alt_mean - null_mean) / (z_alpha + z_power)   # 1.0708
cut_lo  <- null_mean - z_alpha * se                          # -2.10
cut_hi  <- null_mean + z_alpha * se                          #  2.10

x <- seq(-10, 10, length.out = 2000)
null_df <- data.frame(x = x, y = dnorm(x, null_mean, se))
alt_df  <- data.frame(x = x, y = dnorm(x, alt_mean, se))
peak    <- max(null_df$y)

col_null <- "#569BBD"
col_alt  <- "#4D7A1F"
col_cut  <- "#E5432F"
col_z1   <- "#C9A400"   # 0.84 SE arrow (yellow)
col_z2   <- "#B22222"   # 1.96 SE arrow (red)

arrow_y <- -0.10 * peak
label_y <- -0.22 * peak
arw     <- arrow(ends = "both", length = unit(0.12, "cm"))

p <- ggplot() +
  geom_area(data = subset(alt_df, x <= cut_lo), aes(x, y),
            fill = col_alt, alpha = 0.5) +
  geom_line(data = alt_df,  aes(x, y), color = col_alt,  linewidth = 1) +
  geom_line(data = null_df, aes(x, y), color = col_null, linewidth = 1) +
  geom_segment(aes(x = c(cut_lo, cut_hi), xend = c(cut_lo, cut_hi),
                   y = 0, yend = 0.75 * peak),
               color = col_cut, linetype = "dotted", linewidth = 0.8) +
  geom_segment(aes(x = c(null_mean, alt_mean), xend = c(null_mean, alt_mean),
                   y = 0, yend = peak),
               color = c(col_null, col_alt), linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  annotate("text", x = alt_mean - 1.4, y = 1.02 * peak, hjust = 1, color = col_alt,
           size = 4.5, label = "Distribution with") +
  annotate("text", x = alt_mean - 1.4, y = 0.80 * peak, hjust = 1, color = col_alt,
           size = 4.5, label = "mu[trmt] - mu[ctrl] == -3", parse = TRUE) +
  annotate("text", x = null_mean + 1.2, y = 0.9 * peak, hjust = 0, color = col_null,
           size = 4.5, label = "Null distribution") +
  annotate("text", x = -3.45, y = 0.28 * peak, color = "white",
           size = 3.6, fontface = "bold", label = "Power\n= 80%") +
  scale_x_continuous(breaks = seq(-9, 9, 3), limits = c(-10, 10)) +
  scale_y_continuous(limits = c(label_y * 1.25, 1.12 * peak)) +
  labs(x = expression(bar(x)[trmt] - bar(x)[ctrl]), y = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_line(), axis.ticks.length.x = unit(0.15, "cm"))

arrows <- list(
  # 0.84 SE: from alternative center to cutoff
  annotate("segment", x = alt_mean, xend = cut_lo, y = arrow_y, yend = arrow_y,
           arrow = arw, color = col_z1, linewidth = 0.9 ),
  annotate("text", x = alt_mean - 0.15, y = arrow_y, hjust = 1, label = "0.84 SE",
           color = col_z1, fontface = "bold", size = 4 ),
  # 1.96 SE: from cutoff to null center
  annotate("segment", x = cut_lo, xend = null_mean, y = arrow_y, yend = arrow_y,
           arrow = arw, color = col_z2, linewidth = 0.9 ),
  annotate("text", x = (cut_lo + null_mean) / 2, y = label_y, label = "1.96 SE",
           color = col_z2, fontface = "bold", size = 4) 
)

# Same canvas and limits for both, so the slide build does not shift
ggsave("power_bp_80pct_noarrows.png", p,          width = 7, height = 2.1, dpi = 300)
ggsave("power_bp_80pct.png",          p + arrows, width = 7, height = 2.1, dpi = 300)
