# Load required library
library(ggplot2)

# Set parameters for the mixture of two normals
mean1 <- 6
sd1 <- 1.2
mean2 <- 11
sd2 <- 1
weight1 <- 0.6
weight2 <- 0.4

# Create a weird continuous density: mixture of two normals
set.seed(42)
x <- seq(0, 15, length.out = 1000)
y <- weight1 * dnorm(x, mean = mean1, sd = sd1) + weight2 * dnorm(x, mean = mean2, sd = sd2)

# (a) Whole distribution shaded
df <- data.frame(x = x, y = y)
p1 <- ggplot(df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.7) +
  labs(x = "X", y = "Density") +
  theme_minimal()
ggsave("fig_a_whole_area.png", p1, width = 5, height = 3)

# (b) Shaded outside of min_x < X < max_x
min_x <- 5
max_x <- 10
df$region <- ifelse(df$x < min_x | df$x > max_x, "outside", "inside")
p2b <- ggplot(df, aes(x, y)) +
  # Shade left tail
  geom_area(data = subset(df, x < min_x), fill = "orange", alpha = 0.7) +
  # Shade right tail
  geom_area(data = subset(df, x > max_x), fill = "orange", alpha = 0.7) +
  # Shade middle region
  geom_area(data = subset(df, x >= min_x & x <= max_x), fill = "grey80", alpha = 0.7) +
  labs(x = "X", y = "Density") +
  theme_minimal()
ggsave("fig_b_outside_5_10.png", p2b, width = 5, height = 3)

# compute area inside
p <- weight1 * (pnorm(max_x, mean = mean1, sd = sd1) - pnorm(min_x, mean = mean1, sd = sd1)) +
  weight2 * (pnorm(max_x, mean = mean2, sd = sd2) - pnorm(min_x, mean = mean2, sd = sd2))

print(p)

# (c) Vertical line at X = 7
p3 <- ggplot(df, aes(x, y)) +
  geom_area(fill = "grey80", alpha = 0.5) +
  geom_vline(xintercept = 7, color = "red", linewidth = 1) +
  labs(x = "X", y = "Density") +
  theme_minimal()
ggsave("fig_c_pr_x_eq_7.png", p3, width = 5, height = 3)