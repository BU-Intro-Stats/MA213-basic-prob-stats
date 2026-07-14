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

# P(X<x)
x_cut = 10
p4 <- ggplot(df, aes(x, y)) +
  geom_area(data = subset(df, x <= x_cut), fill = "orange", alpha = 0.7) +
  geom_line(size = 1) +
  annotate("text", x = x_cut, y = -0.01, label = "x", vjust = 1.2, color = "black", size = 5) +
  annotate("text", x = 6, y = 0.07, label = "P(X<x)", color = "black", size = 5) +
  labs(x = "", y = "Density")+
  scale_x_continuous(labels = NULL) +
  theme_minimal()

ggsave("fig_p_less_than_x.png", p4, width = 5, height = 3)

# P(X>a)
x_cut2 = 5
p5 <- ggplot(df, aes(x, y)) +
  geom_area(data = subset(df, x >= x_cut2), fill = "orange", alpha = 0.7) +
  geom_line(size = 1) +
  annotate("text", x = x_cut2, y = -0.01, label = "a", vjust = 1.2, color = "black", size = 5) +
  annotate("text", x = 6, y = 0.07, label = "P(X>a)", color = "black", size = 5) +
  labs(x = "", y = "Density")+
  scale_x_continuous(labels = NULL) +
  theme_minimal()

ggsave("fig_p_greater_than_a.png", p5, width = 5, height = 3)

# P(a<X,B)
x_cut3 = 8
p6 <- ggplot(df, aes(x, y)) +
  geom_area(data = subset(df, x_cut2<= x & x <= x_cut3), fill = "orange", alpha = 0.7) +
  geom_line(size = 1) +
  annotate("text", x = x_cut2, y = -0.01, label = "a", vjust = 1.2, color = "black", size = 5) +
  annotate("text", x = x_cut3, y = -0.01, label = "b", vjust = 1.2, color = "black", size = 5) +
  annotate("text", x = 6.5, y = 0.07, label = "P(a<X<b)", color = "black", size = 5) +
  labs(x = "", y = "Density")+
  scale_x_continuous(labels = NULL) +
  theme_minimal()

ggsave("fig_p_between_a_and_b.png", p6, width = 5, height = 3)

# Normal P(X>x)
mean3 <- 1
sd3 <- 2
x2 <- seq(-7, 9, length.out = 1000)
y2 <- dnorm(x2, mean = mean3, sd = sd3)
df2 <- data.frame(x = x2, y = y2)

x_cut4 = 5
p7 <- ggplot(df2, aes(x, y)) +
  geom_area(data = subset(df2, x >= x_cut4), fill = "orange", alpha = 0.7) +
  geom_line(size = 1) +
  annotate("text", x = 1, y = 0.05, label = "mu==1~','~sigma==2", parse = TRUE, size = 5) +
  labs(x = "X", y = "Density") +
  theme_minimal()

ggsave("fig_normal.png", p7, width = 5, height = 3)


