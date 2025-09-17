# Data generation for post lab activity 3

library(tidyverse)

set.seed(100)

# Parameters
N <- 97
extra_n <- 3
total_n <- N + extra_n

# ---- A: Generate data for boxplot ----
A <- rnorm(N, mean = 0, sd = 1)
# Append extra values
A <- c(A, c(5.001023, 4.23992, 6.212323))
A <- round(A, 5)
# Randomize order
A <- sample(A)

# Create initial dataframe
df <- tibble(
  N = seq_len(total_n),
  A = A
)

# Boxplot for A
df %>%
  ggplot(aes(y = A)) +
  geom_boxplot() +
  ggtitle("Boxplot of A")

# Alternative base R boxplot
boxplot(df$A, main = "Boxplot of A (base R)")

# ---- B and C: Data for scatter plot ----
B <- seq(-5, 4.9, by = 0.1)
C <- B^2

df <- df %>%
  mutate(B = B, C = C)

# Scatter plot B vs C
plot(df$B, df$C, main = "Scatter plot of B vs C", xlab = "B", ylab = "C")

# ---- D: Left skewed data ----
D <- rgamma(total_n, shape = 0.5, scale = 2)
D <- max(D) - D  # Reflect to create left skew

df <- df %>%
  mutate(D = D)

# Histogram of D
ggplot(df, aes(x = D)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  ggtitle("Histogram of left-skewed D")

# Boxplot of D
df %>%
  ggplot(aes(y = D)) +
  geom_boxplot() +
  ggtitle("Boxplot of D")

# ---- E: Categorical data for barplot ----
E <- c(rep("A", 7), rep("B", 13), rep("C", 20), rep("D", 27), rep("E", 33))
E <- sample(E)

df <- df %>%
  mutate(E = E)

# Count frequencies of E
new_df <- df %>%
  count(E)

# Bar plot of counts
ggplot(new_df, aes(x = E, y = n, fill = E)) +
  geom_col(show.legend = FALSE) +
  ggtitle("Barplot of categories in E") +
  ylab("Count")

# ---- Save data ----
write.csv(df, "somedata.csv", row.names = FALSE)

# ---- Read and inspect saved data ----
my_df <- read.csv("somedata.csv")
head(my_df)