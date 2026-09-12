# Lecture 4, pages 14--15: extreme observations and robust statistics

# Make sure the working directory contains both this file and
# house_income_sorted.csv before running the script.

library(ggplot2)

# ---- 1. Original household-income data ----

df <- read.csv("house_income_sorted.csv")
income <- df$house_income

head(df)
length(income)       # sample size
summary(income)

# Calculate the four statistics used in the lecture.
median_original <- median(income)
IQR_original <- IQR(income)
mean_original <- mean(income)
sd_original <- sd(income)

median_original
IQR_original
mean_original
sd_original

# ---- 2. Move the largest observation to $10 million ----

income_largest_moved <- income
income_largest_moved[which.max(income_largest_moved)] <- 10000000

# 10,000,000 is ten million.  We replace one value; we do not add a value.

median_largest_moved <- median(income_largest_moved)
IQR_largest_moved <- IQR(income_largest_moved)
mean_largest_moved <- mean(income_largest_moved)
sd_largest_moved <- sd(income_largest_moved)

median_largest_moved
IQR_largest_moved
mean_largest_moved
sd_largest_moved

# ---- 3. Move the smallest observation to $10 million ----

income_smallest_moved <- income
income_smallest_moved[which.min(income_smallest_moved)] <- 10000000

median_smallest_moved <- median(income_smallest_moved)
IQR_smallest_moved <- IQR(income_smallest_moved)
mean_smallest_moved <- mean(income_smallest_moved)
sd_smallest_moved <- sd(income_smallest_moved)

median_smallest_moved
IQR_smallest_moved
mean_smallest_moved
sd_smallest_moved

# ---- 4. Put all three plots together ----

# Combine the three vectors into one data frame.  The scenario column tells
# ggplot which panel each observation belongs to.
plot_data <- rbind(
  data.frame(
    scenario = "Original data",
    house_income = income
  ),
  data.frame(
    scenario = "Largest moved to $10 million",
    house_income = income_largest_moved
  ),
  data.frame(
    scenario = "Smallest moved to $10 million",
    house_income = income_smallest_moved
  )
)

plot_data$scenario <- factor(
  plot_data$scenario,
  levels = c(
    "Original data",
    "Largest moved to $10 million",
    "Smallest moved to $10 million"
  )
)

# Use the same x- and y-axis ranges in every panel.  stackdir = "up" makes the
# dots grow upward from y = 0 instead of being centered around y = 0.
combined_plot <- ggplot(plot_data, aes(x = house_income)) +
  geom_dotplot(
    binaxis = "x",
    stackdir = "up",
    binwidth = 25000,
    dotsize = 0.7
  ) +
  facet_wrap(~ scenario, ncol = 1, scales = "fixed") +
  labs(
    title = "Effect of moving one observation to $10 million",
    x = "Household income ($)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

print(combined_plot)

# ---- 5. Put the results next to each other ----

comparison <- data.frame(
  scenario = c(
    "Original data",
    "Largest moved to $10 million",
    "Smallest moved to $10 million"
  ),
  median = c(
    median_original,
    median_largest_moved,
    median_smallest_moved
  ),
  IQR = c(
    IQR_original,
    IQR_largest_moved,
    IQR_smallest_moved
  ),
  mean = c(
    mean_original,
    mean_largest_moved,
    mean_smallest_moved
  ),
  sd = c(
    sd_original,
    sd_largest_moved,
    sd_smallest_moved
  )
)

comparison

# Values in thousands of dollars, as shown on the lecture slide.
comparison_in_thousands <- comparison
comparison_in_thousands$median <- round(comparison$median / 1000)
comparison_in_thousands$IQR <- round(comparison$IQR / 1000)
comparison_in_thousands$mean <- round(comparison$mean / 1000)
comparison_in_thousands$sd <- round(comparison$sd / 1000)

comparison_in_thousands

# Notice: the median and IQR change very little, while the mean and SD change
# substantially.  This is why median and IQR are called more robust.
