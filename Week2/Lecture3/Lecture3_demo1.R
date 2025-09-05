# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library
library(dplyr)    # load the data manipulation library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

source("Lecture3_demoFunctions.R")

# ---- 1. Generate data ----
# The "dataset" column indicates which dataset each observation corresponds to
data1 <- generate_data1()
head(data1)

# Count the number of observations for each dataset
table(data1$dataset)


# ---- 2. Plot histograms ----

# What do the following histograms of the datasets tell you?
# i.e. are they uni-modal/bimodal/multimodal, or uniform? What are the mode(s)?
# Are they skewed, left/right, or symmetric? Are there any outliers?

ggplot(data=data1, aes(x=x, fill=as.factor(dataset))) +
  geom_histogram() +
  facet_wrap(~dataset, ncol=1, scales = "free_y") +
  guides(fill = "none")

# Let's look at the datasets more closely:
# What is the mean here? What about the median? And the mode(s)?
# If the mean and median are different, why do you think that is?
data1_summary <- data1 %>%
  group_by(dataset) %>%
  summarise(
    mean = mean(x),
    median = median(x),
  )
print(data1_summary)

ggplot(data=data1, aes(x=x, fill=as.factor(dataset))) +
  geom_histogram() +
  facet_wrap(~dataset, ncol=1, scales = "free_y") +
  geom_vline(data = data1_summary, aes(xintercept = mean, col='mean')) +
  geom_vline(data = data1_summary, aes(xintercept = median, col='median')) +
  scale_color_manual(
    name = "Statistic",
    values = c(mean = "red", median = "blue"),
    labels = c(mean = "Mean", median = "Median")
  ) +
  guides(fill = "none")

