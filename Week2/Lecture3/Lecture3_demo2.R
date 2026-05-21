# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library
library(dplyr)    # load the data manipulation library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

source("Lecture3_demoFunctions.R")


# ---- 2. Comparing shapes of distributions and their outliers ----

# Let's try looking at distributions with different variances:
data2 <- generate_data2()
head(data2)

table(data2$dataset)

# Look at the distributions
ggplot(data=data2, aes(x=x, fill=as.factor(dataset))) +
  geom_histogram() +
  facet_wrap(~dataset, ncol=1) +
  guides(fill = "none")

# Compute summary statistics for central tendency and variability
data2_summary <- data2 %>%
  group_by(dataset) %>%
  summarise(
    mean = round(mean(x),3),
    variance = var(x),
    std_dev = sd(x),
    Q1 = quantile(x, 0.25),
    median = median(x),
    Q3 = quantile(x, 0.75),
    IQR = IQR(x)
  )
print(data2_summary)


# Plot the distributions again, with lines to show mean+/-2SD
plot <- histograms_plusminus2sd(data2)
print(plot)

# We can also look at the IQRs:
plot2 <- histograms_plusIQR(data2)
print(plot2)

# What if the distributions are skewed?
data3 <- generate_data3()

print(histograms_plusminus2sd(data3))
print(histograms_plusIQR(data3))

