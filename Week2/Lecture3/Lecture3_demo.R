# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

set.seed(42)
source("Lecture3_demoFunctions.R")

# ---- 1. Generate data ----

datasets <- generate_data()

data1 <- datasets$data1
head(data1)
data2 <- datasets$data2
data3 <- datasets$data3
data4 <- datasets$data4


# ---- 2. Plot histograms ----

# What do the following histograms of the datasets tell you?
# i.e. are they uni-modal/bimodal/multimodal, or uniform? What are the mode(s)?
# Are they skewed, left/right, or symmetric? Are there any outliers?

ggplot(data=data1, aes(x=x1)) +
  geom_histogram(bins=15, col='gray')

ggplot(data=data2, aes(x=x2)) +
  geom_histogram(bins=15, col='gray')

ggplot(data=data3, aes(x=x3)) +
  geom_histogram(bins=15, col='gray')

ggplot(data=data4, aes(x=x4)) +
  geom_histogram(bins=15, col='gray')


# Let's look at one of the datasets more closely:
# What is the mean here? What about the median? And the mode(s)?
# If the mean and median are different, why do you think that is?

ggplot(data=data2, aes(x=x2)) +
  geom_histogram(bins=15, col='gray') +
  geom_vline(xintercept=mean(data2$x2), col='red') +
  geom_vline(xintercept=median(data2$x2), col='orange')

