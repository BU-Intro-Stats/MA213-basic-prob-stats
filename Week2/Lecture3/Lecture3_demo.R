# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

set.seed(42)
source("Lecture3_demoFunctions.R")

# ---- 1. Generate data ----

datasets <- generate_data1()

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


# ---- 2. Comparing shapes of distributions and their outliers ----

# Let's try changing the variance of our distribution and see how its 
# shape changes:
data5 <- generate_data2()
head(data5)

ggplot(data=data5, aes(x=x, fill=as.factor(variance))) +
  geom_histogram(col='gray', alpha=0.6)

# We can also examine these data with boxplots:
ggplot(data=data5, aes(x=x, color=as.factor(variance))) +
  geom_boxplot()

ggplot(data=data5, aes(x=x, fill=as.factor(variance))) +
  geom_histogram(col='gray', alpha=0.6) +
  geom_boxplot

# Now let's look at the highest-variance distribution more closely:
data6 <- data5[data5$variance == 100,] 
lower_sd <- mean(data6$x) - 2*sd(data6$x)
upper_sd <- mean(data6$x) + 2*sd(data6$x)

# Q: keep the boxplot in this histogram? feels a little crowded
ggplot(data=data6, aes(x=x)) +
  geom_histogram(col='gray')  +
  geom_histogram(col='gray', alpha=0.6) +
  geom_boxplot(outlier.color='black',   
               outlier.shape=16, outlier.size=2, notch=FALSE) +
  geom_vline(xintercept=mean(data6$x), col='red') +
  geom_vline(xintercept=lower_sd, col='orange') +
  geom_vline(xintercept=upper_sd, col='orange')
