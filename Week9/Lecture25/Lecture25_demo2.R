# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# Load data
scores <- read.csv("scores.csv")

# As before, add a column for the difference between the reading and writing scores
scores$diff <- scores$read - scores$write


# ---- 1. Computations ----

# Sample mean and standard deviation
xbar <- mean(scores$diff)
s <- sd(scores$diff)


# ---- 2. Test statistic and p-value ----

SE <- s / sqrt(length(scores$diff))
Tstat = (xbar - 0) / SE


# Calculate the tail area of the t distribution
2*pt(Tstat, df=200-1, lower.tail=FALSE)

# TODO: Emily please check, feel like I'm going crazy trying to make sense of
# the computation in the slides (slide 9)
