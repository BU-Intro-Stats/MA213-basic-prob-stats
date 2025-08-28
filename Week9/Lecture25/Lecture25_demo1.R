# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# Load data
data <- read.csv("scores.csv")


# ---- 1. Analyzing the difference of the scores ----

scores$diff <- scores$read - scores$write

# First, let's plot the new 'diff' variable
ggplot(data=scores, aes(x=diff)) +
  geom_histogram(alpha=0.6, binwidth=3)

