# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

setwd(dirname(getSourceEditorContext()$path))  # set working directory

# Load data
scores <- read.csv("scores.csv")


# ---- 1. Analyzing the difference of the scores ----

scores$diff <- scores$read - scores$write

# First, let's plot the new 'diff' variable
ggplot(data=scores, aes(x=diff)) +
  geom_histogram(alpha=0.6, binwidth=3)



# ----------------------------------------------
# ---- 2. Computations ----

# Sample mean and standard deviation
xbar <- mean(scores$diff)
s <- sd(scores$diff)


# ---- 3. Test statistic and p-value ----

SE <- s / sqrt(length(scores$diff))
Tstat = (xbar - 0) / SE


# Calculate the tail area of the t distribution
pval = 2*pt(Tstat, df=200-1, lower.tail=TRUE)

# ----------------------------------------------
# ---- 4. 95% Confidence interval ----

CI = xbar+c(-1,1)*qt(p = 0.975, df = 200-1)*SE
