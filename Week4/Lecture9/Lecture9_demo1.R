# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

# ---- 1. Card game: draw one ----

# Create a list of the outcomes and their probabilities
outcomes <- c(1, 5, 10, 0)
probabilities <- c(12/52, 4/52, 1/52, 35/52)

# Draw a single card
draw <- sample(outcomes, 1, replace=TRUE, prob=probabilities)
print(draw)

# Why are we sampling with replacement?


# ---- 2. Draw many ----

samples <- sample(outcomes, 1000, replace=TRUE, prob=probabilities)
samples <- as.data.frame(samples)
print(samples)

# plot a histogram of the counts...
ggplot(data=samples, aes(x=samples)) +
  geom_histogram(color="black", fill="blue", binwidth=1) +
  scale_x_continuous(breaks=c(0:10))

# ... or the proportions
ggplot(data=samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), color="black", fill="blue", binwidth=1) +
  scale_x_continuous(breaks=c(0:10))

# Compare the histogram of proportions to the theoretical distribution
# (a bar chart of the probabilities of each outcome)
theoretical <- data.frame(outcomes, probabilities)
ggplot(data=theoretical, aes(x=outcomes, y=probabilities)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=c(0:10))

# ---- 3. Comparing the samples and the theoretical distribution ----

# Compute the sample mean:
sample_mean <- mean(samples$samples)
  
# Compute the theoretical expected value:
ev <- (12/52)*1 + (4/52)*5 + (1/52)*10 + (35/52)*0 

# Compute the sample variance:
sample_var <- sd(samples$samples)**2

# Compute the theoretical variance:
var <- (12/52)*(1-ev)**2 + (4/52)*(5-ev)**2 + (1/52)*(10-ev)**2 + (35/52)*(0-ev)**2

# Add all the values to a table to print
tab <- matrix(c(sample_mean, ev, sample_var, var), ncol=2, byrow=TRUE)
colnames(tab) <- c("Mean", "Variance")
rownames(tab) <- c("Sample", "Theoretical")

print(tab)

