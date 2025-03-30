library(ggplot2)

# Testing a cross-platform working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
#print(getSourceEditorContext()$path)
setwd(dirname(getSourceEditorContext()$path))

source("Lec15DemoFunctions.R")


# ---- 1. Sampling from a population ----

# ---- 1a. Creating the population ----
# Create a set of 250 thousand entries, where 88% of them are "support"
# and 12% are "not".
pop_size <- 25000
pop_proportion <- 0.65
population <- create_population(N=pop_size, p=pop_proportion)
head(population)
sum(population == "support") / pop_size  # Should be 0.88
# Sanity check: visualize the population with a chart
ggplot(data=data.frame(population), aes(x=population)) +
  geom_bar(color=4, fill=4)


# ---- 1b. Take samples ----

# Q: if we sample from the population multiple times, do you think the samples
# will all be the same, or different?
sample_size <- 1000

# Run these lines more than once and see what happens:
# Note: can also change the sample size above and observe!
samples <- sample(population, size=sample_size)
head(samples)
ggplot(data=data.frame(samples), aes(x=samples)) +
  geom_bar(color=4, fill=4)

# Q: Do you think a chart of the samples will look like the chart of the 
# population values? 


# ---- 1c. Computing p-hat, sampling variability ----
# Q: How would you compute p-hat from the samples?
# Q: What happens to p-hat as the sample size increases? Which value of p-hat 
# do you think will be closer to the "true" (population) value?

phat <- sum(samples == "support") / sample_size


# ---- 2. First simulation: taking many samples ----
K <- 1000  # Simulation size

# Write a function to run the simulation and compute p-hat:
sample_get_phat_fn <- function(population, n) {
  sampled_entries <- sample(population, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}

simulation <- replicate(K, sample_get_phat_fn(population, n=sample_size))
title <- sprintf("Histogram of p-hat values from experiment with %sx%s samples", 
                 K, sample_size)  # in case we change the sample/simulation size

ggplot(data=data.frame(simulation), aes(x=simulation)) +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  geom_histogram(bins=100, alpha=0.5, color=4, fill="white") +
  labs(title=title, x="Sample proportion", y="Frequency") +
  xlim(c(0,1))

# Q: Why have we set the xlimits of the graph to be (0,1)?
# Q: What determines where the center or mean of the sampling distribution will
# fall?  


# ---- 3. Estimating p-hat for a small sample - introducing bias ----
# Imagine that we have a very small sample size, eg. 10 or fewer, and a true
# population proportion that is heavily weighted to one side or the other 
# (ie very close to 0 or very close to 1). This means we may not actually 
# observe both successes and failures in our sample, so the sample p-hat will
# be wrong. However, we can adjust our estimator for this case!

# To illustrate the problem, try this new setting:
new_pop_size <- 25000
new_pop_proportion <- 0.9
new_population <- create_population(N=new_pop_size, p=new_pop_proportion)
head(new_population)
sum(new_population == "support") / new_pop_size  # Should be 0.9

new_sample_size <- 5
new_samples <- sample(new_population, size=new_sample_size)
head(new_samples)
ggplot(data=data.frame(new_samples), aes(x=new_samples)) +
  geom_bar(color=4, fill=4)

# First, compute p-hat as we have been
phat_unbiased <- sum(new_samples == "support") / new_sample_size
print(phat_unbiased)

# Now try this method, called the Rule of Succession estimator:
phat_biased <- sum(new_samples == "support") / (new_sample_size + 2)
print(phat_biased)

