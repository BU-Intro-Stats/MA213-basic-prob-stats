library(ggplot2)

# ---- 1. Sampling from a population ----
# ---- 1a. Creating the population ----
# Create a set of 250 thousand entries, where 88% of them are "support"
# and 12% are "not".
pop_N <- 250000
pop_proportion <- 0.88

# create the population
population <- c(rep("support", pop_proportion*pop_N), rep("not", (1-pop_proportion)*pop_N))

# Check that the proportion of "support" is 0.88
sum(population == "support") / pop_N 

# Visualize the population with a bar chart
ggplot(data=data.frame(population), aes(x=population)) +
  geom_bar(color=4, fill=4) + 
  ylim(0,pop_N) +
  ggtitle(sprintf("Population proportion (p): %s", pop_proportion))

# ---- 1b. Take samples and compute phat ----

# Q: if we sample from the population multiple times, do you think the samples
# will all be the same, or different?
sample_size <- 1000

samples <- sample(population, size=sample_size)
head(samples,n=10)
sample_proportion <- sum(samples == "support") / sample_size
ggplot(data=data.frame(samples), aes(x=samples)) +
  geom_bar(color=4, fill=4) + ylim(0,sample_size) + 
  ggtitle(sprintf("Sample proportion (phat): %s", sample_proportion))

# Q: Do you think a chart of the samples will look like the chart of the 
# population values? 

# ---- 2. Repeat the experiment 5000 times ----
K <- 5000  # Simulation size

# Write a function to run the simulation and compute p-hat:
sample_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}

# Simulate the experiment 5000 times
simulation <- replicate(K, sample_phat_fn(population, n=sample_size))

# mean and standard deviation of sampling distribution
m = mean(simulation)
s = sd(simulation)

# Plot the results
title <- sprintf("Histogram of p-hat values from repeating the experiment %s times, \np=%s, sample size=%s", 
                 K, pop_proportion, sample_size)  # in case we change the sample/simulation size
ggplot(data=data.frame(simulation), aes(x=simulation)) +
  geom_histogram(breaks=seq(0,1,0.01), color=4, fill="white") +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  annotate("pointrange", x=m, xmin=m-2*s,xmax=m+2*s,y=K/50, color="blue") +
  labs(title=title, x="Sample proportion (phat)", y="Count") 

# Q: What is the shape and center of this distribution?
# Q: Based on this distribution, what is a good guess for the population proportion?
# Q: Why have we set the xlimits of the graph to be (0,1)?

