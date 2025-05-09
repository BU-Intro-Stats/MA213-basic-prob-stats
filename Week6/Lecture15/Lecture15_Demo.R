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
pop_size <- 250000
pop_proportion <- 0.88

# create the population
population <- create_population(N=pop_size, p=pop_proportion)

# Check that the proportion of "support" is 0.88
sum(population == "support") / pop_size 

# Visualize the population with a chart
ggplot(data=data.frame(population), aes(x=population)) +
  geom_bar(color=4, fill=4) + 
  ylim(0,pop_size) +
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

# ---- 2b. What would happen if ... ----
sample_plot_sampling_dist_fn <- function(pop_size,pop_proportion,sample_size,K,
                                         sample_phat_fn){
  # Create the new population
  population <- create_population(N=pop_size, p=pop_proportion)
  
  # Run the experiment K times
  simulation <- replicate(K, sample_phat_fn(population, n=sample_size))
  
  # mean and standard deviation of sampling distribution
  m = mean(simulation)
  s = sd(simulation)
  
  # plot the Histogram of phat values
  title <- sprintf("Population size=%s; p=%s; sample size=%s; repeats=%s\n Mean=%.3f; St Dev=%.3f", 
                   pop_size, pop_proportion, sample_size,K,m,s)  
  
  ggplot(data=data.frame(simulation), aes(x=simulation)) +
    geom_histogram(breaks=seq(0,1,0.01), color=4, fill="white") +
    geom_vline(aes(xintercept=pop_proportion), color="red") +
    annotate("pointrange", x=m, xmin=m-2*s,xmax=m+2*s,y=K/50, color="blue") +
    labs(title=title, x="Sample proportion (phat)", y="Count")
}

sample_plot_sampling_dist_fn(250000, 0.2, 100, 5000, sample_phat_fn)
sample_plot_sampling_dist_fn(250000, 0.99, 100, 5000, sample_phat_fn)
sample_plot_sampling_dist_fn(250000, 0.2, 10, 5000, sample_phat_fn)

# ---- 3. Estimating p-hat for a small sample - introducing bias ----
# Imagine that we have a small sample size and/or a true
# population proportion that is heavily weighted to one side or the other 
# (ie very close to 0 or very close to 1), so that we may not actually 
# observe both successes and failures in our sample. The Rule of Succession was
# designed for this situation. It defines a new estimator (k+1)/(n+2), that 
# cannot be exactly 0 or exactly 1.

sample_plot_sampling_dist_fn(250000, 0.01, 20, 5000, sample_phat_fn)

succession_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- (sum(sampled_entries == "support")+1) / (n+2)
  return(phat)
}

sample_plot_sampling_dist_fn(250000, 0.01, 20, 5000, succession_phat_fn)


