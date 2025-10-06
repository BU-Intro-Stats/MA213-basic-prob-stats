library(ggplot2)



# ---- Define our function to sample and plot the sampling distribution  ----
sample_plot_sampling_dist_fn <- function(pop_N,pop_proportion,sample_size,K,
                                         sample_phat_fn){
  # Create the new population
  population <- c(rep("support", pop_proportion*pop_N), rep("not", (1-pop_proportion)*pop_N))
  
  # Run the experiment K times
  simulation <- replicate(K, sample_phat_fn(population, n=sample_size))
  
  # mean and standard deviation of sampling distribution
  m = mean(simulation)
  s = sd(simulation)
  
  # plot the Histogram of phat values
  title <- sprintf("Population size=%s; p=%s; sample size=%s; repeats=%s\n Mean=%.3f; St Dev=%.3f", 
                   pop_N, pop_proportion, sample_size,K,m,s)  
  
  ggplot(data=data.frame(simulation), aes(x=simulation)) +
    geom_histogram(breaks=seq(0,1,0.01), color=4, fill="white") +
    geom_vline(aes(xintercept=pop_proportion), color="red") +
    annotate("pointrange", x=m, xmin=m-2*s,xmax=m+2*s,y=K/50, color="blue") +
    labs(title=title, x="Sample proportion (phat)", y="Count")
}

# ---- 3. Estimating p-hat for a small sample - introducing bias ----
# Imagine that we have a small sample size and/or a true
# population proportion that is heavily weighted to one side or the other 
# (ie very close to 0 or very close to 1), so that we may not actually 
# observe both successes and failures in our sample. The Rule of Succession was
# designed for this situation. It defines a new estimator (k+1)/(n+2), that 
# cannot be exactly 0 or exactly 1.

# Plot the sampling distribution of the sample proportion
# Population size: 250000
# Population proportion: 0.01
# Sample size: 20 (small)
# compute the sample proportion
# Repeat the experiment 5000 times
sample_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}
sample_plot_sampling_dist_fn(250000, 0.01, 20, 5000, sample_phat_fn)
# over 80% of the time, phat=0 (i.e. you're estimating that the population has
# no "support" in it. It could be risky to assume there are
# no supporters after observing only 20 people!)

# Plot the sampling distribution for the rule of succession
# Population size: 250000
# Population proportion: 0.01
# Sample size: 20 (small)
# compute the rule-of-succession proportion estimator (k+1)/(n+2)
# Repeat the experiment 5000 times
succession_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- (sum(sampled_entries == "support")+1) / (n+2)
  return(phat)
}
sample_plot_sampling_dist_fn(250000, 0.01, 20, 5000, succession_phat_fn)
# even if there are no "support" in the sample of size 20, the rule of 
# succession will estimate phat=1/21. It is *biased*, but in a way that reduces
# the risk of estimating 0 as the population proportion.


# How would these estimators compare if the sample sizes were much larger (200)?
sample_plot_sampling_dist_fn(250000, 0.01, 200, 5000, sample_phat_fn)
sample_plot_sampling_dist_fn(250000, 0.01, 200, 5000, succession_phat_fn)
# --> with more evidence, the rule of succession becomes less biased
# and the two estimators agree more closely 

# You might want to use the rule of succession in situations where you don't have
# a lot of data, and e.g.:
# (1) there are negative risks associated with estimating the population 
#   proportion as exactly 0 or exactly 1
# (2) you have prior reason to believe that the population proportion isn't 
#   exactly equal to 0 or 1
