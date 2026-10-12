library(ggplot2)

# ---- 2b. What would happen if ... ----

sample_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}

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

sample_plot_sampling_dist_fn(250000, 0.5, 100, 5000, sample_phat_fn)
sample_plot_sampling_dist_fn(250000, 0.99, 100, 5000, sample_phat_fn)
sample_plot_sampling_dist_fn(250000, 0.2, 10, 5000, sample_phat_fn)


# Does this distribution look familiar? It is just like Binomial(10,0.2)!

# Plot a binomial PMF and add another x axis for proportions
n_binom <- 10
p_binom = 0.2
binom_outcomes <- 0:n_binom
binom_pmf <- dbinom(binom_outcomes, size = n_binom, prob = p_binom)
binom_pmf_df <- data.frame(
  successes = binom_outcomes,
  probability = binom_pmf
)

p <- ggplot(binom_pmf_df, aes(x = successes, y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = paste0("Binomial PMF (n = ", n_binom, ", p = ", p_binom, ")"),
       x = "Number of Successes (out of 10)",
       y = "Probability")

# Create a new axis: 0 to 10, matching the 0-1 scale
axis_vals <- seq(0, 1, by=0.1)
axis_labels <- as.character(axis_vals)
# Convert 0-1 to 0-10 scale for positioning
axis_breaks <- axis_vals * 10

# Add the secondary axis using ggplot2's sec_axis
p2 <- p + scale_x_continuous(breaks = binom_outcomes,
  sec.axis = sec_axis(~ . , name = "Proportion of successes (out of 1)", breaks = axis_breaks, labels = axis_labels)
)

print(p2)
#ggsave("sampling_dist_of_phat.png", p2, width = 4, height = 2.5, units = "in")
