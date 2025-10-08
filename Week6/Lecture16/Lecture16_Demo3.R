library(ggplot2)

# Testing a cross-platform working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
#print(getSourceEditorContext()$path)
setwd(dirname(getSourceEditorContext()$path))

source("Lecture16_DemoFunctions.R")
source("Lecture16_ShinyApp.R")

# ---- 1. From last time: sampling from a population, simulation ----
# Create the population
pop_N <- 250000
pop_proportion <- 0.88
population <- c(rep("support", pop_proportion*pop_N), rep("not", (1-pop_proportion)*pop_N))

# Sample 100 samples from it, 5000 times
K <- 5000  # Simulation size (repeat the experiment K times)
sample_size <- 100 # Sample size

sample_phat_fn <- function(pop, n) {
  sampled_entries <- sample(pop, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}

simulation <- replicate(K, sample_phat_fn(population, n=sample_size))

# plot the results
title <- sprintf("Sampling distribution of phat\np=%s; sample size=%s; repeats=%s", 
                 pop_proportion, sample_size,K)  
ggplot(data=data.frame(simulation), aes(x=simulation)) +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  geom_histogram(bins=100, alpha=0.5, color=4, fill="white") +
  labs(title=title, x="Sample proportion", y="Frequency") +
  xlim(c(0,1))


# ---- 2. Normal approximation ----
# We derived the mean and standard error for phat using the Binomial distribution
# E[phat]=p
# StdErr(phat)=sqrt(p(1-p)/n)

mean_phat = pop_proportion
SE_phat <- sqrt((pop_proportion*(1-pop_proportion))/sample_size)

# Calculate a normal density with the same mean and standard error 
# (we did this in Lecture14_demo1)
x_vals <- seq(0, 1, length.out = 1000)
normal_pdf <- dnorm(x_vals, mean = mean_phat, sd = SE_phat)
normal_pdf_df <- data.frame(
  x = x_vals,
  prob = normal_pdf
)

# Plot the histogram with the Normal density:
ggplot(data=as.data.frame(simulation), aes(x=simulation)) +
  geom_histogram(aes(y=after_stat(density)), 
                 breaks=seq(0,1,0.01), alpha=0.5, color=4, fill="white") +
  geom_line(
    data = normal_pdf_df,
    aes(x=x, y=prob),
    color = "darkred"
  ) +
  xlab("Sample proportion") +
  ggtitle(title) +
  xlim(c(0,1))

# 3. ---- Repeat the experiment for different sample sizes ---- 
# Q: What happens to the distributions as sample size increases?

# Run Rshiny app
shinyApp(ui = ui, server = server)

