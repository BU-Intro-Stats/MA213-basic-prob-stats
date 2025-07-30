library(ggplot2)

# Testing a cross-platform working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
#print(getSourceEditorContext()$path)
setwd(dirname(getSourceEditorContext()$path))

source("Lec16DemoFunctions.R")
source("Lec16ShinyApp.R")

# ---- 1. From last time: sampling from a population, simulation ----

pop_size <- 250000
pop_proportion <- 0.65
population <- create_population(N=pop_size, p=pop_proportion)

sample_size <- 1000
samples <- sample(population, size=sample_size)
phat <- sum(samples == "support") / sample_size

K <- 1000  # Simulation size
simulation <- replicate(K, sample_get_phat_fn(population, n=sample_size))
title <- sprintf("Histogram of p-hat values from experiment with %sx%s samples", 
                 K, sample_size)  # in case we change the sample/simulation size

ggplot(data=data.frame(simulation), aes(x=simulation)) +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  geom_histogram(bins=100, alpha=0.5, color=4, fill="white") +
  labs(title=title, x="Sample proportion", y="Frequency") +
  xlim(c(0,1))


# ---- 2. Normal approximation ----
# Q: Where do the parameters for the Normal distribution approximating p-hat 
# come from?

bw <- 0.01
n_obs <- length(simulation)
SE_phat <- sqrt((pop_proportion*(1-pop_proportion))/n_obs)

# Plot the histogram with the Normal density:
ggplot(data=as.data.frame(simulation), aes(x=simulation)) +
  geom_histogram(binwidth=bw, alpha=0.5, color=4, fill="white") +
  stat_function(fun = function(x) 
    dnorm(x, mean=pop_proportion, sd=SE_phat)*bw*n_obs) +
  xlab("Sample proportion") +
  ggtitle(title) +
  xlim(c(0,1))


# 3. ---- Repeat the experiment for different sample sizes ---- 
# Q: What happens to the distributions as sample size increases?

# Run Rshiny app
shinyApp(ui = ui, server = server)

