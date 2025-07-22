# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

# Today we will explore the Law of Large Numbers through repeated coin tosses.

# ---- 1. Toss a single coin ----

outcomes <- c(0,1)  # sample space: 0 == Heads, 1 == Tails
probabilities <- c(0.5, 0.5)  # let's say we have a fair coin
toss <- sample(outcomes, 1, replace=TRUE, prob=probabilities)  # make the toss

print(toss)


# ---- 2. Toss many more coins and observe the sample means ----

tossCoin <- function(p=0.5, max_n=100) {
  
  # Now repeat the toss n times:
  outcomes <- c(0,1)  # same sample space: 0 == Heads, 1 == Tails
  probabilities <- c(p, 1-p)  # now we can modify the probability
  
  # Create a vector to store our mean values
  means <- rep(0, max_n)
  sizes <- c(1:max_n)
  
  for(n in sizes) {
    toss <- sample(outcomes, n, replace=TRUE, prob=probabilities)  # toss
    means[n] <- mean(toss)  # store mean value
  }
  
  # Combine everything into a dataframe
  return(data.frame(sizes, means))
}


# ---- 3. Plot the results ----

trial <- tossCoin(p=0.5, max_n=1000)
last_mean <- tail(trial$means, 1)

ggplot(data=trial, aes(x=sizes, y=means)) +
  geom_line(color='dark blue') +
  geom_hline(yintercept=0.5, color='red') +
  labs(x = "n (number of tosses)", 
       y = "Sample mean") +
  annotate("text", x=500, y=.8,
           label=paste("P(Heads) = 0.50")) +
  annotate("text", x=500, y=.65,
           label=paste("Final sample mean =", last_mean))

# What do you observe?
# What do you think would happen for different values of p?
