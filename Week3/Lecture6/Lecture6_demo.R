# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

# Today we will explore the Law of Large Numbers through repeated coin tosses.

# ---- 1. Toss a single coin ----

outcomes <- c(0,1)  # sample space: 0 == Tails, 1 == Heads
probabilities <- c(0.5, 0.5)  # let's say we have a fair coin
toss <- sample(outcomes, 1, replace=TRUE, prob=probabilities)  # make the toss

print(toss)


# ---- 2. Toss many more coins and observe the proportion of heads ----

tossCoins <- function(p=0.5, n=100) {
  
  outcomes <- c(0,1)  # same sample space: 0 == Tails, 1 == Heads
  probabilities <- c(1-p, p)  # now we can modify the probability
  
  # Create vectors to store our tosses and our proportions
  tosses <- rep(0,n)
  proportions <- rep(0, n)
  counts <- rep(0,n)
  
  # loop n times
  for(i in 1:n) {
    # toss the coin
    tosses[i] <- sample(outcomes, 1, replace=TRUE, prob=probabilities)
    
    # compute the proportion of heads so far
    proportions[i] <- mean(tosses[1:i])
    
    # store the number of tosses you've done so far
    counts[i] <- i
  }
  
  # Combine everything into a dataframe
  return(data.frame(tosses, proportions, counts))
}


# ---- 3. Simulate 100 coin tosses and plot the results ----

Ntoss = 100
prob = 1/2

trial <- tossCoins(p=prob, n=Ntoss)
last_proportion <- tail(trial$proportions, 1)

ggplot(data=trial, aes(x=counts, y=proportions)) +
  geom_line(color='dark blue') +
  geom_hline(yintercept=prob, color='red') +
  labs(x = "Number of tosses", 
       y = "Proportion of heads") +
  annotate("text", x=Ntoss*.75, y=.8,
           label=paste("P(Heads) = 0.50")) +
  annotate("text", x=Ntoss*.75, y=.7,
           label=paste("Final sample mean =", last_proportion))

# What do you observe?
# What do you think would happen for different values of p?
