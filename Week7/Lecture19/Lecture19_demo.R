
# ---- 0. Setup and load libraries, if any ----

library(ggplot2)
set.seed(213)

# In this demonstration, we will recreate the analysis done in 2015 by James Scott
# on bookdown.org for a series titled Data Science in R: A Gentle Introduction
# (https://bookdown.org/jgscott/DSGI/p-values.html).
#
# This example examines the chances (via a hypothesis test and p-values) that 
# the Patriots, notorious for cheating as well as for winning often, cheated
# during the 2014-2015 seasons specifically during the pre-game coin flips,
# which determine which team starts a game with the ball. 

# Our data: during 25 games in 2014-2015, the Patriots won 19 coin flips, for a
# p-hat of 76%. How would you set up the hypotheses for this test?


# ---- 1. Computing p-values using the standard normal distribution ----

# How would you compute the test statistic?
SE <- sqrt((0.5*0.5)/25)
Z <- (0.76-0.5)/SE  

# The sample proportion is 2.6 standard deviations away from the hypothesized
# value (fair coin, p=0.5). Is this statistically significant?
# How would you calculate the p-value?

pval <- 1-pnorm(Z)  # 0.0047 - smaller than, for example, an alpha=0.05

# What do you conclude from this p-value?


# ---- 2. Simulation ----

# First, let's see what happens when we flip 25 coins:
rbinom(n=1, size=25, prob=0.5)  # far from 19 successes

# Now simulate the experiment (25 coin flips) many times:
data <- data.frame(rbinom(n=10000, size=25, prob=0.5))
colnames(data) <- c('successes')
head(data)

# Graph the outcomes:
ggplot(data) + 
  geom_histogram(aes(x=successes), binwidth=1)

# How many times were there at least 19/25 successes?
sum(data >= 19)  # 77 / 10000, or 0.77% -- very unlikely!

# Graph the likelihood:

data$group = ifelse(data$successes < 19, 'less than 19', 'at least 19')
ggplot(data, aes(x=successes, fill=group)) + 
  geom_histogram(aes(x=successes), binwidth=1) +
  geom_vline(xintercept=18.5)  # intercept=18.5 because the binwidth is 0.5


# We still find that achieving at least 19 successes out of 25 coin flips is
# very unlikely. But even if we can reject H0, do you think we can conclude that 
# the Patriots cheated on the coin flips in their 25-game streak? 
