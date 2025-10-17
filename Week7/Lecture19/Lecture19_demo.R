
# ---- 0. Setup and load libraries, if any ----

library(ggplot2)
set.seed(213)

# In this demonstration, we will recreate the analysis done in 2015 by James Scott
# on bookdown.org for a series titled Data Science in R: A Gentle Introduction
# (https://bookdown.org/jgscott/DSGI/p-values.html).
#
# This example examines the chances (via a hypothesis test and p-values) that 
# the Patriots, notorious for cheating as well as for winning often, cheated
# during the 2014-2015 regular seasons specifically during the opening kickoff.
# The referee flips a coin and teams call it, and the team to win the flip gets
# to kickoff the game with the ball (though the winning team can choose to 
# defer and allow the other team to kickoff instead).
# 
# Our data: during 25 regular games in 2014-2015, the Patriots won 19 
# consecutive kickoff flips, for a p-hat of 76%. 
# How would you set up the hypotheses for this test?


# ---- 1. Computing p-values using the standard normal distribution ----

# H0: p=0.5
# HA: p>0.5 (one-sided)
# Test statistic: z=(phat-0.5)/SE
# Alpha: 0.05

# Observed phat: 0.76

# How would you compute the test statistic?
p0 <- 0.5 # the value of p under the Null hypothesis
N <- 25
SE <- sqrt((p0*(1-p0))/N)
Z <- (0.76-p0)/SE  

# The sample proportion is 2.6 standard deviations away from the hypothesized
# value (fair coin, p=0.5). Is this statistically significant?
# How would you calculate the p-value?

pval <- 1-pnorm(Z)  # one-sided hypothesis test, so we want the upper tail probability
print(pval) # 0.0047 - smaller than, for example, an alpha=0.05

# What do you conclude from this p-value?


# ---- 2. Simulation ----

# First, let's see what happens when we flip 25 coins:
rbinom(n=1, size=N, prob=p0)  # far from 19 successes

# Now simulate the experiment (25 coin flips) many times:
data <- data.frame(rbinom(n=10000, size=N, prob=p0))
colnames(data) <- c('successes')
head(data)

# How many times were there at least 19/25 successes?
sum(data >= 19)  # 77 / 10000, or 0.77% -- very unlikely!

# Graph the likelihood:
data$group = ifelse(data$successes < 19, 'less than 19', 'at least 19')
ggplot(data, aes(x=successes, fill=group)) + 
  geom_histogram(aes(x=successes), binwidth=1) +
  geom_vline(xintercept=18.5)  # intercept=18.5 because of the binwidth


# We still find that achieving at least 19 successes out of 25 coin flips is
# very unlikely. But even if we can reject H0, do you think we can conclude that 
# the Patriots cheated on the coin flips in their 25-game streak? 


# Calculate z-scores for each simulated outcome
data$zscore <- (data$successes/N - p0) / SE

# Plot histogram of z-scores
ggplot(data, aes(x = zscore)) +
  geom_histogram(aes(y = ..density..), binwidth = 1/N/SE, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, color = "black", size = 1) +
  geom_area(
    stat = "function",
    fun = dnorm,
    xlim = c((18.5/N - p0) / SE, 3.5),
    fill = "orange",
    alpha = 0.8
  ) +
  labs(
    title = "Histogram of Simulated Z-scores with Standard Normal Overlay",
    x = "Z-score",
    y = "Density"
  )