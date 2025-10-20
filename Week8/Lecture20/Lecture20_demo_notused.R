
# ---- 0. Setup and load libraries, if any ----
library(ggplot2)

# Last time we calculated a p-value to determine whether the Patriots winning
# 19/25 coin flips at the start of their games in 2014-2015 could just be
# attributed to random chance, or if something else was going on. We rejected
# the null hypothesis (that the coin flip was truly fair), and by p-value
# concluded that the Patriots must have cheated. 
# 
# But we also couldn't explain how the Patriots could have feasibly done this.
#
# Today, we'll repeat the analysis but this time with data from the Patriots'
# 2013-2016 seasons. Let's see if our results change!
# 
# This time, the data look like this:
#
####### 2013: LLLLWLWWLWWLWWLL (9 losses, 7 wins; longest streak of 4 losses)
####### 2014: LWWWLWLWWWWWWWWW (3 losses, 13 wins; longest streak of 9 wins)
####### 2015: WWWWWWWWWWWWWWWW (0 losses, 16 wins; longest streak of 16 wins)
####### 2016: LLWLWLLWLWLLLLWL (11 losses, 5 wins; longest streak of 4 losses)
#
# data source: pro-football-reference.com

# ---- 1. Graphing the data ----

observed <- as.data.frame(
  c(rep(0, 4), 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0,   # 2013
    0, rep(1, 3), 0, 1, 0, rep(1, 9),                # 2014
    rep(1, 16),                                      # 2015   
    0, 0, 1, 0, 1, 0, 0, 1, 0, 1, rep(0,4), 1, 0))   # 2016

observed$year <- c(rep(2013, 16), rep(2014, 16), rep(2015, 16), rep(2016, 16))
observed$x <- seq(1, 64)
colnames(observed) <- c("outcome", "year", "x")

ggplot(observed, aes(x=x, y=outcome, color=as.factor(year))) +
  geom_line() +
  geom_point() +
  xlab("regular season kickoff flips (2013-2016)") +
  scale_y_continuous(breaks=c(0,1)) +
  scale_color_discrete(name="season")
  

# ---- 2. Effect of sample size on p-values and hypothesis tests ----

N <- 64  # 64 total regular-season games with kickoff flips
p_hat <- mean(observed$outcome)  # 0.64 - a bit more balanced this time!

# How would you compute the test statistic?

SE <- sqrt((0.5*0.5)/N)
Z <- (p_hat-0.5)/SE  

# The sample proportion is 2.24 standard deviations away from the hypothesized
# value (fair coin, p=0.5). Is this statistically significant?
# How would you calculate the p-value?

pval <- 1-pnorm(Z)  # 0.01 - still smaller than alpha=0.05, but by much less

# What do you conclude from this p-value?
