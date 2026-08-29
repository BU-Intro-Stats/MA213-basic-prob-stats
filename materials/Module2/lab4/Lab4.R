############################################################
# MA 213 - Lab 4 Starter Code
# Dice Games, Expected Value, and Simulation
#
# This file is a starter, not a solution key.
#
# How to read this file:
#   - Code that is NOT commented out is ready to run. Run it.
#   - Code that IS commented out contains blanks marked ____ .
#     Fill in the blanks, remove the # marks, then run it.
#     (In RStudio, select the block and press Cmd/Ctrl + Shift + C.)
#
# The order here matches the worksheet: we simulate first and
# bring in the formulas afterwards.
############################################################

library(dplyr)
library(ggplot2)


############################################################
# Getting Started. Randomness in R
############################################################

# Roll one fair die once.
sample(1:6, size = 1)

# Roll one fair die 20 times.
sample(1:6, size = 20, replace = TRUE)

# Roll a weighted die where 6 is twice as likely as the other values.
# True probabilities: 1/7 for each of 1-5, and 2/7 for 6.
sample(1:6, size = 20, replace = TRUE,
       prob = c(1, 1, 1, 1, 1, 2))

# PREDICT FIRST: with many rolls, will you see exactly twice as many
# 6s as 1s? Write your prediction down, then run the code below.

rolls_small <- sample(1:6, size = 20, replace = TRUE,
                      prob = c(1, 1, 1, 1, 1, 2))
table(rolls_small) / 20

rolls_big <- sample(1:6, size = 10000, replace = TRUE,
                    prob = c(1, 1, 1, 1, 1, 2))
table(rolls_big) / 10000

# True probabilities for comparison.
c(1, 1, 1, 1, 1, 2) / 7

# Do the observed proportions add up to something familiar?
sum(table(rolls_small) / 20)
sum(table(rolls_big) / 10000)

# Interpretation:
# - Which set of proportions is closer to the true probabilities?
# - What is the sum in each case, and why does that happen at any
#   number of rolls?


############################################################
# Question 1. Play the game before you analyze it
############################################################

# Game A:
# Roll one die.
# If the roll is 1, win $6.
# If the roll is 2, 3, 4, 5, or 6, lose $1.

# PREDICT FIRST: would you play this game?

# Fill in the blanks, then uncomment.

# play_game_a <- function() {
#   roll <- sample(1:6, size = 1)
#
#   if (roll == ____) {
#     winnings <- ____
#   } else {
#     winnings <- ____
#   }
#
#   return(winnings)
# }

# Play ten rounds by hand and record them on the worksheet.
# Run this line ten times.

# play_game_a()

# NOTE: do NOT set a seed in this question. Your numbers should differ
# from your partner's -- that difference is the point.

# Now let replicate() do the repeating.

# results_10    <- replicate(10, play_game_a())
# results_100   <- replicate(100, play_game_a())
# results_1000  <- replicate(1000, play_game_a())
# results_10000 <- replicate(10000, play_game_a())
#
# mean(results_10)
# mean(results_100)
# mean(results_1000)
# mean(results_10000)

# Interpretation:
# - Compare your table with your partner's. At which row do you agree?
# - What single number does the average seem to settle toward?


############################################################
# Question 2. Where did that number come from?
############################################################

# The number you found has a name: the expected value, E(X).
# The formula is a shortcut for the long-run average:
#   E(X) = sum of x_i * P(x_i)

# outcomes <- c(____, ____)
# probs <- c(____, ____)
#
# # Check that this is a valid distribution before using it.
# sum(probs)
#
# E_X <- sum(outcomes * probs)
# E_X

# Interpretation:
# - How close is E_X to what your 10000-game simulation produced?


############################################################
# Question 3. What does the law of large numbers look like?
############################################################

# From here on we use set.seed(). A seed fixes which random results
# you get, so your submitted work is reproducible. It does not make
# the results any less random.

## ---- 3a. One long run ------------------------------------

# set.seed(213)
# n_games <- 10000
# results_a <- replicate(n_games, play_game_a())
#
# cumulative_data_a <- data.frame(
#   game = 1:n_games,
#   average_profit = cumsum(results_a) / (1:n_games)
# )
#
# ggplot(cumulative_data_a, aes(x = game, y = average_profit)) +
#   geom_line(color = "steelblue") +
#   geom_hline(yintercept = E_X, color = "red", linetype = "dashed") +
#   labs(
#     x = "Number of games played",
#     y = "Cumulative average profit",
#     title = "Game A: cumulative average profit",
#     subtitle = "Red dashed line shows the theoretical expected value"
#   )

# Interpretation:
# - What happens early in the graph? What happens later?


## ---- 3b. A simulated average is itself random -------------

# You and your partner got different answers from 100 games, so the
# average of 100 games is a random quantity. Look at its distribution
# by running the whole 100-game experiment 500 separate times.

# set.seed(2024)
# means_100 <- replicate(500, mean(replicate(100, play_game_a())))
#
# mean(means_100)
# sd(means_100)
#
# ggplot(data.frame(m = means_100), aes(x = m)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   geom_vline(xintercept = E_X, color = "red", linetype = "dashed") +
#   labs(
#     x = "Average winnings from 100 games",
#     y = "Count",
#     title = "500 repetitions of a 100-game experiment"
#   )

# Now use 1000 games per repetition instead of 100.

# set.seed(2025)
# means_1000 <- replicate(500, mean(replicate(1000, play_game_a())))
#
# mean(means_1000)
# sd(means_1000)

# Interpretation:
# - Which number changed a lot: the mean of the averages, or their SD?
# - What does that say about WHERE the average lands versus HOW MUCH
#   it bounces around?


############################################################
# Question 4. How risky is Game A?
############################################################

# Look at the spread in the results you already have.

# table(results_a)
# var(results_a)
# sd(results_a)

# That spread has a formula too:
#   Var(X) = sum of (x_i - mu)^2 * P(x_i) = E(X^2) - [E(X)]^2

# variance_X <- sum((outcomes - E_X)^2 * probs)
# sd_X <- sqrt(variance_X)
#
# variance_X
# sd_X

# Check variance using E(X^2) - [E(X)]^2.

# E_X_squared <- sum(outcomes^2 * probs)
# variance_X_check <- E_X_squared - E_X^2
# variance_X_check

# Interpretation:
# - What does the standard deviation say about the size of the swings?
# - Is the expected value alone enough to decide whether to play?


############################################################
# Question 5. Probability rules, discovered by simulation
############################################################

# KEY IDEA: mean() of a TRUE/FALSE vector is the proportion of TRUEs,
# which estimates the probability of that event. R treats TRUE as 1
# and FALSE as 0. You will use this same trick for p-values later.

set.seed(99)
n <- 10000
rolls <- data.frame(
  d1 = sample(1:6, size = n, replace = TRUE),
  d2 = sample(1:6, size = n, replace = TRUE)
)

A <- rolls$d1 == 1                 # event A: the first die is a 1
B <- (rolls$d1 + rolls$d2) >= 7    # event B: the total is at least 7

head(A)         # look at what these objects actually are

mean(A)         # estimate of P(A)
mean(B)         # estimate of P(B)
mean(A & B)     # estimate of P(A and B)
mean(A | B)     # estimate of P(A or B)
mean(B[A])      # estimate of P(B given A)

# Sketch the Venn diagram on the worksheet before moving on.


## ---- 5a. The addition rule -------------------------------

mean(A | B)
mean(A) + mean(B) - mean(A & B)

# Interpretation:
# - Do these agree? Why must we subtract P(A and B)?


## ---- 5b. Does multiplying always work? -------------------

# PREDICT FIRST: will P(A and B) = P(A) * P(B) hold for A and B above?

mean(A & B)             # what the simulation says
mean(A) * mean(B)       # what the independence rule would predict

# Now a different pair of events.
C <- rolls$d1 == 1      # first die is a 1
D <- rolls$d2 == 1      # second die is a 1

mean(C & D)
mean(C) * mean(D)

# Compare the unconditional and conditional chances of B.
mean(B)
mean(B[A])

# Interpretation:
# - Which pair obeyed the multiplication rule, and which did not?
# - Why does knowing d1 == 1 change the chance the total is >= 7,
#   but not the chance that d2 == 1?


## ---- 5c. Are all outcomes equally likely? ----------------

total <- rolls$d1 + rolls$d2
table(total) / n
sum(table(total) / n)

# Interpretation:
# - Each die is fair, but are all 11 totals equally likely?
# - Which total appears most often, and why?


############################################################
# Question 6. What changes in a two-dice game?
############################################################

# Game B:
# Roll two independent dice.
# First die:  if it is 1, win $6; otherwise lose $1.   -> X1
# Second die: if it is 1, win $3; otherwise lose $3.   -> X2
# Total winnings are Y = X1 + 2 * X2.

# PREDICT FIRST: higher or lower expected value than Game A?
#                More or less variability?

# No template this time. You have already written play_game_a(),
# so use the same ideas and decide the structure yourself.

# play_game_b <- function() {
#
#   # Your code here. Roll two dice, work out x1 and x2,
#   # and return the total winnings Y = x1 + 2 * x2.
#
# }
#
# # Test it before trusting it.
# play_game_b()
# play_game_b()
#
# set.seed(456)
# n_games <- 10000
# results_b <- replicate(n_games, play_game_b())
#
# mean(results_b)
# var(results_b)
# sd(results_b)


## ---- 6a. Settle the argument with data -------------------

# For independent random variables:
#   E(X1 + 2*X2) = E(X1) + 2*E(X2)
#
# DEBATE: what happens to the 2 in the variance?
#   Group 1 says Var(X1 + 2*X2) = Var(X1) + 2*Var(X2)
#   Group 2 says Var(X1 + 2*X2) = Var(X1) + 4*Var(X2)
# Do not look it up. Compute both and let var(results_b) decide.

# First die.
# outcomes_x1 <- c(6, -1)
# probs_x1 <- c(1/6, 5/6)
# E_X1 <- sum(outcomes_x1 * probs_x1)
# Var_X1 <- sum((outcomes_x1 - E_X1)^2 * probs_x1)

# Second die.
# outcomes_x2 <- c(____, ____)
# probs_x2 <- c(____, ____)
# E_X2 <- sum(outcomes_x2 * probs_x2)
# Var_X2 <- sum((outcomes_x2 - E_X2)^2 * probs_x2)

# E_Y <- E_X1 + 2 * E_X2

# The two competing claims.
# Var_Y_group1 <- Var_X1 + 2 * Var_X2
# Var_Y_group2 <- Var_X1 + 4 * Var_X2
#
# Var_Y_group1
# Var_Y_group2
# var(results_b)      # the referee

# Interpretation:
# - Which group was right? What number decided it?


## ---- 6b. Theory versus simulation for Game B -------------

# Use the winning formula to fill in the worksheet table:
#   E(Y), Var(Y), SD(Y) -- theoretical vs simulated vs difference.

# SD_Y <- sqrt(____)
# E_Y
# SD_Y

# Final claim:
# - Which game is the better choice?
# - What theoretical and simulated evidence supports your answer?


############################################################
# Question 7. What does independence actually buy you?
############################################################

# Everything in Question 6 assumed the dice were independent.
# Break that condition and watch what happens: here ONE roll drives
# both terms, so X1 and X2 are completely dependent.

play_game_b_same_die <- function() {
  die <- sample(1:6, size = 1)   # ONE roll used for both terms

  if (die == 1) {
    x1 <- 6
    x2 <- 3
  } else {
    x1 <- -1
    x2 <- -3
  }

  return(x1 + 2 * x2)
}

set.seed(789)
results_same <- replicate(10000, play_game_b_same_die())

mean(results_same)      # compare to mean(results_b)
var(results_same)       # compare to var(results_b)

# Interpretation:
# - One rule survived losing independence and one did not. Which?
# - Why does a single shared die make the swings bigger even though
#   the average payout did not move?


############################################################
# Optional Challenge. Design your own game
############################################################

# Your game must have at least two possible outcomes and at least two
# different payoffs.

# my_game <- function() {
#   # Your code here.
# }

# 1. Define the game rules as a function.
# 2. Simulate 10000 games and record the mean and variance.
# 3. Calculate theoretical E(X) and Var(X) from outcomes and probabilities.
# 4. Compare theoretical and simulated values in the worksheet table.
# 5. Decide whether you would play your own game.
