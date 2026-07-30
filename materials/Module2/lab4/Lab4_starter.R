############################################################
# MA 213 - Lab 4 Starter Code
# Dice Games, Expected Value, and Simulation
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the missing pieces first.
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
sample(1:6, size = 20, replace = TRUE,
       prob = c(1, 1, 1, 1, 1, 2))

# Discuss:
# - If the weighted die is rolled many times, should exactly twice as
#   many 6s appear as each other number?
# - What changes when the number of rolls gets larger?


############################################################
# Question 1. Is the one-die game fair?
############################################################

# Game A:
# Roll one die.
# If the roll is 1, win $6.
# If the roll is 2, 3, 4, 5, or 6, lose $1.

# Fill in the outcomes and probabilities, then uncomment.

# outcomes <- c(____, ____)
# probs <- c(____, ____)
#
# E_X <- sum(outcomes * probs)
# E_X

# Interpretation:
# - Is E_X positive, negative, or zero?
# - Based on expected value, should you play Game A?


############################################################
# Question 2. How risky is Game A?
############################################################

# Variance measures how much outcomes vary around the expected value.

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
# - What does the standard deviation tell you about Game A?
# - Is the expected value alone enough to describe this game?


############################################################
# Question 3. Can simulation verify the theory?
############################################################

# Write a function that plays Game A once.

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
#
# play_game_a()
# play_game_a()
# play_game_a()


# Simulate Game A many times.

# set.seed(123)
# n_games <- 100
#
# results_a <- replicate(n_games, play_game_a())
#
# mean(results_a)
# var(results_a)
# sd(results_a)

# Try changing n_games to 10, 100, 1000, and 10000.

# Interpretation:
# - Are the simulated values close to the theoretical values?
# - How large did n_games need to be before the mean felt stable?


############################################################
# Question 4. What does the law of large numbers look like?
############################################################

# The cumulative average should get closer to the expected value
# in the long run.

# set.seed(213)
# n_games <- 10000
# results_a <- replicate(n_games, play_game_a())
#
# cumulative_data_a <- data.frame(
#   game = 1:n_games,
#   average_profit = cumsum(results_a) / (1:n_games),
#   expected_value = E_X
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
# - What happens early in the graph?
# - What happens later in the graph?


############################################################
# Question 5. What changes in a two-dice game?
############################################################

# Game B:
# Roll two independent dice.
# First die: if it is 1, win $6; otherwise lose $1.
# Second die: if it is 1, win $3; otherwise lose $3.
# Total winnings are Y = X1 + 2X2.

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

# Total Y = X1 + 2X2.
# E_Y <- ____
# Var_Y <- ____
# SD_Y <- sqrt(Var_Y)
#
# E_Y
# Var_Y
# SD_Y

# Interpretation:
# - Does Game B have a higher or lower expected value than Game A?
# - Does Game B have more or less variability than Game A?


############################################################
# Question 6. Simulate Game B
############################################################

# play_game_b <- function() {
#   die1 <- sample(1:6, size = 1)
#   die2 <- sample(1:6, size = 1)
#
#   if (die1 == 1) {
#     x1 <- ____
#   } else {
#     x1 <- ____
#   }
#
#   if (die2 == 1) {
#     x2 <- ____
#   } else {
#     x2 <- ____
#   }
#
#   total <- ____
#   return(total)
# }
#
# set.seed(456)
# n_games <- 10000
# results_b <- replicate(n_games, play_game_b())
#
# mean(results_b)
# var(results_b)
# sd(results_b)

# Final claim:
# - Which game is the better choice?
# - What theoretical and simulated evidence supports your answer?


############################################################
# Optional Challenge. Design your own game
############################################################

# Your game must have at least two possible outcomes and at least two
# different payoffs.

# my_game <- function() {
#   # Your code here.
# }

# 1. Define the game rules.
# 2. Calculate theoretical E(X) and Var(X).
# 3. Simulate 10000 games.
# 4. Compare theoretical and simulated values.
# 5. Decide whether you would play your own game.
