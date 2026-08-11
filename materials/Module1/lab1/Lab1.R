# Starter R file for Lab 1
# Keep this file in the same folder as sleep.csv and burger.csv.

# Part 2: Is R working?
R.version.string

# Part 3: Your first data import
getwd()

sleep_data <- read.csv("sleep.csv")
head(sleep_data)

sum(sleep_data$extra)

# Optional Challenge
burger_data <- read.csv("burger.csv")
table(burger_data$best_burger_place)
