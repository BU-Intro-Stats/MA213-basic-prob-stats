# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Generate data ----

# Our dataset consists of 24 male and 24 female employees (48 total)
# Of these, 21 male employees were promoted (and 3 not promoted), while
# 14 female employees were promoted (and 10 not promoted):
#
#           Yes (promoted)    No (not promoted)   Total
# Male         21                   3              24
# Female       14                  10              24  
#

# We take 21+14 "yes" and 3+10 "no" as the total
data <- c(rep("yes", 35), rep("no", 13))
head(data)


# ---- 2. Setting the null and alternative hypotheses ----

# What are the null and alternative hypotheses in this study?
#
# Based on the data, 21/24=87.5% of male employees were promoted,
# while 14/24=58.33% of female employees were promoted, leading to a difference
# in proportions of 29.167%. 
#
# H0: The employees' gender and their promotion outcomes are independent.
# That is, they have no relationship and the observed difference between the 
# proportion of employees who were promoted in each group, 29.167%, is due to 
# chance.
#
# Ha: The employees' gender and their promotion outcomes are dependent. That is,
# the observed promotion outcomes are not due to chance and gender had an effect.


# ---- 3. Sampling once from the study population ----

# Randomly sample the male and female employees, independent of outcome
# (i.e. under the null hypothesis):
index <- sample(1:length(data), 24) 
Male <- data[index]
Female <- data[-index]

# Create a contingency table

group <- c(rep("Male", length(Male)), rep("Female", length(Female)))
outcome <- c(Male, Female)
df <- data.frame(Group = group, Outcome = outcome)
df_table <- table(df)
df_table

# Get the sample proportions and difference

ratio1 = df_table[3] / 24
ratio2 = df_table[4] / 24
ratio2-ratio1


# ---- 4. Repeat the sampling many times in a simulation ----

# First, create a function for sampling (like we did above):

simulation <- function() {
  index <- sample(1:48, 24)
  Male <- data[index]
  Female <- data[-index]
  
  group <- c(rep("Male", length(Male)), rep("Female", length(Female)))
  outcome <- c(Male, Female)
  
  df <- data.frame(Group = group, Outcome = outcome)
  df_table <- table(df)

  ratio1 = df_table[3] / 24
  ratio2 = df_table[4] / 24
  result <- ratio2-ratio1
  
  return(result)
}

# Now repeat many times:

Nsim <- 100
simulated_rates <- rep(0, Nsim)

for(i in 1:Nsim) {
  simulated_rates[i] <- simulation()
}

# Plot the results:

simulation_df <- data.frame(DifferenceProportion=simulated_rates)
ggplot(data=simulation_df, aes(x=DifferenceProportion)) +
  geom_dotplot()

# What can we conclude?
