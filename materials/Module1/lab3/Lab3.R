############################################################
# MA 213 - Lab 3 Starter Code
# From Data to Visual Evidence
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the missing pieces first.
############################################################

library(dplyr)
library(ggplot2)
library(tidyr)


############################################################
# Getting Started. Open the Evidence Files
############################################################

air <- read.csv("airquality.csv", row.names = 1)
titanic <- read.csv("Titanic.csv", row.names = 1)

head(air)
head(titanic)

str(air)
str(titanic)

# Discuss:
# - What is one row in air?
# - What is one row in titanic?
# - Which variables are numerical? Which variables are categorical?


############################################################
# Question 1. What kind of data are we looking at?
############################################################

# Use head(), str(), and names() to help classify the variables.

names(air)
names(titanic)

# No additional code is required here, but use the handout to write:
# - the observational unit for each data set
# - the numerical variables
# - the categorical variables
# - one possible research question for each data set


############################################################
# Question 2. Does solar radiation vary by month?
############################################################

# Compare average Solar.R across months.
# Fill in the blanks, then uncomment.

# solar_by_month <- air %>%
#   group_by(________) %>%
#   summarize(solar_r_avg = mean(________, na.rm = TRUE))
#
# solar_by_month

# Make a bar plot from the summary table.

# ggplot(solar_by_month, aes(x = factor(________), y = solar_r_avg)) +
#   geom_col() +
#   labs(
#     x = "Month",
#     y = "Average solar radiation",
#     title = "Average solar radiation by month"
#   )

# Interpretation:
# - Which month has the highest average solar radiation?
# - Was your prediction correct?


############################################################
# Question 3. How are temperature and solar radiation related?
############################################################

# First remove rows where Solar.R or Temp is missing.

air_clean <- air %>%
  filter(!is.na(Solar.R), !is.na(Temp))

# Create a scatter plot comparing Temp and Solar.R.

ggplot(air_clean, aes(x = Temp, y = Solar.R)) +
  geom_point() +
  labs(
    x = "Temperature (F)",
    y = "Solar radiation",
    title = "Solar radiation and temperature"
  )

# Interpretation:
# - Does the plot suggest a positive association, negative association,
#   or no clear association?
# - Are there any points that seem unusual?


############################################################
# Question 4. Can we categorize air quality conditions?
############################################################

# Create categorical versions of Ozone and Wind.
# This block is complete, but read it carefully before running it.

air_categories <- air %>%
  filter(!is.na(Ozone), !is.na(Wind)) %>%
  mutate(
    ozone_level = ifelse(Ozone > median(Ozone), "High", "Low"),
    wind_level = case_when(
      Wind < 1 ~ "Calm",
      Wind < 4 ~ "Light air",
      Wind < 7 ~ "Light breeze",
      Wind < 12 ~ "Gentle breeze",
      Wind < 18 ~ "Moderate breeze",
      Wind < 24 ~ "Fresh breeze",
      TRUE ~ "Strong breeze"
    )
  )

ozone_wind_table <- air_categories %>%
  count(wind_level, ozone_level)

ozone_wind_table

# Interpretation:
# - Is ozone_level numerical or categorical?
# - Is wind_level numerical or categorical?
# - How did mutate() change the data set?


############################################################
# Question 5. Did Titanic survival differ by class?
############################################################

# Build a count table for Class and Survived.
# Fill in the blanks, then uncomment.

# class_survival_table <- titanic %>%
#   count(________, ________)
#
# class_survival_table

# Make a dodged bar plot showing survival counts by class.

# ggplot(class_survival_table, aes(x = Class, y = n, fill = Survived)) +
#   geom_col(position = "dodge") +
#   labs(
#     x = "Passenger class",
#     y = "Number of passengers",
#     title = "Titanic survival counts by class"
#   )

# Interpretation:
# - Which class had the highest survival count?
# - What pattern do you see between passenger class and survival?


############################################################
# Question 6. Counts or proportions?
############################################################

# Counts can hide patterns when groups have different sizes.
# Use a filled bar chart to compare proportions within class.

# ggplot(class_survival_table, aes(x = Class, y = n, fill = Survived)) +
#   geom_col(position = "fill") +
#   labs(
#     x = "Passenger class",
#     y = "Proportion within class",
#     title = "Titanic survival proportions by class"
#   )

# Final claim:
# - Do passenger class and survival appear related?
# - What evidence from your table or plot supports your answer?


############################################################
# Optional Challenge. Survival by Sex and Age
############################################################

# sex_age_table <- titanic %>%
#   count(Sex, Age)
#
# sex_age_table

# Count the number of male children in the data.

# child_male <- titanic %>%
#   filter(Sex == "Male", Age == "Child") %>%
#   summarize(total = n())
#
# child_male
