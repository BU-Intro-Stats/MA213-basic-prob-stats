############################################################
# MA 213 - Lab 3 Starter Code
# From Data to Visual Evidence
#
# This file is intentionally a starter, not a solution key. Some lines
# are commented out because you need to fill in the missing pieces first.
#
# Reminders: 
# You can comment/uncomment lines in RStudio with
# Ctrl+Shift+C (Windows) or Cmd+Shift+C (Mac).
#
# To run a line (or selected lines) of code, put your cursor on the
# line (or select multiple lines) and press Ctrl+Enter (Windows) or
# Cmd+Enter (Mac).
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

# No additional code is required here, but use the handout to discuss/write:
# - the observational unit for each data set
# - the numerical variables
# - the categorical variables
# - what patterns or relationships you might expect to find in each data set
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

# What does the na.rm = TRUE argument do in the mean() function? Why is it important here?

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
# Discuss with your partner what each part of the code is doing.

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
# - What does the filter() line do here? What would happen to the counts
#   in ozone_wind_table if you removed it?
# - ifelse() takes a condition, a value if TRUE, and a value if FALSE.
#   In words, what rule does ifelse() use to assign ozone_level?
# - case_when() checks a list of conditions in order and uses the value
#   tied to the first one that is TRUE. In words, what rule does
#   case_when() use to assign wind_level? Why does case_when() make more
#   sense than ifelse() for this variable?
# - The original Wind variable is numerical. After you create wind_level,
#   should it be treated as numerical or categorical? Make your case.


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
# Optional Challenge. Did survival differ by sex or age?
############################################################

# Using the same approach as above, build a count table and
# dodged/filled bar plots for Sex and Age.

# Interpretation:
# - Which sex had the highest survival count? Which had the highest survival rate?
# - Which age group had the highest survival rate? Was the difference between children and adults large or small?
# - Does the pattern for Sex resemble the pattern you saw for Class? What about Age?


############################################################
# Extra Challenge. Is there an interaction between sex and class?
############################################################

# The effect of class on survival might not be the same for both sexes.
# A mosaic plot can show three categorical variables at once: column width
# reflects the number of passengers in each group, and the height of the
# colored blocks within a column reflects the proportion who survived.

# sex_class_survival_table <- table(titanic$Sex, titanic$Class, titanic$Survived)
# mosaicplot(sex_class_survival_table, color=TRUE)

# Interpretation:
# - In which Class-by-Sex combination is the survival rate highest? Lowest?
# - Does the class pattern look the same for men as it does for women?
# - Does the relationship between class and survival appear to depend on sex?
