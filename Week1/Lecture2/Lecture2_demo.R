
# ---- 0. Set up and load libraries, if any ----

library(ggplot2)  # We will use this to plot today, and throughout the course

# Set the working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))
# (you probably won't need to use this yourselves)


# ---- 1. Load data ----

# Load the .csv data file into an object called a dataframe:
survey_data <- read.csv("lecture_1_survey_data_clean.csv")

# Confirm that the object is indeed a dataframe:
print(is.data.frame(survey_data))

# Display the first row:
head(survey_data) 


# ---- 32 Plotting ----

# Now we get to ask interesting questions about the data, and try to answer them
# using plots. For example, how many people in the class go to bed within each 
# time range? How many Capricorns are in the class? Is there a relationship
# between hours of sleep and caffeinated drinks? How does bedtime factor in?

# Bar plot of bedtimes:
ggplot(data=survey_data, aes(x=bedtime)) +
  geom_bar() +
  xlab("Bedtime options")

# Bar plot of Zodiac signs:
ggplot(data=survey_data, aes(x=zodiac)) +
  geom_bar() +
  xlab("Zodiac sign")

# Scatter plot of hours of sleep vs caffeinated drinks:
ggplot(data=survey_data, aes(x=caffeine, y=sleep)) +
  geom_point() +
  xlab("Number of caffeinated drinks") +
  ylab("Hours of sleep")

# Scatter plot of hours of sleep vs caffeinated drinks, colored by bedtimes
ggplot(data=survey_data, aes(x=caffeine, y=sleep, color=bedtime)) +
  geom_point() +
  xlab("Number of caffeinated drinks") +
  ylab("Hours of sleep") +
  labs(color="Bedtime")

# What other questions can you think of asking? How would you make a plot to
# try to answer them?