
# ---- 0. Set up and load libraries, if any ----

library(ggplot2)  # We will use this to plot today, and throughout the course

# Set the working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))
# (you probably won't need to use this yourselves)


# ---- 1. Load data ----

# Load the .csv data file into an object called a dataframe:
survey_data <- read.csv("lecture_1_classroom_survey.csv")

# Confirm that the object is indeed a dataframe:
print(is.data.frame(survey_data))

# Display the first row:
head(survey_data) 

# There are lots of columns we don't need, so let's clean things up


# ---- 2. Process data (lightly) ----

# Print all the column names:
print(colnames(survey_data))

# We want to drop all but the last 7 columns (of 19), since the last 7 columns
# contain the survey responses we're actually interested in.

# We can take a subset of the columns in the dataframe, using a range of
# numbers (inclusive). The '-' indicates an inversion, which means we're dropping 
# the columns listed in the range. 
# Then we assign the object to a new dataframe:
survey_data_clean <- subset(survey_data, select = -c(1:11))

# View the new dataframe:
head(survey_data_clean)

# ---- 3. Plotting ----

# Now we get to ask interesting questions about the data, and try to answer them
# using plots. For example, how many people in the class go to bed within each 
# time range? How many Capricorns are in the class? Is there a relationship
# between hours of sleep and caffeinated drinks? How does bedtime factor in?

# Bar plot of bedtimes:
ggplot(data=survey_data_clean, aes(x=Item.2.Answer)) +
  geom_bar() +
  xlab("Bedtime options")

# Bar plot of Zodiac signs:
ggplot(data=survey_data_clean, aes(x=Item.6.Answer)) +
  geom_bar() +
  xlab("Zodiac sign")

# Scatter plot of hours of sleep vs caffeinated drinks:
ggplot(data=survey_data_clean, aes(x=Item.4.Answer, y=Item.1.Answer)) +
  geom_point() +
  xlab("Number of caffeinated drinks") +
  ylab("Hours of sleep")

# Scatter plot of hours of sleep vs caffeinated drinks, colored by bedtimes
ggplot(data=survey_data_clean, aes(x=Item.4.Answer, y=Item.1.Answer, color=Item.2.Answer)) +
  geom_point() +
  xlab("Number of caffeinated drinks") +
  ylab("Hours of sleep") +
  labs(color="Bedtime")

# What other questions can you think of asking? How would you make a plot to
# try to answer them?