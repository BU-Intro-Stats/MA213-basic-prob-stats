# Starter R file for Lab 1

# Question 1
sleep_data <- read.csv("sleep.csv")
head(sleep_data)

# Question 2
dim(sleep_data)
names(sleep_data)
length(unique(sleep_data$ID))

# Question 3
str(sleep_data)


# Question 4 
burger_data <- read.csv("burger.csv")
table(burger_data)