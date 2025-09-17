#Lab 3
# Do not change the code here
############################################################################################################
required_pkg <- c("dplyr", "tidyr", "ggplot2")

new_pkgs <- required_pkg[!(required_pkg %in% installed.packages()[, "Package"])]

if(length(new_pkgs)) {
  install.packages(new_pkgs)
}

invisible(lapply(required_pkg, library, character.only = TRUE))



############################################################################################################


# Write your code below here.
# Do not setup your working directory here. 


############################################################################################################



# Q1. load the somedata.csv file and assign it to my_df object.




# Q2. Get box plot of A in my_df. Looking at the box plot, how many suspected outliers can you see? assign it to n_suspected . (For example, you can do n_suspected <- 1).




# Q3. Obtain the scatter plot of B and C (x-axis for B, y-axis for C). What kind of relationship can you see? You can choose from those two
# linear , nonlinear
# You can answer this by assigning a variable relationship. 
# (For example, you can do relationship <- "linear" or `relationship <- "nonlinear" ).





# Q4. You are interested in looking at this mysterious variable D in the data. Look at the distribution of the data by producing a plot, answer if the shape of D is one of
# right skewed , left skewed , uniform , symmetric
# You can answer this by assigning a variable shape. 
# (For example, you can do shape <- "right skewed" or `shape <- "uniform" ... )




# Q5. Obtain a barplot of E in the data. 
# If you create a barplot showing counts on Y axis and categories (from A to E) on X axis, what is the pattern of this barplot? 
# You can answer if it is one of
# increasing , decreasing , flat
# You can answer this by assigning a variable pattern . 
# (For example, you can do pattern <- "increasing" or `pattern <- "decreasing" ... )







