# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

set.seed(42)

# ---- 1. Load data ----
data(airquality)

head(airquality)

# Let's deliberately add an incorrect entry
new_row <- data.frame(
  Ozone = 50,      # example value
  Solar.R = 200,   # example value
  Wind = -5,       # outlier: negative wind speed
  Temp = 80,       # example value
  Month = 6,       # example value
  Day = 15         # example value
)
airquality2 <- rbind(airquality, new_row) # add the row to the dataset
tail(airquality2)

# ---- 2. Visualize data

ggplot(data=airquality2, aes(x=Wind, y=Ozone)) +
  geom_point()

ggplot(data=airquality2, aes(x = Wind)) +
  geom_histogram()

# ---- 2. Boxplots ----

ggplot(data=airquality2, aes(y=Wind)) +
  geom_boxplot(outlier.color = "orange")

# Note: This is the code we used last time to plot the IQR:
ggplot(data=airquality2, aes(x = Wind)) +
  geom_histogram() +
  geom_boxplot(aes(x = Wind),  # Place boxplot below x-axis
               width = 1, 
               outlier.shape = NA,
               coef = 0,
               position = position_nudge(y = -2)
  ) 
guides(fill = "none")


