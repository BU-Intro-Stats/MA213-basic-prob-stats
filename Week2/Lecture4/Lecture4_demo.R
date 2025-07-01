# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory

set.seed(42)

# ---- 1. Load and visualize data ----
data(airquality)

ggplot(data=airquality, aes(x=Wind, y=Ozone)) +
  geom_point()

# ---- 2. Boxplots ----

ggplot(data=airquality, aes(x=Wind, y=Ozone)) +
  geom_boxplot(outlier.color = "orange") +
  geom_point(color="light blue")
