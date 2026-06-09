# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# ---- 1. Reminder: least squares line for the poverty dataset
# Load data
poverty <- read.table("poverty.txt", header = T, sep = "\t")

# b1=sy/sx * R
sx <- sd(poverty$Graduates)
sy <- sd(poverty$Poverty)
R <- cor(poverty$Graduates, poverty$Poverty)
b1 <- R * (sy / sx)

# b0= ybar - b1*xbar
xbar <- mean(poverty$Graduates)
ybar <- mean(poverty$Poverty)
b0 <- ybar - b1 * xbar

# plot line on scatterplot
p4 = ggplot(poverty, aes(x = Graduates, y = Poverty)) +
  geom_point(color = "blue") +
  geom_abline(slope = b1, intercept = b0, color = "red", size = 1) +
  labs(title = "Least Squares Regression Line",
       x = "% HS Graduates",
       y = "% in Poverty")
print(p4) 

# ---- 2. Extrapolate the line to the range 60%-100% HS Graduates
p4 <- p4 +
  coord_cartesian(xlim = c(60, 100), ylim=c(0, 30))

print(p4)

# ---- 3. What does the model predict at 60%, 85%, or 100% HS Graduates?

pred60 = b0+b1*60
pred85 = b0+b1*85
pred100 = b0+b1*100

# Do you believe these predictions? why/why not?

pred200 = b0+b1*200
# Note that the model doesn't respect the limits of the problem
# % HS Graduates should be between 0 and 100
# % in Poverty should be between 0 and 100

# -----------------------------------------------------------------
# ---- 4. How strong is the relationship between % HS Graduates and % in Poverty?

Rsquared <- cor(poverty$Graduates,poverty$Poverty)^2

