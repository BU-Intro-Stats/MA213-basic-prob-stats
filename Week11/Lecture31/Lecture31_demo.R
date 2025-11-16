# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# ---- 1. Scatterplot for the poverty dataset
# Load data
poverty <- read.table("poverty.txt", header = T, sep = "\t")

p1 = ggplot(poverty, aes(x = Graduates, y = Poverty)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot of % in Poverty vs. % HS Graduates",
       x = "% HS Graduates",
       y = "% in Poverty")
print(p1)

# ---- 2. Plot line 64.78 - 0.62*Graduates
p2 = p1 + geom_abline(slope = -0.62, intercept = 64.78, color = "red", linewidth = 1) +
  labs(title = "Scatterplot with Fitted Line")
print(p2)


# ---- 3. Compute residuals
poverty$predicted <- 64.78 - 0.62 * poverty$Graduates
poverty$residuals <- poverty$Poverty - poverty$predicted


# ---- 4. Compute correlation
correlation <- cor(poverty$Graduates, poverty$Poverty)
cat("Correlation between % HS Graduates and % in Poverty:", correlation, "\n")

correlation <- cor(poverty$PercentFemaleHouseholderNoHusbandPresent, poverty$Poverty)
cat("Correlation between % no husband and % in Poverty:", correlation, "\n")

#---------------------------------------------------------------------------
# ---- 5. Residuals plot
p3 = ggplot(poverty, aes(x = Graduates, y = residuals)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. % HS Graduates",
       x = "% HS Graduates",
       y = "Residuals")
print(p3)

#---------------------------------------------------------------------------
# ---- 6. Histogram of residuals
p4 = ggplot(poverty, aes(x = residuals)) +
  geom_histogram(binwidth = 1, boundary=0, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Count")
print(p4)

# -------------------------------------------------------
# ---- 7. Slope and intercept of the least squares line

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
  geom_abline(slope = b1, intercept = b0, color = "red", linewidth = 1) +
  labs(title = "Least Squares Regression Line",
       x = "% HS Graduates",
       y = "% in Poverty")
print(p4) 

