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
p2 = p1 + geom_abline(slope = -0.62, intercept = 64.78, color = "red", size = 1) +
  labs(title = "Scatterplot with Fitted Line")
print(p2)


# ---- 3. Compute residuals
poverty$predicted <- 64.78 - 0.62 * poverty$Graduates
poverty$residuals <- poverty$Poverty - poverty$predicted

# Plot Graduates vs. residuals
p3 = ggplot(poverty, aes(x = Graduates, y = residuals)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. % HS Graduates",
       x = "% HS Graduates",
       y = "Residuals")
print(p3)

# ---- 4. Compute correlation
correlation <- cor(poverty$Graduates, poverty$Poverty)
cat("Correlation between % HS Graduates and % in Poverty:", correlation, "\n")