# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# ---- 1. Fit a big model of interest rates for the Loans dataset----
loans <- read.csv("Loans.csv")

fit <- lm(interest_rate ~ 
             income_ver +
             debt_to_income +
             bankruptcy +
             term,
           data = loans)
summary(fit)

# -------------------------------------------------------------------
# ---- 2. Compute R^2 three ways for female_house vs. poverty
poverty <- read.table("poverty.txt", header = T, sep = "\t")

# rename the column for brevity
poverty$female_house <- poverty$PercentFemaleHouseholderNoHusbandPresent

# Fit the model
fit_poverty <- lm(Poverty ~ female_house, data = poverty)

# Plot the data and regression line
ggplot(poverty, aes(x = female_house, y = Poverty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Percent of Female-Headed Households",
       y = "Poverty Rate (%)",
       title = "Poverty Rate vs. Female-Headed Households")

# Print the ANOVA table
anova(fit_poverty)

# R^2 method 1: Corr(Y,X)^2
R2_1 <- cor(poverty$Poverty, poverty$female_house)^2
print(paste("R^2 method 1:", R2_1))

# R^2 method 2: Corr(Y,Yhat)^2
poverty$Poverty_hat <- predict(fit_poverty)
R2_2 <- cor(poverty$Poverty, poverty$Poverty_hat)^2
print(paste("R^2 method 2:", R2_2)) 

# R^2 method 3: (Explained SS) / (Total SS)
SS_Total <- sum((poverty$Poverty - mean(poverty$Poverty))^2)
SS_Error <- sum((poverty$Poverty - poverty$Poverty_hat)^2)
SS_Model = SS_Total - SS_Error
R2_3 <- SS_Model / SS_Total
print(paste("R^2 method 3:", R2_3)) 

# Note: we could also have computed SS_Model directly as:
# SS_Model <- sum((poverty$Poverty_hat - mean(poverty$Poverty))^2)

# -------------------------------------------------------------------
# ---- 3. What happens when the model has multiple predictors? ----
fit_poverty2 <- lm(Poverty ~ female_house + White, data = poverty)
summary(fit_poverty2)
anova(fit_poverty2)

# Method 1: Corr(Y, X)^2 -- there are two different X's now!
# So this method is not applicable in multiple regression

# Method 2: Corr(Y, Yhat)^2
poverty$Poverty_hat2 <- predict(fit_poverty2)
R2_2_multi <- cor(poverty$Poverty, poverty$Poverty_hat2)
print(paste("R^2 method 2 (multiple predictors):", R2_2_multi^2))

# Method 3: (Explained SS) / (Total SS)
SS_Error2 <- sum((poverty$Poverty - poverty$Poverty_hat2)^2)
SS_Model2 = SS_Total - SS_Error2
R2_3_multi <- SS_Model2 / SS_Total
print(paste("R^2 method 3 (multiple predictors):", R2_3_multi)) 

# Compare the R^2 values with and without the additional predictor
print(paste("R^2 with one predictor:", R2_3))
print(paste("R^2 with two predictors:", R2_3_multi))

# ---- 4. What if we add a useless predictor? ----
set.seed(1414)  # for reproducibility

# Add a predictor that doesn't relate to poverty
poverty$useless_predictor <- rnorm(nrow(poverty), mean = 50, sd = 10) # random noise

# Fit the model with the predictor
fit_poverty3 <- lm(Poverty ~ female_house + White + useless_predictor, data = poverty)
summary(fit_poverty3)
anova(fit_poverty3)

# Compute R^2
poverty$Poverty_hat3 <- predict(fit_poverty3)
R2_3_multi_useless <- sum((poverty$Poverty_hat3 - mean(poverty$Poverty))^2) / SS_Total

# Compare the R^2 from the models with 1, 2, and 3 predictors
print(paste("R^2 with one predictor:", R2_3))
print(paste("R^2 with two predictors:", R2_3_multi))
print(paste("R^2 with three predictors (including useless):", R2_3_multi_useless))

# -------------------------------------------------------------------
# ---- 5. Calculate Adjusted R^2 for all three models ----
n <- nrow(poverty)  # number of observations

# Adjusted R^2 for model with 1 predictor
p <- 1               # number of predictors
adj_R2 <- 1 - ( (1 - R2_3) * (n - 1) / (n - p - 1) )
print(paste("Adjusted R^2 with one predictor:", adj_R2))

# Adjusted R^2 for model with 2 predictors
p <- 2               # number of predictors
adj_R2_multi <- 1 - ( (1 - R2_3_multi) * (n - 1) / (n - p - 1) )
print(paste("Adjusted R^2 with two predictors:", adj_R2_multi))

# Adjusted R^2 for model with 3 predictors (including useless)
p <- 3               # number of predictors
adj_R2_multi_useless <- 1 - ( (1 - R2_3_multi_useless) * (n - 1) / (n - p - 1) )
print(paste("Adjusted R^2 with three predictors (including useless):", adj_R2_multi_useless))

