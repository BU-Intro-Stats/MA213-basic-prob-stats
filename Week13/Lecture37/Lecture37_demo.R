# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# Load the data
beauty <- read.csv("beauty.csv")

# ---- 1. Fit the model that was chosen in Lecture 36 with step (based on AIC) ----

response = "profevaluation"
predictors <- c("beauty", "gender", "age", "formal", "native", 
                "tenure")

model_string = paste(response, "~", paste(predictors, collapse = " + "))

fit <- lm(model_string, data = beauty)

summary(fit)

# ---- 2. Check the conditions ----

# Compute fitted values and residuals
beauty$yhat <- fitted(fit)
beauty$residuals <- residuals(fit)

# 1. Residuals are nearly normal
#  Histogram of residuals
hist(beauty$residuals, main = "Histogram of Residuals", xlab = "Residuals")

# 2. Residuals have constant variability (relative to fitted values)
# Scatter plot of residuals vs. fitted values
plot(beauty$yhat, beauty$residuals, 
     main = "Residuals vs. Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

# Scatter plot of abs(residuals) vs. fitted values
plot(beauty$yhat, abs(beauty$residuals), 
     main = "abs(Residuals) vs. Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

# 3. Residuals are independent
# Plot residuals in the order of data collection
plot(beauty$residuals,
     main = "Residuals in Order of Data Collection",
     xlab = "Order of Data Collection",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Scatter plot of residual(i) vs. residual(i-1)
plot(beauty$residuals[-1], beauty$residuals[-nrow(beauty)],
     main = "Residual(i) vs. Residual(i-1)",
     xlab = "Residual(i-1)",
     ylab = "Residual(i)")
# Check whether residuals are correlated to the next residual
cor(beauty$residuals[-1], beauty$residuals[-nrow(beauty)])

# 4. Each variable is linearly related to the outcome (and constant variability)

# Beauty (scatter plot for continuous variable)
pred = 'beauty'
plot(beauty[[pred]], beauty$residuals,
     main = paste("Residuals vs.", pred),
     xlab = pred,
     ylab = "Residuals")
abline(h = 0, col = "red")

# Gender (box plot for categorical variable)
pred = 'gender'
boxplot(beauty$residuals ~ beauty[[pred]],
        main = paste("Residuals vs.", pred),
        xlab = pred,
        ylab = "Residuals")
abline(h = 0, col = "red")

# Age
pred = 'age'
plot(beauty[[pred]], beauty$residuals,
     main = paste("Residuals vs.", pred),
     xlab = pred,
     ylab = "Residuals")
abline(h = 0, col = "red")

# Formal
pred = 'formal'
boxplot(beauty$residuals ~ beauty[[pred]],
        main = paste("Residuals vs.", pred),
        xlab = pred,
        ylab = "Residuals")
abline(h = 0, col = "red")

# Non-native English speaker
pred = 'native'
boxplot(beauty$residuals ~ beauty[[pred]],
        main = paste("Residuals vs.", pred),
        xlab = pred,
        ylab = "Residuals")
abline(h = 0, col = "red")

# Tenure status
pred = 'tenure'
boxplot(beauty$residuals ~ beauty[[pred]],
        main = paste("Residuals vs.", pred),
        xlab = pred,
        ylab = "Residuals")
abline(h = 0, col = "red")

