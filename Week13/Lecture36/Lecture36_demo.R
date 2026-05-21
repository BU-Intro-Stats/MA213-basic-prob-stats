# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# Load the data
beauty <- read.csv("beauty.csv")

# ---- 1. Fit the full model ----

response = "profevaluation"
predictors <- c("beauty", "gender", "age", "formal", "lower", "native", 
                "minority", "students", "tenure")

model_string = paste(response, "~", paste(predictors, collapse = " + "))

fit <- lm(model_string, data = beauty)

summary(fit)

# ---- 2. Use Step to do backward elimination with AIC ----
step(fit)

# ---- 3. Use a loop to do backward elimination with adjusted R^2 ----
backward_elimination_adj_r2 <- function(data, response, predictors) {
  current_predictors <- predictors
  best_adj_r2 <- -Inf
  best_model <- NULL
  
  repeat {
    models <- list()
    adj_r2_values <- c()
    
    for (pred in current_predictors) {
      temp_predictors <- setdiff(current_predictors, pred)
      formula <- as.formula(paste(response, "~", paste(temp_predictors, collapse = "+")))
      model <- lm(formula, data = data)
      models[[pred]] <- model
      adj_r2_values <- c(adj_r2_values, summary(model)$adj.r.squared)
    }
    
    max_adj_r2 <- max(adj_r2_values)
    if (max_adj_r2 > best_adj_r2) {
      best_adj_r2 <- max_adj_r2
      best_pred_to_remove <- current_predictors[which.max(adj_r2_values)]
      current_predictors <- setdiff(current_predictors, best_pred_to_remove)
      best_model <- models[[best_pred_to_remove]]
    } else {
      break
    }
  }
  
  return(best_model)
}

best_model_adj_r2 <- backward_elimination_adj_r2(beauty, "profevaluation", predictors)
summary(best_model_adj_r2)
