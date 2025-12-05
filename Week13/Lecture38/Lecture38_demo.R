# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
mc_maze_data <- read.csv("mc_maze_simplified.csv")

# Split into 40s training, 20s testing
train_data <- mc_maze_data %>% filter(time <= 40)
test_data <- mc_maze_data %>% filter(time > 40)

# Make a list of neuron spike columns and firing rate columns
spike_cols <- names(mc_maze_data)[grep("neuron_spikes_", names(mc_maze_data))]
fr_cols <- names(mc_maze_data)[grep("neuron_fr_", names(mc_maze_data))]

# ---- 1. Visualize the data ----
# Plot cursor trajectory
ggplot(mc_maze_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path() +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'Cursor Trajectory') +
    theme_minimal()

# Plot cursor position, both x and y for 10 seconds
pos_data <- mc_maze_data[1:10000, c("time", "cursor_x", "cursor_y")] %>%
    pivot_longer(cols = c(cursor_x, cursor_y), names_to = "position", values_to = "value")

p3 <- ggplot(pos_data, aes(x = time, y = value, color = position)) +
    geom_line() +
    scale_color_manual(values = c("cursor_x" = "black", "cursor_y" = "red"),
                                         labels = c("X Position", "Y Position")) +
    labs(x = 'Time (s)', y = 'Cursor Position', 
             title = 'Cursor Position (X and Y)', color = NULL) +
    theme_minimal()
print(p3)

# Plot spikes for first 3 neurons for 10 seconds
spike_data <- mc_maze_data[1:10000, c("time", spike_cols[1:3])] %>%
    pivot_longer(cols = -time, names_to = "neuron", values_to = "spikes")

p1 <- ggplot(spike_data, aes(x = time, y = spikes, color = neuron)) +
    geom_line() +
    labs(x = 'Time (s)', y = 'Spikes', title = 'Spikes for first 3 neurons') +
    theme_minimal()
print(p1)

# Plot firing rates for first 3 neurons for 10 seconds
fr_data <- mc_maze_data[1:10000, c("time", fr_cols[1:3])] %>%
    pivot_longer(cols = -time, names_to = "neuron", values_to = "firing_rate")

p2 <- ggplot(fr_data, aes(x = time, y = firing_rate, color = neuron)) +
    geom_line() +
    labs(x = 'Time (s)', y = 'Firing Rate', title = 'Firing Rates for first 3 neurons') +
    theme_minimal()
print(p2)


# ---- 2. Fit a linear regression model to predict cursor position from firing rates ----
# Fit linear regression model
response = "cursor_x"
predictors <- fr_cols
model_string = paste(response, "~", paste(predictors, collapse = " + "))

# x position
fit_x <- lm(model_string, data = train_data)
summary(fit_x)

# y position
response = "cursor_y"
model_string = paste(response, "~", paste(predictors, collapse = " + "))
fit_y <- lm(model_string, data = train_data)
summary(fit_y)

# ---- 3. Visualize model predictions ----
# Predict cursor positions (on training data for now)
train_data$pred_cursor_x <- fitted(fit_x)
train_data$pred_cursor_y <- fitted(fit_y)

# Plot the true vs predicted cursor trajectory on training data
ggplot(train_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path(color = "blue", alpha = 0.5) +
    geom_path(aes(x = pred_cursor_x, y = pred_cursor_y), color = "red", alpha = 0.5) +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'True (blue) vs Predicted (red) Cursor Trajectory (Training Data)') +
    theme_minimal() 

# Plot the true and predicted cursor x position as a function of time
ggplot(train_data, aes(x = time)) +
    geom_line(aes(y = cursor_x, color = "True Cursor X"), alpha = 0.5) +
    geom_line(aes(y = pred_cursor_x, color = "Predicted Cursor X"), alpha = 0.5) +
    labs(x = 'Time (s)', y = 'Cursor X Position', 
             title = 'True vs Predicted Cursor X Position (Training Data)', color = NULL) +
    scale_color_manual(values = c("True Cursor X" = "black", "Predicted Cursor X" = "red")) +
    theme_minimal()

# Plot the true vs predicted cursor x position as scatter plot on training data
ggplot(train_data, aes(x = cursor_x, y = pred_cursor_x)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(x = 'True Cursor X Position', y = 'Predicted Cursor X Position', 
             title = 'True vs Predicted Cursor X Position (Training Data)') +
    theme_minimal()

# Calculate R^2 on training data
r2_x_train <- summary(fit_x)$r.squared
r2_y_train <- summary(fit_y)$r.squared
cat("R^2 on training data - Cursor X:", r2_x_train, "\n")
cat("R^2 on training data - Cursor Y:", r2_y_train, "\n")

# Calculate adjusted R^2 on training data
adj_r2_x_train <- summary(fit_x)$adj.r.squared
adj_r2_y_train <- summary(fit_y)$adj.r.squared
cat("Adjusted R^2 on training data - Cursor X:", adj_r2_x_train, "\n")
cat("Adjusted R^2 on training data - Cursor Y:", adj_r2_y_train, "\n")

# ---- 4. Evaluate goodness of fit ----
# (1) Nearly normal residuals (histogram)
train_data$residuals_x <- residuals(fit_x)
train_data$residuals_y <- residuals(fit_y)

ggplot(train_data, aes(x = residuals_x)) +
    geom_histogram(binwidth = 0.1, color = "black", fill = "lightblue") +
    labs(x = 'Residuals (Cursor X)', y = 'Frequency', 
             title = 'Histogram of Residuals for Cursor X') +
    theme_minimal()

# Probability plots (Q-Q plots)
qqnorm(train_data$residuals_x, main = "Q-Q Plot of Residuals for Cursor X")
qqline(train_data$residuals_x, col = "red")

# (2) Constant variability (residuals vs fitted values)
ggplot(train_data, aes(x = pred_cursor_x, y = residuals_x)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red") +
    labs(x = 'Fitted Values (Cursor X)', y = 'Residuals', 
             title = 'Residuals vs Fitted Values for Cursor X') +
    theme_minimal()

# (3) Independence of residuals (residuals over time)
ggplot(train_data, aes(x = time, y = residuals_x)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red") +
    labs(x = 'Time (s)', y = 'Residuals', 
             title = 'Residuals over Time for Cursor X') +
    theme_minimal()

# Residuals lag plot
ggplot(train_data, aes(x = lag(residuals_x), y = residuals_x)) +
    geom_point(alpha = 0.5) +
    labs(x = 'Residual(i-1)', y = 'Residual(i)', 
             title = 'Lag Plot of Residuals for Cursor X') +
    theme_minimal()

# (4) Linearity and Constant variability for each predictor (just check first 3 neurons)
for (pred in fr_cols[1:3]) {
    pi<-ggplot(train_data, aes_string(x = pred, y = "residuals_x")) +
        geom_point(alpha = 0.5) +
        geom_hline(yintercept = 0, color = "red") +
        labs(x = pred, y = 'Residuals', 
                 title = paste('Residuals vs', pred, 'for Cursor X')) +
        theme_minimal()

    print(pi)
}

# ---- 5. Evaluate model performance on test data ----
# Predict on test data
test_data$pred_cursor_x <- predict(fit_x, newdata = test_data)
test_data$pred_cursor_y <- predict(fit_y, newdata = test_data)

# Calculate R^2 on test data
ss_total_x <- sum((test_data$cursor_x - mean(test_data$cursor_x))^2)
ss_res_x <- sum((test_data$cursor_x - test_data$pred_cursor_x)^2)
r2_x_test <- 1 - (ss_res_x / ss_total_x)

ss_total_y <- sum((test_data$cursor_y - mean(test_data$cursor_y))^2)
ss_res_y <- sum((test_data$cursor_y - test_data$pred_cursor_y)^2)
r2_y_test <- 1 - (ss_res_y / ss_total_y)

cat("R^2 on test data - Cursor X:", r2_x_test, "\n")
cat("R^2 on test data - Cursor Y:", r2_y_test, "\n")

# ---- 6. Visualize model predictions on test data ----
# Plot the true vs predicted cursor trajectory on test data
ggplot(test_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path(color = "blue", alpha = 0.5) +
    geom_path(aes(x = pred_cursor_x, y = pred_cursor_y), color = "red", alpha = 0.5) +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'True (blue) vs Predicted (red) Cursor Trajectory (Test Data)') +
    theme_minimal()

# Plot the true and predicted cursor x position as a function of time on test data
ggplot(test_data, aes(x = time)) +
    geom_line(aes(y = cursor_x, color = "True Cursor X"), alpha = 0.5) +
    geom_line(aes(y = pred_cursor_x, color = "Predicted Cursor X"), alpha = 0.5) +
    labs(x = 'Time (s)', y = 'Cursor X Position', 
             title = 'True vs Predicted Cursor X Position (Test Data)', color = NULL) +
    scale_color_manual(values = c("True Cursor X" = "black", "Predicted Cursor X" = "red")) +
    theme_minimal()

# ---- 7. Briefly, try hand velocity prediction ----
# Fit linear regression model for hand velocity
response = "hand_vx"
model_string = paste(response, "~", paste(predictors, collapse = " + "))
fit_vx <- lm(model_string, data = train_data)
summary(fit_vx)

# Predict on test data
test_data$pred_hand_vx <- predict(fit_vx, newdata = test_data)
# Calculate R^2 on test data for hand velocity
ss_total_vx <- sum((test_data$hand_vx - mean(test_data$hand_vx))^2)
ss_res_vx <- sum((test_data$hand_vx - test_data$pred_hand_vx)^2)
r2_vx_test <- 1 - (ss_res_vx / ss_total_vx)
cat("R^2 on test data - Hand Velocity X:", r2_vx_test, "\n") 

# ---- 8. Briefly, try using step to get the best 10 predictors ----
# Start with null model
null_model <- lm(cursor_x ~ 1, data = train_data)
full_model <- lm(model_string, data = train_data)
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                   direction = "forward", steps = 10)
summary(step_model)

# Predict on test data
test_data$pred_step_cursor_x <- predict(step_model, newdata = test_data)
# Calculate R^2 on test data for step model
ss_res_step <- sum((test_data$cursor_x - test_data$pred_step_cursor_x)^2)
r2_step_test <- 1 - (ss_res_step / ss_total_x)
cat("R^2 on test data - Step Model Cursor X:", r2_step_test, "\n")

# Plot the true vs predicted cursor trajectory on test data
ggplot(test_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path(color = "blue", alpha = 0.5) +
    geom_path(aes(x = pred_step_cursor_x, y = pred_cursor_y), color = "green", alpha = 0.5) +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'True (blue) vs Step Model Predicted (green) Cursor Trajectory (Test Data)') +
    theme_minimal()

# Plot the true and step model predicted cursor x position as a function of time on test data
ggplot(test_data, aes(x = time)) +
    geom_line(aes(y = cursor_x, color = "True Cursor X"), alpha = 0.5) +
    geom_line(aes(y = pred_step_cursor_x, color = "Step Model Predicted Cursor X"), alpha = 0.5) +
    labs(x = 'Time (s)', y = 'Cursor X Position', 
             title = 'True vs Step Model Predicted Cursor X Position (Test Data)', color = NULL) +
    scale_color_manual(values = c("True Cursor X" = "black", "Step Model Predicted Cursor X" = "green")) +
    theme_minimal()