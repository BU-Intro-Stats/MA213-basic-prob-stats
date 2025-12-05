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
head(mc_maze_data)
names(mc_maze_data)

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

# Plot spikes for 10 seconds
spike_data <- mc_maze_data[1:10000, c("time", spike_cols)] %>%
    pivot_longer(cols = -time, names_to = "neuron", values_to = "spikes") %>%
    filter(spikes > 0)

ggplot(spike_data, aes(x = time, y = neuron)) +
    geom_point(shape = "|", size = 0.5) +
    labs(x = 'Time (s)', y = 'Neuron', 
         title = 'Spike Raster Plot') +
    scale_y_discrete(limits = spike_cols,
                     breaks = spike_cols[seq(1, length(spike_cols), by = 100)]) +
    theme_minimal() +
    theme(legend.position = "none")

# Just first 3 neurons
# Convert to ggplot for consistent styling with firing rates plot
spike_data_3 <- spike_data %>% filter(neuron %in% spike_cols[1:3])

ggplot(spike_data_3, aes(x = time, y = neuron, color = neuron)) +
    geom_point(shape = "|", size = 3) +
    labs(x = 'Time (s)', y = 'Neuron', 
         title = 'Spike Raster Plot (first 3 neurons)') +
    theme_minimal()

# Plot firing rates for first 3 neurons for 10 seconds
fr_data <- mc_maze_data[1:10000, c("time", fr_cols[1:3])] %>%
    pivot_longer(cols = -time, names_to = "neuron", values_to = "firing_rate")

p2 <- ggplot(fr_data, aes(x = time, y = firing_rate, color = neuron)) +
    geom_line() +
    labs(x = 'Time (s)', y = 'Firing Rate', title = 'Firing Rates for first 3 neurons') +
    theme_minimal()
print(p2)


# ---- 2. Fit a linear regression model to predict cursor position from firing rates ----
# Fit linear regression model with all spikes as predictors
response = "cursor_x"
predictors <- fr_cols
model_string = paste(response, "~", paste(predictors, collapse = " + "))

fit_x <- lm(model_string, data = train_data)
summary(fit_x)

# Use step to get the best 10 predictors for cursor_x and cursor_y
null_model <- lm(cursor_x ~ 1, data = train_data)
full_model <- fit_x
step_model_x <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                   direction = "forward", steps = 10)
summary(step_model_x)


# Do the same for the y position
response = "cursor_y"
model_string = paste(response, "~", paste(predictors, collapse = " + "))
fit_y <- lm(model_string, data = train_data)
# summary(fit_y)

null_model <- lm(cursor_y ~ 1, data = train_data)
full_model <- fit_y
step_model_y <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                     direction = "forward", steps = 10)
# summary(step_model_y)

# ---- 3. Visualize model predictions ----
# Predict cursor positions (on training data for now)
train_data$pred_cursor_x <- fitted(fit_x)
train_data$pred_cursor_y <- fitted(fit_y)
train_data$pred_cursor_step_x <- predict(step_model_x, newdata = train_data)
train_data$pred_cursor_step_y <- predict(step_model_y, newdata = train_data)

# Plot the true vs predicted cursor trajectory on training data
ggplot(train_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path(color = "blue", alpha = 0.5) +
    geom_path(aes(x = pred_cursor_x, y = pred_cursor_y), color = "red", alpha = 0.5) +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'True (blue) vs Predicted (red) Cursor Trajectory (Full model, Training Data)') +
    theme_minimal() 

ggplot(train_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path(color = "blue", alpha = 0.5) +
    geom_path(aes(x = pred_cursor_step_x, y = pred_cursor_step_y), color = "green", alpha = 0.5) +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'True (blue) vs Predicted (green) Cursor Trajectory (Step model, Training Data)') +
    theme_minimal()

# Plot the true and predicted cursor x position as a function of time, 10 seconds
ggplot(train_data %>% filter(time <= 10), aes(x = time)) +
    geom_line(aes(y = cursor_x, color = "True Cursor X"), alpha = 0.5) +
    geom_line(aes(y = pred_cursor_x, color = "Predicted Cursor X (Full model)"), alpha = 0.5) +
    geom_line(aes(y = pred_cursor_step_x, color = "Predicted Cursor X (Step model)"), alpha = 0.5) +
    labs(x = 'Time (s)', y = 'Cursor X Position', 
             title = 'True vs Predicted Cursor X Position (Training Data)', color = NULL) +
    scale_color_manual(values = c("True Cursor X" = "black", "Predicted Cursor X (Full model)" = "red", "Predicted Cursor X (Step model)" = "green")) +
    theme_minimal()

# Plot the true vs predicted cursor x position as scatter plot on training data (both models)
ggplot(train_data, aes(x = cursor_x, y = pred_cursor_x)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    labs(x = 'True Cursor X Position', y = 'Predicted Cursor X Position (Full model)', 
             title = 'True vs Predicted Cursor X Position (Full model, Training Data)') +
    theme_minimal()

ggplot(train_data, aes(x = cursor_x, y = pred_cursor_step_x)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    labs(x = 'True Cursor X Position', y = 'Predicted Cursor X Position (Step model)', 
             title = 'True vs Predicted Cursor X Position (Step model, Training Data)') +
    theme_minimal()

# Calculate R^2 on training data
r2_x_train <- summary(fit_x)$r.squared
cat("R^2 on training data - Cursor X (Full model):", r2_x_train, "\n")

r2_x_train_step <- summary(step_model_x)$r.squared
cat("R^2 on training data - Cursor X (Step model):", r2_x_train_step, "\n")

# Calculate adjusted R^2 on training data
adj_r2_x_train <- summary(fit_x)$adj.r.squared
cat("Adjusted R^2 on training data - Cursor X (Full model):", adj_r2_x_train, "\n")

adj_r2_x_train_step <- summary(step_model_x)$adj.r.squared
cat("Adjusted R^2 on training data - Cursor X (Step model):", adj_r2_x_train_step, "\n")

# note that the adjusted R^2 is unbiased *if the model is accurate*

# ---- 4. Evaluate goodness of fit ----
# (1) Nearly normal residuals (histogram)
train_data$residuals_x <- residuals(fit_x)
train_data$residuals_step_x <- residuals(step_model_x)

ggplot(train_data, aes(x = residuals_x)) +
    geom_histogram(binwidth = 0.1, color = "black", fill = "lightblue") +
    labs(x = 'Residuals (Cursor X)', y = 'Frequency', 
             title = 'Histogram of Residuals for Cursor X (Full model)') +
    theme_minimal()

ggplot(train_data, aes(x = residuals_step_x)) +
    geom_histogram(binwidth = 0.1, color = "black", fill = "lightgreen") +
    labs(x = 'Residuals (Cursor X - Step model)', y = 'Frequency', 
             title = 'Histogram of Residuals for Cursor X (Step model)') +
    theme_minimal()

# Probability plots (Q-Q plots)
qqnorm(train_data$residuals_x, main = "Q-Q Plot of Residuals for Cursor X (Full model)")
qqline(train_data$residuals_x, col = "black")

qqnorm(train_data$residuals_step_x, main = "Q-Q Plot of Residuals for Cursor X (Step model)")
qqline(train_data$residuals_step_x, col = "black")

# (2) Constant variability (residuals vs fitted values)
ggplot(train_data, aes(x = pred_cursor_x, y = residuals_x)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "black") +
    labs(x = 'Fitted Values (Cursor X)', y = 'Residuals', 
             title = 'Residuals vs Fitted Values for Cursor X (Full model)') +
    theme_minimal()

ggplot(train_data, aes(x = pred_cursor_step_x, y = residuals_step_x)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "black") +
    labs(x = 'Fitted Values (Cursor X - Step model)', y = 'Residuals', 
             title = 'Residuals vs Fitted Values for Cursor X (Step model)') +
    theme_minimal()

# (3) Independence of residuals (residuals over time)
ggplot(train_data, aes(x = time, y = residuals_x)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "black") +
    labs(x = 'Time (s)', y = 'Residuals', 
             title = 'Residuals over Time for Cursor X (Full model)') +
    theme_minimal()

# Residuals lag plot
ggplot(train_data, aes(x = lag(residuals_x), y = residuals_x)) +
    geom_point(alpha = 0.5) +
    labs(x = 'Residual(i-1)', y = 'Residual(i)', 
             title = 'Lag Plot of Residuals for Cursor X (Full model)') +
    theme_minimal()
cor(train_data$residuals_x[-1], train_data$residuals_x[-nrow(train_data)])

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
test_data$pred_cursor_x_step <- predict(step_model_x, newdata = test_data)

# Calculate R^2 on test data
ss_total_x <- sum((test_data$cursor_x - mean(test_data$cursor_x))^2)
ss_res_x <- sum((test_data$cursor_x - test_data$pred_cursor_x)^2)
r2_x_test <- 1 - (ss_res_x / ss_total_x)
cat("R^2 on test data - Cursor X:", r2_x_test, "\n")

ss_res_x_step <- sum((test_data$cursor_x - test_data$pred_cursor_x_step)^2)
r2_x_test_step <- 1 - (ss_res_x_step / ss_total_x)
cat("R^2 on test data - Cursor X (Step model):", r2_x_test_step, "\n")

# ---- 6. Use the step model for inference on the test data ----
test_data$pred_cursor_y_step <- predict(step_model_y, newdata = test_data)

# Plot the true vs predicted cursor trajectory on test data
ggplot(test_data, aes(x = cursor_x, y = cursor_y)) +
    geom_path(color = "blue", alpha = 0.5) +
    geom_path(aes(x = pred_cursor_x_step, y = pred_cursor_y_step), color = "green", alpha = 0.5) +
    labs(x = 'Cursor X Position', y = 'Cursor Y Position', 
             title = 'True (blue) vs Predicted (green) Cursor Trajectory (Test Data)') +
    theme_minimal()

# Plot the true and predicted cursor x position as a function of time on test data
ggplot(test_data, aes(x = time)) +
    geom_line(aes(y = cursor_x, color = "True Cursor X"), alpha = 0.5) +
    geom_line(aes(y = pred_cursor_x, color = "Predicted Cursor X"), alpha = 0.5) +
    labs(x = 'Time (s)', y = 'Cursor X Position', 
             title = 'True vs Predicted Cursor X Position (Test Data)', color = NULL) +
    scale_color_manual(values = c("True Cursor X" = "black", "Predicted Cursor X" = "green")) +
    theme_minimal()

# Same as above, but with predictive intervals
pred_with_pi <- predict(step_model_x, newdata = test_data, interval = "prediction", level = 0.95)
test_data$pred_cursor_x_lower <- pred_with_pi[, "lwr"]
test_data$pred_cursor_x_upper <- pred_with_pi[, "upr"]

ggplot(test_data, aes(x = time)) +
    geom_line(aes(y = cursor_x, color = "True Cursor X"), alpha = 0.5) +
    geom_line(aes(y = pred_cursor_x, color = "Predicted Cursor X"), alpha = 0.5) +
    geom_ribbon(aes(ymin = pred_cursor_x_lower, ymax = pred_cursor_x_upper), alpha = 0.2, fill = "blue") +
    labs(x = 'Time (s)', y = 'Cursor X Position', 
             title = 'True vs Predicted Cursor X Position with 95% PI (Test Data)', color = NULL) +
    scale_color_manual(values = c("True Cursor X" = "black", "Predicted Cursor X" = "green")) +
    theme_minimal()