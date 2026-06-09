# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# ---- 1. Compute the ANOVA table for the aldrin dataset
# Load data
aldrin <- read.csv("aldrin.csv", header = T)

# Fit one-way ANOVA and show table up to the F statistic
fit <- aov(aldrin ~ depth, data = aldrin)
anova_tab <- anova(fit)
print(anova_tab)

df_between <- anova_tab["depth", "Df"]
df_within  <- anova_tab["Residuals", "Df"]
F_obs      <- anova_tab["depth", "F value"]


# ---- 2. Simulate the post-hoc t-tests, assuming no difference in group means
set.seed(1)
n_sim <- 10000
n <- nrow(aldrin)
mu0 <- mean(aldrin$aldrin)
sigma_hat <- sd(aldrin$aldrin)

F_sim <- numeric(n_sim)
t_sim <- numeric(n_sim)
for(i in seq_len(n_sim)) {
    # Simulate n values assuming that the mean and std dev are the same in all groups
    sim_y <- rnorm(n, mean = mu0, sd = sigma_hat)
    sim_fit <- aov(sim_y ~ aldrin$depth)
    sim_an  <- anova(sim_fit)
    F_sim[i] <- sim_an["aldrin$depth", "F value"] 
    
    mse = sim_an["Residuals","Mean Sq"]
    se = sqrt(mse/10+mse/10)
    xbar1 = mean(sim_y[1:10])
    xbar2 = mean(sim_y[11:20])
    t_sim[i] <- (xbar1-xbar2)/se
}

# Plot simulated null distribution with theoretical F overlay
df_sim <- data.frame(F = F_sim)
plt <- ggplot(df_sim, aes(x = F)) +
    geom_histogram(aes(y=after_stat(density)), bins=60, boundary=0, color="black", fill="lightblue") +
    stat_function(fun = function(x) df(x, df_between, df_within), color="red", linewidth=1) +
    geom_vline(xintercept = F_obs, color="black", linetype="dashed", linewidth=1) +
    labs(x = "F statistic", y = "Density",
             title = "Simulated null distribution of F (blue) with theoretical F(df1,df2) in red",
             subtitle = paste0("Observed F = ", round(F_obs, 3))) +
    theme_minimal()
print(plt)

# Plot simulated null distribution with theoretical t overlay
df_sim <- data.frame(t = t_sim)
plt <- ggplot(df_sim, aes(x = t)) +
  geom_histogram(aes(y=after_stat(density)), bins=60, boundary=0, color="black", fill="lightblue") +
  stat_function(fun = function(x) dt(x, df_within), color="red", linewidth=1) +
  labs(x = "t statistic", y = "Density",
       title = "Simulated null distribution of t (blue) with theoretical t(df2) in red") +
  theme_minimal()
print(plt)


