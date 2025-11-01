# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Calculate power for the two-sample z-test ----
alpha <- 0.05  # significance level
n1 <- 100       # sample size for group 1
n2 <- 100       # sample size for group 2
sd1 <- 12      # standard deviation for group 1
sd2 <- 12      # standard deviation for group 2
null_mean <- 0  # mean difference under the null hypothesis
alt_mean <- -3 # mean of the alternative hypothesis

# compute parameters for power calculation
effect_size <- abs(alt_mean-null_mean)  # difference in means we want to detect
se <- sqrt((sd1^2 / n1) + (sd2^2 / n2)) # standard error of the difference in means

# Make a reasonable set of x values for plotting
x <- seq(-4 * se + min(null_mean, alt_mean), 
         4 * se + max(null_mean, alt_mean), length.out = 1000)


# Step 1. Plot the sampling distribution (of xbar1-xbar2) under the null and alternative hypotheses
null_dist <-dnorm(x, mean = null_mean, sd = se)
alt_dist <- dnorm(x, mean = alt_mean, sd = se)

plot_df <- rbind(
  data.frame(x = x, y = null_dist, dist = "Null"),
  data.frame(x = x, y = alt_dist, dist = "Alternative")
)

plt <- ggplot(plot_df, aes(x = x, y = y, color = dist)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Null" = "steelblue", "Alternative" = "darkgreen")) +
  labs(title = "Sampling Distributions",
       x = "Difference in Sample Means",
       y = "Density") + 
  theme_minimal(base_size = 15)
  
print(plt)

# Step 2. Find the rejection region
# We reject when (Point estimate-null value)/SE > zstar 
#             or (Point estimate-null value)/SE < -zstar 
# Solving for the Point estimate, that is when
#                Point estimate > null value + zstar*SE
#             or Point estimate < null value - zstar*SE
zstar = qnorm(1 - alpha / 2) # Critical value for the z test
upper_boundary <- null_mean + zstar * se  
lower_boundary <- null_mean - zstar * se

plt <- plt +
  geom_vline(xintercept = c(lower_boundary, upper_boundary), linetype = "dashed", color = "steelblue")

print(plt)

# Step 3. Power is the probability of rejecting the null, if the alternative is true
# I.e., the area under f_A in the rejection region
plt <- plt +
  geom_area(
    data = subset(plot_df, dist=="Alternative" & x >= upper_boundary), 
    fill = 'darkgreen', alpha = 0.5) +
  geom_area(
    data = subset(plot_df, dist=="Alternative" & x <= lower_boundary), 
    fill = 'darkgreen', alpha = 0.5)

print(plt)

power <- pnorm(lower_boundary, mean=alt_mean, sd=se) + (1 - pnorm(upper_boundary, mean=alt_mean, sd=se))


