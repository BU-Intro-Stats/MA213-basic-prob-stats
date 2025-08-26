# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. 68-95-99.5 (Empirical) Rule for Standard Normal ----

# Areas under the curve
pnorm(1) - pnorm(-1)  # standard normal, +- 1 sd (~.68)

pnorm(2) - pnorm(-2)  # standard normal, +- 2 sd's (~0.95)

pnorm(3) - pnorm(-3)  # standard normal, +- 3 sd's (~0.99)

x <- seq(-5,5,0.1)
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue') +
  geom_vline(xintercept=1, color='red', linetype='dotted') +
  geom_vline(xintercept=-1, color='red', linetype='dotted') +
  geom_vline(xintercept=2, color='red', linetype='dotted') +
  geom_vline(xintercept=-2, color='red', linetype='dotted') +
  geom_vline(xintercept=3, color='red', linetype='dotted') +
  geom_vline(xintercept=-3, color='red', linetype='dotted') +
  scale_x_continuous(breaks=c(-5:5))


# ---- 2. Adding Variability ----

# Now let's change the sd and figure out the new cutoffs:
# If we have N(mean=0, sd=3), then +- 1 sd is at +-3;
# +- 2 sd's is at +- 6; and +- 3 sd's is at +-9

# Areas under the curve
pnorm(3, mean=0, sd=3) - pnorm(-3, mean=0, sd=3)  # ~0.68
pnorm(6, mean=0, sd=3) - pnorm(-6, mean=0, sd=3)  # ~0.95
pnorm(9, mean=0, sd=3) - pnorm(-9, mean=0, sd=3)  # ~0.99

x <- seq(-10,10,0.1)
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=3), color='steelblue') +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='gray', alpha=0.8) +
  geom_vline(xintercept=3, color='red', linetype='dotted') +
  geom_vline(xintercept=-3, color='red', linetype='dotted') +
  geom_vline(xintercept=6, color='red', linetype='dotted') +
  geom_vline(xintercept=-6, color='red', linetype='dotted') +
  geom_vline(xintercept=9, color='red', linetype='dotted') +
  geom_vline(xintercept=-9, color='red', linetype='dotted') +
  scale_x_continuous(breaks=c(-10:10))

# ---- 3. Applying the Empirical Rule to real datasets ----

# Let's define a function to compute the proportion of samples within 1, 2, or 3 standard deviations
empirical_rule_test <- function(x, name = "Dataset") {
  x <- na.omit(x)
  mu <- mean(x)
  sigma <- sd(x)
  within1 <- mean(abs(x - mu) <= sigma)
  within2 <- mean(abs(x - mu) <= 2 * sigma)
  within3 <- mean(abs(x - mu) <= 3 * sigma)
  cat(sprintf("\n%s\n", name))
  cat(sprintf("Within     1 SD: %.1f%%\n", within1 * 100))
  cat(sprintf("Within     2 SDs: %.1f%%\n", within2 * 100))
  cat(sprintf("Within     3 SDs: %.1f%%\n", within3 * 100))
}

# Apply it to some built-in datasets
n50 = rnorm(50, mean=0, sd=3)
n500 = rnorm(500, mean=0, sd=3)
empirical_rule_test(n50, "Simulated N(0,3) Data, N=50")
empirical_rule_test(n500, "Simulated N(0,3) Data, N=500")
empirical_rule_test(trees$Height, "trees$Height (Girth of Black Cherry Trees)")
empirical_rule_test(airquality$Ozone, "airquality$Ozone (Ozone)")
empirical_rule_test(faithful$eruptions, "faithful$eruptions (Old Faithful)")

# Visualize the datasets
plot_with_sd <- function(x, name) {
  x <- na.omit(x)
  mu <- mean(x)
  sigma <- sd(x)
  hist(x, breaks = 20, main = name, xlab = "", col = "lightblue", border = "white")
  abline(v = mu, col = "red", lwd = 2)
  abline(v = mu + c(-1,1)*sigma, col = "orange", lwd = 2, lty = 2)
  abline(v = mu + c(-2,2)*sigma, col = "forestgreen", lwd = 2, lty = 3)
  abline(v = mu + c(-3,3)*sigma, col = "blue", lwd = 2, lty = 4)
  legend("topright",
         legend = c("Mean", "±1 SD", "±2 SDs", "±3 SDs"),
         col = c("red", "orange", "forestgreen", "blue"),
         lty = c(1,2,3,4), lwd = 2, bty = "n"
  )
}

par(mfrow = c(2,2))
plot_with_sd(n50, "Normal N(0,3), N=50")
plot_with_sd(trees$Height, "trees$Height")
plot_with_sd(airquality$Ozone, "airquality$Ozone")
plot_with_sd(faithful$eruptions, "faithful$eruptions")
par(mfrow = c(1,1))
