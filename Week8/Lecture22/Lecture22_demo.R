# ---- 0. Setup and load libraries, if any ----

library(ggplot2)

# ---- 1. Plotting the chi-squared distribution, and computing prob.s ----

# Set an x-axis over which to evaluate the chi-squared distribution
x <- seq(1, 40)

# Which function do you think we should use to compute P(x) for each x above:
# dchisq, pchisq, qchisq, or rchisq?

df05 <- dchisq(x, df=0.5)  
df1 <- dchisq(x, df=1)
df5 <- dchisq(x, df=5)
df10 <- dchisq(x, df=10)
df50 <- dchisq(x, df=30)

data <- data.frame(rep(x, 5), 
                   c(df05, df1, df5, df10, df50), 
                   c(rep(0.5, 40), rep(1, 40), rep(5, 40), rep(10, 40), 
                     rep(30, 40)))
colnames(data) <- c("x", "density", "df")

ggplot(data=data, aes(x=x, y=density, color=factor(df))) +
  geom_line() +
  labs(color="df") +
  ggtitle("Chi-squared distribution with different degrees of freedom (df)")
  
