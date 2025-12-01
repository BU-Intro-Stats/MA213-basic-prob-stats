library(ggplot2)
library(gridExtra)

# set seed
set.seed(42)

x = runif(100, -5, 5)

e = rnorm(100, 0, 0.5)
y1 = (x+e)^2
y2 = exp(x+e)
y3 = 1/(x+e)

# Scatter plots (3 plots in one figure)

p1 = ggplot(data = data.frame(x, y1), aes(x = x, y = y1)) +
    geom_point() +
    ggtitle("(a)") +
    xlab("x") +
    ylab("y") +
    theme_minimal()
p2 = ggplot(data = data.frame(x, y2), aes(x = x, y = y2)) +
    geom_point() +
    ggtitle("(b)") +
    xlab("x") +
    ylab("y") +
    theme_minimal()
p3 = ggplot(data = data.frame(x, y3), aes(x = x, y = y3)) +
    geom_point() +
    ggtitle("(c)") +
    xlab("x") +
    ylab("y") +
    theme_minimal()

grid.arrange(p1, p2, p3, ncol = 3)

# Plot the scatters after the appropriate transformations
p1_t = ggplot(data = data.frame(x, y1), aes(x = x, y = sqrt(y1))) +
    geom_point() +
    ggtitle("(a)") +
    xlab("x") +
    ylab(expression(sqrt(y))) +
    theme_minimal()
p2_t = ggplot(data = data.frame(x, y2), aes(x = x, y = log(y2))) +
    geom_point() +
    ggtitle("(b)") +
    xlab("x") +
    ylab(expression(log(y))) +
    theme_minimal()
p3_t = ggplot(data = data.frame(x, y3), aes(x = x, y = 1/y3)) +
    geom_point() +
    ggtitle("(c)") +
    xlab("x") +
    ylab(expression(frac(1, y))) +
    theme_minimal()

grid.arrange(p1_t, p2_t, p3_t, ncol = 3)
