# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Binomial Distribution ----
# let X1 be the number of successes if you flip a coin 10 times and the 
# probability of a heads is 0.1. Simulate X1 10000 times

# draw 10000 samples from Binomial(10, 0.1)
# WARNING: the "n" argument in rbinom is the number of samples/simulations, 
# *not* the number of trials in Binomial(n,p)
samples1 <- rbinom(n=10000, size=10, prob=0.1) 
head(samples1)
mean(samples1)
sd(samples1)

ggplot(data=data.frame(samples1), aes(x=samples1)) +
  geom_histogram(binwidth=1, boundary=0) +
  xlab("samples")

# let X2 be the number of successes if you flip a coin 100 times and the 
# probability of a heads is 0.1. Simulate X2 10000 times

# draw 10000 samples from Binomial (100, 0.1)
samples2 <- rbinom(n=10000, size=100, prob=0.1) 
head(samples2)
mean(samples2)
sd(samples2)

ggplot(data=data.frame(samples2), aes(x=samples2)) +
  geom_histogram(binwidth=1, boundary=0) +
  xlab("samples")

# let X3 be the number of successes if you flip a coin 300 times and the 
# probability of a heads is 0.1. Simulate X3 10000 times

# draw 10000 samples from Binomial (300, 0.1)
samples3 <- rbinom(n=10000, size=300, prob=0.1) 
head(samples3)
mean(samples3)
sd(samples3)

ggplot(data=data.frame(samples3), aes(x=samples3)) +
  geom_histogram(binwidth=1, boundary=0) +
  xlab("samples")


# ---- 2. Normal approximation ----

mean1 <- 10*0.1  
var1 <- 10*0.1*(1-0.1) 

mean2 <- 100*0.1  
var2 <- 100*0.1*(1-0.1)  

mean3 <- 300*0.1  
var3 <- 300*0.1*(1-0.1)  

# Let's plot the binomial pmfs for each case, and see if they are 
# well-approximated by a normal distribution with the same mean and std dev.
# Recall that for a binomial, E[X] = np and Var(X) = np(1-p)
plot_binomial_and_normal <- function(n,p) {
  x <- 0:n
  # compute the binomial PMF
  pmf <- dbinom(x, n, p)
  
  # compute the mean and standard deviation of the binomial
  mu <- n * p # Mean of a binomial
  sigma <- sqrt(n * p * (1 - p)) # standard deviation of a binomial
  
  # compute the pdf of a Normal with those mean, stdev
  x_norm <- seq(min(x), max(x), length.out = 200)
  norm_pdf <- dnorm(x_norm, mean = mu, sd = sigma)
  
  # save it all in dataframes
  binomial_pmf_df <- data.frame(
    x = x,
    prob = pmf,
    type = "Binomial PMF",
    mu = mu,
    sigma = sigma
  )
  normal_pdf_df <- data.frame(
    x = x_norm,
    prob = norm_pdf,
    type = "Normal PDF",
    mu = mu,
    sigma = sigma
  )
  data = rbind(binomial_pmf_df, normal_pdf_df)
  
  # plot the pmf and pdf on top of each other
  ggplot(data, aes(x = x, y = prob, color = type, fill = type)) +
    geom_bar(
      data = subset(data, type == "Binomial PMF"),
      stat = "identity", position = "identity", alpha = 0.7, color = "darkblue", fill = "darkblue"
    ) +
    geom_line(
      data = subset(data, type == "Normal PDF"),
      size = 1.2, color = "darkred"
    ) +
    labs(title = paste("Binomial PMF vs Normal PDF for Binomial(",n,",",p,")"),
         x = "Number of successes", y = "Probability") +
    annotate("text", x = Inf, y = Inf, label = paste("Matched\nE[X] =", mu, "\nStdDev(X) =",
                                                     round(sigma,2)), 
             hjust = 1.1, vjust = 1.1, size = 5) +
    theme_minimal()
}

plot_binomial_and_normal(10,0.1)
plot_binomial_and_normal(30,0.1)
plot_binomial_and_normal(100,0.1)
plot_binomial_and_normal(300, 0.1)

