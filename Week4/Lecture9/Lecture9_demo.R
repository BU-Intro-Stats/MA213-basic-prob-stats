# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Continuous distributions: Normal ----

samples_norm_500 <- rnorm(500)
samples <- data.frame(samples=samples_norm_500)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.3) +
  geom_density(aes(y=..density..)) +
  scale_x_continuous(breaks=c(-3:3)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Normal Distribution")


# ---- 1a. What happens when we increase the sample size? ----
samples_norm_10000 <- rnorm(10000)
samples <- data.frame(samples=samples_norm_10000)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.3) +
  geom_density(aes(y=..density..)) +
  scale_x_continuous(breaks=c(-3:3)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Normal Distribution")

  
# ---- 1b. What happens when we decrease the bin size? ----

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6, binwidth=0.1) +
  geom_density(aes(y=..density..)) +
  scale_x_continuous(breaks=c(-3:3)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Normal Distribution")


# ---- 2. Continuous distributions: Exponential ----

samples_exp_500 <- rexp(500)
samples <- data.frame(samples=samples_exp_500)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  geom_density(aes(y=..density..)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Exponential Distribution")

samples_exp_10000 <- rexp(10000)
samples <- data.frame(samples=samples_exp_10000)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  geom_density(aes(y=..density..)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Exponential Distribution")


# ---- 3. Continuous distributions: Uniform ----

samples_uni_500 <- runif(500)
samples <- data.frame(samples=samples_uni_500)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  geom_density(aes(y=..density..)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 500 Samples from Uniform Distribution")

samples_uni_10000 <- runif(10000)
samples <- data.frame(samples=samples_uni_10000)

ggplot(samples, aes(x=samples)) +
  geom_histogram(aes(y=..density..), alpha=0.6) +
  geom_density(aes(y=..density..)) +
  ylab("Density") +
  xlab("Samples") +
  ggtitle("Histogram of 10000 Samples from Uniform Distribution")
