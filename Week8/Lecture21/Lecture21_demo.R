# ---- 0. Setup and load libraries, if any ----
set.seed(213)


# ---- 1. CI for a difference of proportions - northern ice cap survey ----

# What is the null hypothesis for this example?

concern_survey <- matrix(c(69, 454, 36, 226), byrow=TRUE, nrow=2, ncol=2, 
                         dimnames=list(c("A great deal", "Not a great deal"),
                                    c("Duke students", "General Americans")))
concern_survey

p_hat_duke <- 69/(69+36)
p_hat_amer <- 454/(454+226)
p_hat_diff <- p_hat_duke - p_hat_amer
SE_diff <- sqrt((p_hat_duke*(1-p_hat_duke))/(69+36) + (p_hat_amer*(1-p_hat_amer))/(454+226))

alpha <- 0.05
p_hat_diff + c(-1,1)*qnorm(1-alpha/2)*SE_diff

# What can you conclude from this CI?

# Now let's see how the sample size affects our CI...


# ---- 2. Playing with sample size ----

# Same sample size for both groups, and a small but meaningful difference in the 
# proportions - but few samples
n1 <- 25
n2 <- 25
data1 <- rbinom(15, size=n1, prob=0.8)  
data2 <- rbinom(15, size=n2, prob=0.6)  
p_hat1 <- mean(data1/n1)
p_hat2 <- mean(data2/n2)
p_hat_diff <- p_hat1 - p_hat2

# What is the null hypothesis here?

SE_diff <- sqrt((p_hat1*(1-p_hat1)/n1) + (p_hat2*(1-p_hat2))/n2)

alpha <- 0.05
p_hat_diff + c(-1,1)*qnorm(1-alpha/2)*SE_diff

# What can you conclude from this CI?

# Now increase the sample sizes and repeat:

n1 <- 100
n2 <- 100
data1 <- rbinom(15, size=n1, prob=0.8)  
data2 <- rbinom(15, size=n2, prob=0.6)  
p_hat1 <- mean(data1/n1)
p_hat2 <- mean(data2/n2)
p_hat_diff <- p_hat1 - p_hat2
SE_diff <- sqrt((p_hat1*(1-p_hat1)/n1) + (p_hat2*(1-p_hat2))/n2)
alpha <- 0.05
p_hat_diff + c(-1,1)*qnorm(1-alpha/2)*SE_diff

# Have your conclusions changed?

