# ---- 0. Setup and load libraries, if any ----
set.seed(213)

# ---- 1. Hypothesis Testing Example: Facebook survey ----

N = 850
p_hat = 0.67
survey_results <- c(rep("accurate", p_hat*N), rep("not accurate", (1-p_hat)*N))

# View the data
table(survey_results)

# How would you set up a hypothesis test for this example?

# Recall our confidence interval from last time:
alpha <- 0.05
SE <- sqrt(p_hat*(1-p_hat)/N)
p_hat + c(-1,1)*qnorm(1-alpha/2)*SE

# What can you conclude from the CI?


# ---- 2. Hypothesis test can sometimes give a wrong answer! ----

# Generate some new data where the null hypothesis is actually true (a correct decision would be to not reject H0)

# Sample size: 30
# Test statistic: phat
# True proportion: p=0.5
# Repeat the experiment 100 times
data = rbinom(n=100, size=30, p=0.5) 
p_hats = data/30

# Compute a 95% CI for each simulated experiment
alpha <- 0.05
error_count1 <-0
for (i in 1:100) {
    # compute the CI
    SE <- sqrt(p_hats[i]*(1-p_hats[i])/30)
    CI <- p_hats[i] + c(-1,1)*qnorm(1-alpha/2)*SE

    # does it contain the true value of p=0.5?
    if (CI[1] > 0.5 | CI[2] < 0.5) {
        cat("Experiment", i, ": reject H0 (CI:", round(CI,2), ")\n")
    } else {
        cat("Experiment", i, ": do not reject H0 (CI:", round(CI,2), ")\n")
        error_count2 <- error_count1 + 1
    }   
}
cat("Error Count (Type 1): ",error_count1, "/100")

# Generate some new data where the null hypothesis is actually false (a correct decision would be to reject H0)

# Sample size: 30
# Test statistic: phat
# True proportion: p=0.7
# Repeat the experiment 100 times
data = rbinom(n=100, size=30, p=0.7)
p_hats = data/30

# Compute a 95% CI for each simulated experiment
alpha <- 0.05
error_count2 <-0
for (i in 1:100) {
    # compute the CI
    SE <- sqrt(p_hats[i]*(1-p_hats[i])/30)
    CI <- p_hats[i] + c(-1,1)*qnorm(1-alpha/2)*SE

    # does it contain the null value of p=0.5?
    if (CI[1] > 0.5 | CI[2] < 0.5){
        cat("Experiment", i, ": reject H0 (CI:", round(CI,2), ")\n")
        error_count2 <- error_count2 + 1
    } else {
        cat("Experiment", i, ": do not reject H0 (CI:", round(CI,2), ")\n")
    }
}
cat("Error Count (Type 2): ",error_count2, "/100")    
