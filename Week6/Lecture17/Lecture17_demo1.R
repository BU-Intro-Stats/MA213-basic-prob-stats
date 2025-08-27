# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library


# ---- 1. Confidence intervals for a proportion ----
N = 850
p_hat = 0.67
survey_results <- c(rep("accurate", p_hat*N), rep("not accurate", (1-p_hat)*N))

# View the data
table(survey_results)

# Set the CI significance level
alpha <- 0.05

# Compute the standard error 
SE <- sqrt(p_hat*(1-p_hat)/N)

# Compute the CI
p_hat + c(-1,1)*1.96*SE


# ---- 2. Simulate repeating the survey many times ----
# If the true population proportion is 0.7, how often will a confidence interval
# computed like this contain the true value?
source("Lecture17_CIShinyApp.R")
shinyApp(ui = ui, server = server)

