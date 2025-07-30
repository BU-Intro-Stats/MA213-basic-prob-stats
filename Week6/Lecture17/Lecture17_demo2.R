# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

# ---- 1. Changing the width of the confidence interval ----

# Back to our Facebook survey data, let's see how the CI changes as we adjust
# the confidence level (and thus the width)

# Generate the data again
N = 850
p_hat = 0.67
survey_results <- c(rep("accurate", p_hat*N), rep("not accurate", (1-p_hat)*N))

# Vary the significance levels
alphas <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.3)  

# What confidence levels do the alphas correspond to?
print(1-alphas/2)

# Compute the critical z-scores
zs = qnorm(1-alphas/2)

# Compute the standard error 
SE <- sqrt(p_hat*(1-p_hat)/N)

# Compute the CIs
lowers <- p_hat - zs*SE
uppers <- p_hat + zs*SE

# For comparison, compute the width of each interval
widths <- uppers - lowers

# Display the results
tab <- matrix(c(alphas, lowers, uppers, widths, 1-alphas/2), nrow=7, ncol=5)
colnames(tab) <- c('alpha', 'CI Lower Limit', 'CI Upper Limit', 'CI Width', 
                   'confidence')
rownames(tab) <- c(1:7)
print(as.table(tab))

