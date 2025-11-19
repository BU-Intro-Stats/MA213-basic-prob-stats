# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# ---- 1. Use lm to summarize the linear regression on the twins data

twins <- read.table("twins.txt", header = T, sep = "\t")

# fit the simple linear regression
fit <- lm(fosterIQ ~ bioIQ, data = twins)

# ---- 2. Visualize the data and the fitted regression line

b0 = coef(fit)["(Intercept)"]
b1 = coef(fit)["bioIQ"]

# Or, equivalently: 
b0 = coef(fit)[1]
b1 = coef(fit)[2]

p1 = ggplot(twins, aes(x = bioIQ, y = fosterIQ)) +
  geom_point(color = "blue") +
  geom_abline(slope = b1, intercept = b0, color = "red", linewidth = 1) +
  labs(title = "Foster Twins' IQ vs Biological Twins' IQ",
       x = "Biological Twin IQ",
       y = "Foster Twin IQ")
print(p1) 

# ---- 3. Display the summary output
summary_twins <- summary(fit)
print(summary_twins)

# -----------------------------------------------------------------
# ---- 4. Re-calculate the p-value and compare to the summary

SE_b1 <- summary_twins$coefficients[2,2]
dof_b1 <- summary_twins$df[2]

T = b1/SE_b1
print(T)

# H0: beta1=0
# HA: beta1!=0
pval = 2*(1-pt(abs(T),dof_b1))
print(pval)

# ---- 5. Compute one-sided hypothesis tests
# HA: beta1>0
# for a positive slope (probability in the upper tail):
pval_onesided_positive = 1 - pt(T,dof_b1)

# HA: beta1<0
# for a negative slope (probability in the lower tail):
pval_onesided_negative = pt(T,dof_b1)

# Plot the T distribution and the observed T
x <- seq(-12, 12, length.out = 1000)
y <- dt(x, df = dof_b1)
df_t <- data.frame(x = x, y = y)

p2 <- ggplot(df_t, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 1) +
  geom_vline(xintercept = T, color = "red", linewidth = 1) +
  labs(title = paste0("t(", dof_b1, ") distribution with observed T = ", round(T, 3)),
       x = "t", y = "Density") +
  theme_minimal()
print(p2)

# ---- 6. Do a hypothesis test with a nonzero null value
# H0: beta1=0.8
# HA: beta1>0.8

# T = b1-beta0/SE_b1
Tnew = (b1-0.8)/SE_b1
print(Tnew)

pval_nonzero_null = 1 - pt(Tnew,dof_b1)



