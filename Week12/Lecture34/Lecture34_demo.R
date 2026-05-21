# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# Loan data from Lending Club
# This dataset represents thousands of loans made through the Lending Club
# platform, which is a platform that allows individuals to lend to other
# individuals. Of course, not all loans are created equal. Someone who is a
# essentially a sure bet to pay back a loan will have an easier time getting a
# loan with a low interest rate than someone who appears to be riskier. And for
# people who are very risky? They may not even get a loan offer, or they may not
# have accepted the loan offer due to a high interest rate. It is important to
# keep that last part in mind, since this dataset only represents loans actually
# made, i.e. do not mistake this data for loan applications!

loans <- read.csv("Loans.csv")

# ---- 1. Investigate how interest rates depend on prior bankruptcy ----

# Box plot of interest_rates by prior bankruptcy
ggplot(loans, aes(factor(bankruptcy), interest_rate)) +
  geom_boxplot(fill = "#4C78A8") +
  labs(title = "Interest rates by prior bankruptcy status",
       x = "Bankruptcy",
       y = "Interest rate") +
  theme_minimal()

# Regression
# Response variable: interest_rate
# Predictors: bankruptcy (indicator)
fit1 <- lm(interest_rate ~ bankruptcy, data = loans)
summary(fit1)

b0 = coef(fit1)[["(Intercept)"]]
b1 = coef(fit1)[["bankruptcy"]]

# What is the predicted interest rate for borrowers with no prior bankruptcy?
pred0 <- b0+0*b1
print(pred0)

# What is the predicted interest rate for borrowers with a prior bankruptcy?
pred1 <- b0+1*b1
print(pred1)


# ---- 2. Investigate how interest rates depend on the income verification level ----

# Box plot of interest_rates by income_ver (verified, source_only, not)
ggplot(loans, aes(income_ver, interest_rate)) +
    geom_boxplot(fill = "#4C78A8") +
    labs(title = "Interest rates by income verification status",
             x = "Income verification",
             y = "Interest rate") +
    theme_minimal()

# Regression
# Response variable: interest_rate
# Predictors: income_ver (categorical)
fit2 <- lm(interest_rate ~ income_ver, data = loans)
summary(fit2)

b0 = coef(fit2)[["(Intercept)"]]
b1 = coef(fit2)[["income_versource_only"]]
b2 = coef(fit2)[["income_ververified"]]

# -----------------------------------------------------------------
# What is the predicted interest rate for borrowers with verified income?
pred2 = b0+0*b1+1*b2
# or
pred2 = b0+b2

# What is the predicted interest rate for borrowers with source only verification?
pred1 = b0+1*b1+0*b2
# or
pred1 = b0+b1

# What is the predicted interest rate for borrowers with no income verification?
pred0 = b0+0*b1+0*b2
# or
pred0 = b0

# -----------------------------------------------------------------
# ---- 3. Fit a big model of interest rates ----
fit3 <- lm(interest_rate ~ 
             income_ver +
             debt_to_income +
             bankruptcy +
             term,
           data = loans)
summary(fit3)

