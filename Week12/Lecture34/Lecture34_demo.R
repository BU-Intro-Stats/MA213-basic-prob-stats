# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)

# ---- 1. Plot a scatter plot of log(debt_to_income) vs credit_util ----
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

ggplot(loans, aes(x = log(credit_util), y = log(debt_to_income))) +
  geom_point() +
  labs(x = "Credit Utilization Ratio", y = "Log of Debt to Income Ratio",
       title = "Scatter Plot of Debt to Income vs Credit Utilization") +
  theme_minimal()