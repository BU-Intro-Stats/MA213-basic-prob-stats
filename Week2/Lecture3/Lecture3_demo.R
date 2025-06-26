# ---- 0. Setup and load libraries, if any ----

library(ggplot2)    # load the graphing library
library(openintro)  # load the OpenIntro package, for source data

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Load data ----

data(loan50)  # loan50 dataset from the openintro library
head(loan50)

# ---- 2. Plot histograms ----

# What does this histogram of the 'loan amount' variable tell you?
ggplot(data=loan50, aes(x=loan_amount)) +
  geom_histogram(bins=10, col='gray')
  #geom_vline(aes(xintercept=mean(loan_amount)), col='blue')

# TODO: like lecture slides 12-17 -> generate data to look like this?
# also incorporate Edfinity quiz answers

