library(openintro)

d <- loans_full_schema

###
# figure out the pair of continuous variables in d that have highest correlation
num_vars <- names(d)[sapply(d, is.numeric)]
if (length(num_vars) < 2) stop("Not enough numeric variables to compute correlations.")

cmat <- cor(d[, num_vars], use = "pairwise.complete.obs")
abs_cmat <- abs(cmat)
diag(abs_cmat) <- 0  # ignore self-correlation
abs_cmat[abs_cmat > 0.99] <- 0  # ignore correlations of 1

idx <- which(abs_cmat == max(abs_cmat, na.rm = TRUE), arr.ind = TRUE)[1, , drop = TRUE]
var1 <- rownames(abs_cmat)[idx[1]]
var2 <- colnames(abs_cmat)[idx[2]]
max_corr <- cmat[var1, var2]

cat(sprintf("Highest absolute correlation: %s and %s = %.4f\n", var1, var2, max_corr))
# return as a small data.frame if needed
highest_cor_pairs <- data.frame(var1 = var1, var2 = var2, correlation = max_corr, stringsAsFactors = FALSE)
highest_cor_pairs

###

d$credit_util <- round(ifelse(d$total_credit_limit == 0, 0,
                              d$total_credit_utilized / d$total_credit_limit), 4)
d$income_ver <- ifelse(d$verified_income == "Verified", "verified",
                       ifelse(d$verified_income == "Not Verified", "not", "source_only"))
d$bankruptcy <- d$public_record_bankrupt
d$credit_checks <- d$inquiries_last_12m
d$issued <- gsub("-", "", d$issue_month, fixed = TRUE)
these <- d$annual_income %in% 0:1
d$debt_to_income[these] <- d$total_credit_utilized[these] /
  d$annual_income_joint[these]
d$monthly_payment <- d$installment


###
library(GGally)
library(ggplot2)

vars <- c(
  "interest_rate",
  "debt_to_income",
  "credit_util",
  "bankruptcy",
  "term",
  "credit_checks",
  "loan_amount",
  "monthly_payment")

diag_labeller <- function(data, mapping, ...) {
  var <- as_label(mapping$x)
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = var, size = 6) +
    theme_void()
}

ggpairs(
  data   = d[, vars],
  columns = 1:length(vars),
  upper = list(continuous = wrap("points", alpha = 0.7, size = 1.5)),
  lower = list(continuous = wrap("cor", size = 8)),  # big correlations
  diag  = list(continuous = diag_labeller)
)


###
keep <- c(
  "interest_rate",
  "income_ver",
  "debt_to_income",
  "credit_util",
  "bankruptcy",
  "term",
  "issued",
  "credit_checks")
d <- d[keep]

# save as csv
write.csv(d, file = "Loans.csv", row.names = FALSE)



