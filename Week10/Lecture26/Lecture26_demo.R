# ---- 0. Setup and load libraries, if any ----

# Set the working directory
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))

# Load libraries
library(ggplot2)
library(dplyr)

# Load data
data <- read.csv("diamond.csv")
head(data)

# We just need the carat and 
# ptprice (price normalized by carat points) variables
data <- data %>% select(carat, ptprice)
head(data)


# ---- 1. Computing summary statistics and the point estimate ----

n99 <- length(data[data$carat == 'pt99', 'ptprice'])
n100 <- length(data[data$carat == 'pt100', 'ptprice'])
mean99 <- mean(data[data$carat == 'pt99', 'ptprice'])
mean100 <- mean(data[data$carat == 'pt100', 'ptprice'])
s99 <- sd(data[data$carat == 'pt99', 'ptprice'])
s100 <- sd(data[data$carat == 'pt100', 'ptprice'])

diff_est <- mean99 - mean100
print(diff_est)

# ---- 2. Computing the test statistic and p-value ----

SE <- sqrt((s99**2)/n99 + (s100**2)/n100)
df <- min(n99-1, n100-1)
Tstat <- (diff_est - 0) / SE

pt(Tstat, df=df)

#-------------------------------------------------
# ---- 3. Computing a 90% confidence interval ----

tstar <- qt(p=0.90, df=22)
diff_est + c(-1,1)*tstar*SE
