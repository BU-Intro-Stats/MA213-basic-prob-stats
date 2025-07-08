# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Generate data ----

set.seed(8535)

# Our dataset consists of 24 male and 24 female employees (48 total)
gender <- c(rep('male', 24), rep('female', 24))

# Of these, 21 male employees were promoted (and 3 not promoted), while
# 14 female employees were promoted (and 10 not promoted)
outcome <- c(rep(c('promoted', 'not promoted'), c(21, 3)), rep(c('promoted', 'not promoted'), c(14, 10)))

# Create a data table
data <- data.frame(unlist(gender), unlist(outcome))
colnames(data)[1] <- "gender"
colnames(data)[2] <- "outcome"
head(data)

# What are the null and alternative hypotheses in this study?

# ---- 2. Set up a simulation ----

# TODO: rework into the style of lab 4
nsim  = 100
n     = length(gender)
group = gender
var1  = outcome
success = "promoted"
sim   = matrix(NA, nrow = n, ncol = nsim)
n1    = n2 = 24

statistic <- function(var1, group){	
  t1 <- var1 == success & group == levels(as.factor(group))[1]
  t2 <- var1 == success & group == levels(as.factor(group))[2]
  sum(t1)/n1 - sum(t2)/n2 
}

for(i in 1:nsim){
  sim[,i] = sample(group, n, replace = FALSE)
}

sim_dist = apply(sim, 2, statistic, var1 = outcome)
diffs    = sim_dist
pval     = sum(diffs >= 0.29) / nsim
values  <- table(sim_dist)


X <- c()
Y <- c()
for(i in 1:length(diffs)){
  x   <- diffs[i]
  rec <- sum(sim_dist == x)
  X   <- append(X, rep(x, rec))
  Y   <- append(Y, 1:rec)
}


# ---- 3. Plot simulation results ----

# TODO: rewrite with ggplot
#myPDF('discRandDotPlot.pdf', mar=c(3.4, 0.5, 0.5, 0.5), mgp=c(2.35,0.6,0))
#plot(X, Y, xlim=range(diffs)+c(-1,1)*sd(diffs)/4, xlab = "Difference in promotion rates", axes = FALSE, ylim=c(0,max(Y)), col=COL[1], cex=0.8, pch=20)
#axis(1, at = seq(-0.4,0.4,0.1), labels = c(-0.4,"",-0.2,"",0,"",0.2,"",0.4))
#abline(h=0)
