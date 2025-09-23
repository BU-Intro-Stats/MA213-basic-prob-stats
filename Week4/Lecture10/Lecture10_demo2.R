# ---- 0. Setup and load libraries, if any ----

library(ggplot2)  # load the graphing library

if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))  # set working directory


# ---- 1. Standard Normal distribution ----

x <- seq(-8,8,0.1)  # xaxis values

ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue') +
  scale_x_continuous(breaks=c(-8:8))


# What happens if we change the standard deviation, but keep the mean the same?


# ---- 2. Changing the sd ----

ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue') +
  stat_function(fun=dnorm, args=list(mean=0, sd=5), color='dark green') +
  scale_x_continuous(breaks=c(-8:8))


# ---- 3. Changing the mean ----

ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue') +
  stat_function(fun=dnorm, args=list(mean=0, sd=5), color='dark green') +
  stat_function(fun=dnorm, args=list(mean=-4, sd=1), color='orange') +
  scale_x_continuous(breaks=c(-8:8))
  
  
# ---- 4. Changing both the mean and sd ----
  
ggplot(data.frame(x), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue') +
  stat_function(fun=dnorm, args=list(mean=0, sd=5), color='dark green') +
  stat_function(fun=dnorm, args=list(mean=-4, sd=1), color='orange') + 
  stat_function(fun=dnorm, args=list(mean=3, sd=2), color='pink') +
  scale_x_continuous(breaks=c(-8:8))
