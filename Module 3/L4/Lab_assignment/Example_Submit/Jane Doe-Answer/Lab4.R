# Please name your submission as lab4.R
# 
# execute this set.seed(213)
set.seed(213)
# 
# 1. Produce population data and assign it population object. ----

population <- rbinom(100000, size=1, prob=0.1)


# 2. Obtain mean and standard deviation of the population and assign them to mean_pop and sd_pop accordingly. ----
mean_pop <- mean(population)
sd_pop <- sd(population)
 

# 3. Get the sample mean data for $n=10$ with $k=100$ iterations. Assign the mean and standard deviation of the sample mean data to mean_sample and sd_sample. ----
n <- 10
sample_get_xbar_fn <- function(n){
  sampled_entries <- sample(population, size = n)
  xbar <- sum(sampled_entries) / n
  return(xbar)
}


K = 1000 # simulation size is K=10000

Sample_proportions <- replicate(K, sample_get_xbar_fn(n))

mean_sample <- mean(Sample_proportions)
sd_sample <- sd(Sample_proportions)




# 4. Do one more time with the sample mean data for $n=50$ with $k=100$ iterations. Assign the mean and standard deviation of the sample mean data to mean_sample2 and sd_sample2. ----

n <- 50

K = 1000 # simulation size is K=10000

Sample_proportions <- replicate(K, sample_get_xbar_fn(n))

mean_sample2 <- mean(Sample_proportions)
sd_sample2 <- sd(Sample_proportions)

