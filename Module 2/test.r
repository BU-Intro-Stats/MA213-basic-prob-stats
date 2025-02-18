n = 10000

simdata <- replicate(n, {
  dieRoll <- sample(1:6, 2, TRUE)
  sum(dieRoll)
})


sim_table <- table(simdata)
names(sim_table) # it needs to be numeric value
x = as.numeric( names(sim_table) ) 
x
# n = sum(sim_table)
prob_est = sim_table/n
prob_est
E_X = sum(prob_est * x)
E_X


#Theoretical /mu, /sigma^2

# /mu
# how many ways
Number_ways <- c(1,2,3,4,5,6,5,4,3,2,1)
prob <- (1/36) * Number_ways # theoretical prob

prob
prob_est

mu = sum(x * prob)
mu
sigma_2 = sum((x-mu)^2*prob)
sigma_2


sim_fn <- function(n=1000){
  sim_data <- replicate(n,{
    sum_of_two <- sum(sample(1:6, 2, TRUE))
    sum_of_two
  })
  
  sim_table<-table(sim_data)
  x <- 2:12 # possible values
  prob_est = sim_table/n
  mu_hat = sum(prob_est * x)
  sigma_2_hat = sum( (x-mu_hat)^2*prob_est )
  
  return(c(mu_hat, sigma_2_hat))
}


sim_fn(1000)
