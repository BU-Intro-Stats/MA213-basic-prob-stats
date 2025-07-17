### Hypothetical Population
pop <- c(rep("Yes", 1500000), rep("No", 300000))
N <- length(pop)
true_prop = sum(pop == "Yes") / N
N # population size
true_prop # true proportion



## Step 1 
## make a function to sample it 
## (possibly) input : n, output : one row of the dataframe
n = 150
sample_data = sample(pop, n)
sample_data
prop_est = sum(sample_data=="Yes")/length(sample_data) 
prop_est
Lower_bound = p_hat - 1.96 * SE
Upper_bound = p_hat + 1.96 * SE
CI = c(Lower_bound, Upper_bound)
CI
checker = CI[1] <= true_prop & true_prop <= CI[2]

row_out <- data.frame(prop_est, lower = CI[1], upper = CI[2], checker)
row_out


one_CI <- function(n){
  sample_data = sample(pop, n)
  prop_est = sum(sample_data=="Yes")/length(sample_data) 
  SE = sqrt( prop_est * (1-prop_est) /n)
  Lower_bound = prop_est - 1.96 * SE
  Upper_bound = prop_est + 1.96 * SE
  CI = c(Lower_bound, Upper_bound)
  checker = CI[1] <= true_prop & true_prop <= CI[2]
  row_out <- data.frame(prop_est, lower = CI[1], upper = CI[2], checker)
  row_out
}


## Step 2 
## Using for-loop, construct a simulation dataframe with row size K=1000
K = 1000
data_out = data.frame()
for (k in 1:K){
  data_out = rbind( data_out, one_CI(150))
  
}
head(data_out)



## Step 3
## How many intervals cover the true_prop ? What percentage? 
dim(data_out)
sum(data_out[4])/1000

