create_population <- function(N=10000, p=0.5) {
  pop <- c(rep("support", p*N), rep("not", (1-p)*N))
  pop <- sample(pop)  # need to randomize order of entries
  return(pop)
}

sample_get_phat_fn <- function(population, n) {
  sampled_entries <- sample(population, size = n)
  phat <- sum(sampled_entries == "support") / n
  return(phat)
}

create_comparison_data <- function(simulation, density, K=1000) {
  values <- c(simulation, density)
  source <- c(rep("Simulation", K), rep("Normal", K)) 
  return(data.frame(values, source))
}
