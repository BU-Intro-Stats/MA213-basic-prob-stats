create_population <- function(N=10000, p=0.5) {
  pop <- c(rep("support", p*N), rep("not", (1-p)*N))
  pop <- sample(pop)  # need to randomize order of entries
  return(pop)
}
