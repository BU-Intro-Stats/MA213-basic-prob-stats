set.seed(42)

generate_data <- function() {
  x1 <- rchisq(65, 6)
  x2 <- c(rchisq(22, 5.8), rnorm(40, 16.5, 2))
  x3 <- c(rchisq(20, 3), rnorm(35, 12), rnorm(42, 18, 1.5))
  x4 <- runif(100,0,20)
  
  return(list("data1" = as.data.frame(x1), "data2" = as.data.frame(x2), 
              "data3" = as.data.frame(x3), "data4" = as.data.frame(x4)))
}