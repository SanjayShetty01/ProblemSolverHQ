prime_factors <- function(n) {
  factors <- c()
  divisor <- 2
  
  while (n > 1) {
    while (n %% divisor == 0) {
      factors <- c(factors, divisor)
      n <- n / divisor
    }
    divisor <- divisor + 1
  }
  
  return(factors)
}



################################################################################

prime_factors(100)
