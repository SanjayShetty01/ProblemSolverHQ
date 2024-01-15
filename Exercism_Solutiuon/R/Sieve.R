is_prime <- function(x) {
  if (x <= 1) return(FALSE)
  if (x == 2) return(TRUE)
  for (i in 2:sqrt(x)) {
    if (x %% i == 0) return(FALSE)
  }
  return(TRUE)
}

get_primes_up_to_n <- function(n) {
  if (n == 1) return(NULL)
  primes <- c()
  
  for (i in 2:n) {
    if (is_prime(i))  primes <- c(primes, i)
  }
  
  return(primes)
}

sieve <- function(limit) {
  get_primes_up_to_n(limit)
}



################################################################################

is_prime(1)

sieve(1)
