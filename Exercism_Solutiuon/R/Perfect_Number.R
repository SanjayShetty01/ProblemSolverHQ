get_factors <- function(number) {
  return(which(number %% 1:(number - 1) == 0))
}

whichNumber <- function(number, sum){
  if (sum < number || number == 1) {
    return("deficient")
  } 
  else if (sum == number) {
    return("perfect")
  } else {
    return("abundant")
  }
}

number_type <- function(n){
  stopifnot(n > 0)
  factors <- get_factors(n)
  summed_value <- sum(factors)
  return(whichNumber(n, summed_value))
}



################################################################################

number_type(1)

get_factors(1)
