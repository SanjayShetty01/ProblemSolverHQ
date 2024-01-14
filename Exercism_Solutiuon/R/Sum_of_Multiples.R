get_multiples <- function(number, limit){
  if (number < limit) {
  multiples <- seq(number, limit - 1, by = number)
  } else{
    return(0)
  }
  return(multiples)
}

sum_of_multiples <- function(factors, limit) {
  multiples <- sapply(factors, get_multiples, limit = limit)
  return(sum(unique(unlist(multiples))))
}


################################################################################

factor = 3
limit = 20

seq(factor, limit - 1, by = factor) 

g = seq(factor, limit - 1, by = factor) 

sum(g) + q

q = sum(g)

get_multiples(c(3,5), 1)


sapply(c(3,5), get_multiples, limit = 100) |>
  unlist() |>
  sum()

sum_of_multiples(c(3,5), 100)
