# this is a stub function that takes a natural_number
# and should return the difference-of-squares as described
# in the README.md

square_of_sum <- function(number){
  return(sum(1:number)^2)
}

sum_of_square <- function(number){
  number_squared <- sapply(1:number, \(x) x^2)
  return(sum(number_squared))
}

difference_of_squares <- function(natural_number) {
  return(square_of_sum(natural_number) - sum_of_square(natural_number))
}



# optimized solution
square_of_sum1 <- function(number){
  return((number * (number + 1) / 2)^2)
}

sum_of_square1 <- function(number){
  return((number * (number + 1) * (2*number + 1))/6)
}

difference_of_squares1 <- function(natural_number) {
  return(square_of_sum(natural_number) - sum_of_square(natural_number))
}


