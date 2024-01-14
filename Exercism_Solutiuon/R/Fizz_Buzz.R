isFizzBuzz <- function(number){
  if (number %% 3 == 0 && number %% 5 == 0 ) {
    return("Fizz Buzz")
  } else if (number %% 3 == 0) {
    return("Fizz")
  } else if (number %% 5 == 0) {
    return("Buzz")
  } else {
    return(as.character(number))
  }
}


fizz_buzz <- function(n) {
  n_vector <- 1:n
  sapply(n_vector, isFizzBuzz)
}



################################################################################


1:10 |> as.character()
