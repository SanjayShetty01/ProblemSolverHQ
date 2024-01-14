isEven = function(number) {
  ifelse(number %% 2 == 0, TRUE, FALSE)
}

collatz_calculation <- function(number) {
  if (isEven(number)) {
    return(number / 2)
  } else {
    return((3 * number) + 1)
  }
}

collatz_step_counter_step <- function(num) {
  stopifnot(num > 0)
  
  answer = num
  counter = 0
  
  while (answer > 1) {  
    answer = collatz_calculation(answer)
    counter = counter + 1
  }
  counter
}

collatz_step_counter <- Vectorize(collatz_step_counter_step)

################################################################################
collatz_step_counter(c(1,2))

