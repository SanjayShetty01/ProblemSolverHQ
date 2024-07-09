rebase <- function(from_base, digits, to_base) {
  validate_inputs <- function(from_base, digits, to_base) {
    if (from_base < 2) stop("input base must be >= 2")
    if (to_base < 2) stop("output base must be >= 2")
    if (length(digits) == 0) return()  
    if (any(digits < 0) || any(digits >= from_base)) {
      stop("all digits must satisfy 0 <= d < input base")
    }
  }

  digits_to_decimal <- function(from_base, digits) {
    decimal_value <- 0
    power <- length(digits) - 1
    for (digit in digits) {
      decimal_value <- decimal_value + digit * (from_base ^ power)
      power <- power - 1
    }
    decimal_value
  }


  decimal_to_digits <- function(decimal, to_base) {
    if (decimal == 0) return(c(0))
    digits <- integer()
    while (decimal > 0) {
      digits <- c(decimal %% to_base, digits)
      decimal <- decimal %/% to_base
    }
    digits
  }

  validate_inputs(from_base, digits, to_base)

  if (length(digits) == 0) return(0)
  
  decimal_value <- digits_to_decimal(from_base, digits)
  
  result_digits <- decimal_to_digits(decimal_value, to_base)
  
  result_digits
}
