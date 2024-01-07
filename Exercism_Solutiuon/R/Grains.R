square <- function(n) {
  if (n > 0 && n < 65) {
    return(2^(n - 1))
  } else {
    stop("Please enter positive number")
  }
}

total <- function() {
  sum <- sapply(1:64, square) |> sum()
  return(sum)
}
