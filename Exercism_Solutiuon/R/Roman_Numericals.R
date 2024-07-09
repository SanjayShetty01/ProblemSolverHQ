roman <- function(arabic) {
  if (!is.numeric(arabic) || arabic <= 0 || arabic > 3999) {
    stop("Input must be a number between 1 and 3999")
  }

  values <- c(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
  numerals <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")

  result <- ""
  for (i in seq_along(values)) {
    while (arabic >= values[i]) {
      result <- paste0(result, numerals[i])
      arabic <- arabic - values[i]
    }
  }
  
  return(result)
}