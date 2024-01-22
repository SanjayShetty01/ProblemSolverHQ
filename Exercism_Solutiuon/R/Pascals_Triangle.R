pascals_triangle <- function(n) {
  stopifnot("ERROR" = n >= 0)
  
  if (n == 0) list() 
  else lapply(0:(n - 1), \(x) choose(x, 0:x))
}



###############################################################################

n = 2
pascals_triangle(0)
