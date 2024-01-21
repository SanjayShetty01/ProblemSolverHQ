triangle <- function(x, y, z) {
  isTriangle <- c(x + y < z,
                  x + z < y,
                  y + z < x,
                  x == 0,
                  y == 0,
                  z == 0)

  stopifnot("error" != any(isTriangle))
  
  
  unique_sides <- length(unique(c(x, y, z)))
  triangle_class <- switch(unique_sides, 
                           c("equilateral", "isosceles"),
                           "isosceles",
                           "scalene")
  
  structure(list(x = x, y = y, z = z), class = triangle_class)
}