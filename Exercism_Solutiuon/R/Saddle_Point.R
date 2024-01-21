saddle_point <- function(input) {
  max_row = apply(input, 1, max)
  min_row  = apply(input, 2, min)
  positions = outer(max_row, min_row, FUN = "==")
  data.frame(which(positions, arr.ind = T))
}


################################################################################
g = matrix(c(9, 8, 7, 5, 3, 2, 6, 6, 7), ncol = 3, nrow = 3, byrow = TRUE)

data.frame(row = 2, col = 1)

g

l = apply(g, 2, min) 

points = max(l)

which.min(g)

p = max(l)


k = matrix(c(4, 5, 4, 3, 5, 5, 1, 5, 4), nrow = 3, ncol = 3, byrow = TRUE)

m = apply(k, 2, min)

n = max(m)

n

k[k == n]

which(g == p, arr.ind = T)


o = apply(k, 2,  min)

o

max_row = apply(g, 1, max)

min_row  = apply(g, 2, min)

v = outer(max_row, min_row, FUN = "==")

data.frame(which(v, arr.ind = T))
max_row %o% min_row 
