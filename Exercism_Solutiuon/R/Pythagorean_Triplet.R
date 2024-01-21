# Other methods were hitting the timeout.
# Found one solution from 
# https://exercism.org/tracks/r/exercises/pythagorean-triplet/solutions/nzok
# Tried to derive the formula; I encountered different 'A' limit values, 
# but my answer was still accepted. 
# Working with the following equations:
# A < B < C
# A^2 + B^2 = C^2
# A + B + C = N
# B >= A + 1
# C >= B + 1
# Substituting B and C into A + B + C = N:
# We get A <= (N/3) - 1
# and B <= (N - A - 1) / 2
# Despite the variation in 'A' limits, the solution was accepted.

pythagorean_triplet <- function(n) {
  results <- list()
  a_limit <- (n - 3) %/% 3;
  a <- 1
  while (a <= a_limit) {
    b_limit <- (n - a - 1) %/% 2
    b <- a + 1
    while (b <= b_limit) {
      c <- n - a - b
      if (b < c && a^2 + b^2 == c^2) {
        results[length(results) + 1] <- list(c(a , b, c))
      }
      b <- b + 1
    }
    a <- a + 1
  }
  results
}




################################################################################
library(zeallot)
library(Rcpp)



isSeq <- function(a, b, c) ifelse(a < b && b < c, TRUE, FALSE) 

sum_matches <- function(a, b, c) ifelse(a^2 + b^2 == c^2, TRUE, FALSE)

is_pythagorean <- function(triplet) {
  c(a, b, c) %<-% triplet
  ifelse(isSeq(a, b, c) && sum_matches(a, b, c), TRUE, FALSE)
}

get_triplet <- function(start, end) {
  tripet <- c()
  
  for (a in start:end) {
    for (b in a:end) {
      c <- sqrt(a^2 + b^2) |> as.integer()
      if (is_pythagorean(c(a, b, c)) && c <= end) {
        tripet <- append(tripet, list(c(a, b, c)))
      }
    }
  }
  
  return(tripet)
}

pythagorean_triplet4 <- function(n) {
  triplet <- get_triplet(1, n)
  pythagorean_triplet_value <- triplet[sapply(triplet, function(x) sum(x) == n)]
  return(pythagorean_triplet_value)
}


###############################################################################

isSeq <- function(a, b, c) ifelse(a < b && b < c, TRUE, FALSE) 

sum_matches <- function(a, b, c) ifelse(a^2 + b^2 == c^2, TRUE, FALSE)

is_pythagorean <- function(triplet) {
  a <- triplet[1]
  b <- triplet[2]
  c <- triplet[3]
  ifelse(isSeq(a, b, c) && sum_matches(a, b, c), TRUE, FALSE)
}

cppFunction('
  // Rcpp function to get triplets
  List get_triplet(int start, int end) {
    List triplets;

    for (int a = start; a <= end; a++) {
      for (int b = a; b <= end; b++) {
        int c = (int)std::sqrt(a*a + b*b);
        if (c <= end) {
          triplets.push_back(IntegerVector::create(a, b, c));
        }
      }
    }

    return triplets;
  }
')

pythagorean_triplet1 <- function(n) {
  triplet <- get_triplet(1, n)
  pythagorean_triplet_value <- triplet[sapply(triplet, function(x) sum(x) == n && is_pythagorean(x))]
  return(pythagorean_triplet_value)
}


################################################################################

pythagorean_triplet2 <- function(n) {
  results <- list()
  # A+B+C == n; B >= A+1, C >= B+1, so 3A+2 <= n, A <= (n-2)/3
  A.limit <- (n - 2) %/% 3;
  A <- 1
  while (A <= A.limit) {
    # Now B+C == n-A and C >= B+1 so 2B+1 <= n-A, so B <= (n-A-1)/2
    B.limit <- (n - A - 1) %/% 2
    B <- A+1
    while (B <= B.limit) {
      C <- n - A - B
      if (B < C && A*A + B*B == C*C) {
        results[length(results)+1] <- list(c(A,B,C))
      }
      B <- B+1
    }
    A <- A+1
  }
  results
}




isSeq(1,2,3)
sum_matches(3, 4, 5)
is_pythagorean(c(3, 4, 5))

n = 12
bench::mark(pythagorean_triplet(n), 
            pythagorean_triplet1(n), 
            pythagorean_triplet2(n))
