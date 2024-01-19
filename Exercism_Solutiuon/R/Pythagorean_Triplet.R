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

pythagorean_triplet <- function(n) {
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

pythagorean_triplet <- function(n) {
  triplet <- get_triplet(1, n)
  pythagorean_triplet_value <- triplet[sapply(triplet, function(x) sum(x) == n && is_pythagorean(x))]
  return(pythagorean_triplet_value)
}


################################################################################


isSeq(1,2,3)
sum_matches(3, 4, 5)
is_pythagorean(c(3, 4, 5))

pythagorean_triplet(1000)
