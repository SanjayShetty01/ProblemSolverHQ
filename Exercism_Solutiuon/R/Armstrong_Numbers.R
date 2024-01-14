get_armstrong_number <- function(n) {
  summed_N <- n |>
    as.character() |>
    strsplit(x = _, split = "") |>
    unlist() |>
    as.numeric() |>
    (\(x) x^(nchar(n)))() |>
    sum()
  
  return(summed_N)  
}

is_armstrong_number <- function(n) {
  summed_N <- get_armstrong_number(n)
  
  ifelse(n == summed_N, T, F)
}



################################################################################

nchar(100)

sapply(as.numeric((strsplit(as.character(22), "")[[1]])), \(x) x^nchar(2))

n = 153
as.numeric((strsplit(as.character(n), "")[[1]])) |> 
  (\(x) x^(nchar(n)))() |>
  sum()


as.numeric((strsplit(as.character(n), "")[[1]])) |> 
  (\(x) x^(nchar(n)))() |>
  sum()

n |>
  as.character() |>
  strsplit(x = _, split = "") |>
  unlist() |>
  as.numeric() |>
  (\(x) x^(nchar(n)))() |>
  sum()
