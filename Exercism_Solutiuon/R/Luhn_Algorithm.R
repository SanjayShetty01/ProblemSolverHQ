
is_valid <- function(input) {
  number <- input |> 
    gsub(" ", "", x = _) |> 
    strsplit("") |>
    unlist()

  if (!all(number %in% as.character(0:9))) return(FALSE)
  
  if (length(number) <= 1) return(FALSE)
  
  number <- as.numeric(number)
  multip <- rep(c(1,2), length.out = length(number)) |> rev()
  number <- number * multip
  number <- ifelse(number > 9, number - 9, number)
  (sum(number) %% 10) == 0
}



###############################################################################

input <- "1"

gsub(" ", "", input)

number <- input |> 
  gsub(" ", "", x = _) |> 
  as.numeric() |> 
  suppressWarnings() 

is.na(number)

is_valid("0")
