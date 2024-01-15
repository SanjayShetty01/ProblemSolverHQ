# Determine whether the number is valid.
luhn_algorithm <- function(number){
  
}

is_valid <- function(input) {
  number <- input |> 
    gsub(" ", "", x = _) |> 
    as.numeric() |> 
    suppressWarnings() 
  
  if (is.na(number) || (number == 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}



###############################################################################

input <- "1"

gsub(" ", "", input)

number <- input |> 
  gsub(" ", "", x = _) |> 
  as.numeric() |> 
  suppressWarnings() 

is.na(number)

