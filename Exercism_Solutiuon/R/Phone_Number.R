extract_number <- function(number_string) {
  number <- gsub("[^0-9]", "", number_string)
  return(number)
}

get_us_phone_number <- function(number) {
  if (grepl("^[2-9]\\d{2}[2-9]\\d{6}$", number)) {
    return(number)
  } else if (startsWith(number, "1") && nchar(number) == 11) {
    return(substr(number, 2, nchar(number)))
  } else {
    return(NULL)
  }
}


parse_phone_number <- function(number_string) {
  number <- extract_number(number_string)
  us_phone_number <- get_us_phone_number(number) |>
  return(us_phone_number)
}

############################################################################## 
number = "+ 1 613-995-0253"

l = (gsub("[^0-9]", "", number))

nchar(l)

number |> startsWith("1")
?startsWith
