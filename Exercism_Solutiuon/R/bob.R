bob <- function(input) {
  # Remove leading and trailing whitespaces
  input <- trimws(input)
  
  if (nchar(input) == 0) {
    return("Fine. Be that way!")
  } else if (grepl("[A-Z]", input) && toupper(input) == input) {
    if (substr(input, nchar(input), nchar(input)) == "?") {
      return("Calm down, I know what I'm doing!")
    } else {
      return("Whoa, chill out!")
    }
  } else if (substr(input, nchar(input), nchar(input)) == "?") {
    return("Sure.")
  } else {
    return("Whatever.")
  }
}


#################################################################################

input <- "Does this cryogenic chamber make me look fat?"

bob(input)


substr(input, nchar(input), nchar(input))


my_string <- "Hello, World!"

# Get the last character
last_character <- substr(my_string, nchar(my_string), nchar(my_string))

# Print the result
print(last_character)
