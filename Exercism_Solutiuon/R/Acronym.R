get_acronym <- function(string) {
  acronym <- paste0(substring(strsplit(string, " ")[[1]], 1, 1), 
                    collapse = "")
  return(acronym)
}

clean_string <- function(string){
  
  cleaned_string <- string |>
    gsub("-", " ", x = _) |>
    gsub("[[:punct:]]", "", x = _)
  
  return(cleaned_string)
}

acronym <- function(input) {
  clean_string <- clean_string(input)
  acronym <- get_acronym(clean_string)
  return(toupper(acronym))
}


################################################################################

a = "As Soon As-Possible"

a |> 
  gsub("-", " ",x = _) |>
  gsub("[[:punct:]]", "", x = _)

clean_string(a)

paste0(substring(strsplit(a, " ")[[1]], 1, 1), collapse = "")
