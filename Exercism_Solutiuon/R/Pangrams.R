is_pangram <- function(input) {
  identical(letters,sort(unique(unlist(strsplit(gsub("[^a-z]","",
                                                     tolower(input)), split = ""))))) 
}


is_pangram <- function(input) {
  cleaned_input <- input |> 
    tolower() |>
    (\(x) gsub("[^a-z]", "", x))() |>
    strsplit(split = "") |>
    unlist() |>
    unique()
  
  return(length(cleaned_input) == 26)
}


###############################################################################
is_pangram("The quick brown fox jumps over the lazy dog.")


input = "The quick brown fox jumps over the lazy dog."
