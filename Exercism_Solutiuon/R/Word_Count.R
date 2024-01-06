word_count <- function(input) {
    cleaned <- tolower(input) |> 
      strsplit(split = " ") |> 
      unlist() |>
      (\(x) gsub("[^a-z0-9']+", " ", x))() |>
      (\(x) gsub("^'|'$", "", x))()
    
    cleaned <- strsplit(cleaned, split = " ") |>
      unlist()
    as.list(table(cleaned[cleaned != ""]))
}



###############################################################################

input = "car : carpet as java : javascript!!&@$%^&"
word_count("'First: don't laugh. Then: don't cry. You're getting it.'")

word_count("Joe can't tell between 'large' and large.")

word_count("one,two,three")

word_count("car : carpet as java : javascript!!&@$%^&")

y = "Joe can't tell between 'large' and large."
x = "'First: don't laugh. Then: don't cry. You're getting it.'"

gsub("^'|'$", "", x)
gsub("^'|'$", "", y)

sapply(y, gsub, pattern = "^'|'$", replacement = "", USE.NAMES = F)
