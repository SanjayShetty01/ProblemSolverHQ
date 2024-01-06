is_isogram <- function(word) {
  word <- gsub("[^[:alpha:]]", "", tolower(word), perl = TRUE)
  anyDuplicated(strsplit(word, "")[[1]]) == 0
}



##############################################################################

is_isogram("hi")

