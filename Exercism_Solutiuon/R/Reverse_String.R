reverse <- function(text) {
  text_reversed <- ""
  while (nchar(text) > 0) {
    text_reversed <- paste0(text_reversed, substr(text, nchar(text), nchar(text)))
    text <- substr(text, 1, nchar(text) - 1)
  }
  return(text_reversed)
}
