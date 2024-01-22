get_top_half <- function(letter_count) {
  top_half <- sapply(seq(letter_count), \(x) {
    row_width <- rep(" ", letter_count + (letter_count - 1))
    row_width[letter_count + c(x - 1, 1 - x)] <- LETTERS[x]
    paste(row_width, collapse = "")
  })
  
  return(top_half)
}

diamond <- function(letter) {
  position <- which(LETTERS == letter)
  top_half <- get_top_half(position)
  paste(c(top_half, rev(top_half[-position])), collapse = "\n") 
}


################################################################################

position <- which(LETTERS == "A")

total_length <- position + (position - 1)


get_top_half <- function(letter_count) {
  top_half <- sapply(seq(letter_count), \(x) {
    row_width <- rep(" ", letter_count + (letter_count - 1))
    row_width[letter_count + c(x - 1, 1 - x)] <- LETTERS[x]
    paste(row_width, collapse = "")
  })
  
  return(top_half)
}

top_half <- get_top_half(2)

paste(c(top_half, rev(top_half[-letter_count])), collapse = "\n") 


paste(
  sep = "\n",
  " A ",
  "B B",
  " A "
)


diamond("B")
