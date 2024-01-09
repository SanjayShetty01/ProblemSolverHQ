uncover_string <- function(text){
  return(unlist(strsplit(text, "")))
}

hamming <- function(strand1, strand2) {
  stopifnot("Unequal Length" = nchar(strand1) == nchar(strand2))
  strand1_split = uncover_string(strand1)
  strand2_split = uncover_string(strand2)
  sum(strand1_split != strand2_split)
}



###############################################################################

a = "A"
b = "A"

a_1 = uncover_string(a)
b_1 = uncover_string(b)

hamming(a_1, b_1)


mapply(hamming, )