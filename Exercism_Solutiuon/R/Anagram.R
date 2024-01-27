check_same_word <- function(word1, word2) {
  tolower(word1) == tolower(word2)
}

sort_letters <- function(word) {
  paste(sort(strsplit(tolower(word), "", fixed = TRUE)[[1]]), collapse = "") 
}

check_matches <- function(word1, word2){
  if (check_same_word(word1, word2)) {
    return(FALSE)
  }
  word1_sorted <- sort_letters(word1)
  word2_sortted <- sort_letters(word2)
  return(word1_sorted == word2_sortted)
}

anagram <- function(subject, candidates) {
  any_matches <- candidates[sapply(candidates, check_matches, word2 = subject)]
  
  if (length(any_matches) > 0) {
    return(any_matches)
  }
  return(c())
}









################################################################################

comp <- function(s1, s2){         
  in1 = letters %in% strsplit(tolower(s1), "")[[1]]
  in2 = letters %in% strsplit(tolower(s2), "")[[1]]
  sum(in1 & in2)/sum(in1)
}


comp("engle", "galea")

s1 = "engle"
s2 = "galea"

paste(sort(strsplit(s2, "", fixed = TRUE)[[1]]), collapse = "")

strsplit(tolower(s1), "")[[1]]


check_matches("Eons", "ONES")


subject <- "corn"

candidates <- c("corn", "dark", "Corn", "rank", "CORN", "cron", "park")

anagram(subject, candidates)

