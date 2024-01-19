comp <- function(s1, s2){         
  in1 = letters %in% strsplit(tolower(s1), "")[[1]]
  in2 = letters %in% strsplit(tolower(s2), "")[[1]]
  sum(in1 & in2)/sum(in1)
}

anagram <- function(subject, candidates) {
  similarity <- comp(subject, candidates)
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

strsplit(tolower(s1), "")[[1]]
