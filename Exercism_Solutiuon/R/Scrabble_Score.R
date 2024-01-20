get_score <- function(char) {
  
  one_score_letters <- c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T")
  two_score_letters <- c("D", "G")
  three_score_letters <- c("B", "C", "M", "P")
  four_score_letters <- c("F", "H", "V", "W", "Y")
  five_score_letters <- c("K")
  eight_score_letters <- c("J", "X")
  ten_score_letters <- c("Q", "Z")
  
  if (char %in% one_score_letters) return(1)
  if (char %in% two_score_letters) return(2)
  if (char %in% three_score_letters) return(3)
  if (char %in% four_score_letters) return(4)
  if (char %in% five_score_letters) return(5)
  if (char %in% eight_score_letters) return(8)
  if (char %in% ten_score_letters) return(10) 
}


scrabble_score <- function(input){
  if (input == "") return(0)
  string_split <- strsplit(input, split = "") |> unlist() |> toupper()
  scores <- sapply(string_split, get_score) |> sum()
  
  return(scores)
}


################################################################################

one_score_letters <- c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T")
two_score_lettes <- c("D", "G")
three_score_letters <- c("B", "C", "M", "P")
four_score_letters <- c("F", "H", "V", "W", "Y")
five_score_letters <- c("K")
eight_score_letters <- c("J", "X")
ten_score_letters <- c("Q", "Z")


get_score <- function(char) {
  
  one_score_letters <- c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T")
  two_score_letters <- c("D", "G")
  three_score_letters <- c("B", "C", "M", "P")
  four_score_letters <- c("F", "H", "V", "W", "Y")
  five_score_letters <- c("K")
  eight_score_letters <- c("J", "X")
  ten_score_letters <- c("Q", "Z")
  
  if (char %in% one_score_letters) return(1)
  if (char %in% two_score_letters) return(2)
  if (char %in% three_score_letters) return(3)
  if (char %in% four_score_letters) return(4)
  if (char %in% five_score_letters) return(5)
  if (char %in% eight_score_letters) return(8)
  if (char %in% ten_score_letters) return(10)
}


get_score("H")

################################################################################

scrabble_score1 <- function(input){
  if(nchar(input) == 0) return(0)
  letterss = c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T", "D", "G", "B", "C", "M", "P", "F", "H", "V", "W", "Y", "K", "J", "X", "Q", "Z")
  values <- c(rep(1, 10), rep(2,2), rep(3,4), rep(4,5), 5, rep(8,2), rep(10,2))
  def_cor <- data.frame(letterss, values)
  
  
  split_input <- toupper(strsplit(input, "")[[1]])
  sum(sapply(toupper(strsplit(input, "")[[1]]), function(x){ifelse(any(def_cor$letterss == x), def_cor$values[which(def_cor$letterss == x)], 0)}))
}


scrabble_score2 <- function(input){
  library("magrittr")
  
  if(input=="") return(0)
  
  ltrs <- list(
    "1" = c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T"), 
    "2" = c("D", "G"), 
    "3" = c("B", "C", "M", "P"), 
    "4" = c("F", "H", "V", "W", "Y"), 
    "5" = c("K"), 
    "8" = c("J", "X"), 
    "10"= c("Q", "Z")
  )
  
  scores <- ltrs %>% names %>% strtoi
  
  score_ltr <- function(x) {
    scores[sapply(ltrs, function(lst)(x %in% lst))]
  }
  
  input %>% 
    toupper %>%
    strsplit("") %>%
    getElement(1) %>%
    sapply(score_ltr) %>%
    sum
}

bench::mark(scrabble_score("A"),
            scrabble_score1("A"), 
            scrabble_score2("A"))
