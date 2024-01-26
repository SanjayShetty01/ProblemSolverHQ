bottles <- function (n, b) {
  if (n > 1) sprintf("%d bottles of beer", n) else
    if (n > 0) "1 bottle of beer" else
      if (b    ) "No more bottles of beer" else
        "no more bottles of beer"
}

verse <- function (number) {
  line2 <- if (number == 0) "Go to the store and buy some more" else
    paste0("Take ", ifelse(number == 1, "it", "one"),		        
           " down and pass it around")
  paste0(bottles(number, TRUE),  " on the wall, ",
         bottles(number, FALSE), ".\n",
         line2, ", ", bottles(ifelse(number == 0, 99, number-1), FALSE),
         " on the wall.\n")
}

lyrics <- function (first, last) {
  paste(sapply(first:last, verse), sep="", collapse="\n")  
}






################################################################################
lyrics <- function(first, last = NULL) {
  number_seq <- if (is.null(last)) first else first:last
  
  verse(number_seq)
}

first_line_generator <- function(number){
  if (number > 1) {
    n_verse <- sprintf("%d bottles of beer on the wall, %d bottles of beer.", 
                       number, number)
  } else {
    n_verse <- sprintf("%d bottle of beer on the wall, %d bottle of beer.", 
                       number, number)
  }
  return(n_verse)
}


second_line_generator <- function(number){
  if (number > 2) {
    n_verse <- sprintf("Take one down and pass it around, %d bottles of beer on the wall.\n", 
                       number - 1)
  } else {
    n_verse <- sprintf("Take one down and pass it around, %d bottle of beer on the wall.\n", 
                       number - 1)
  }
  return(n_verse)
}


verse_single <- function(number) {
  
  if (number > 1) {
  n_verse <- paste(first_line_generator(number), second_line_generator(number), 
                   sep = "\n")
  } else {
    n_verse <- 
      "No more bottles of beer on the wall, no more bottles of beer.
      Go to the store and buy some more, 99 bottles of beer on the wall.\n"
  }
  
  return(n_verse)  
}


verse <- Vectorize(verse_single)

################################################################################


verse(2)


paste(
  sep = "\n",
  "2 bottles of beer on the wall, 2 bottles of beer.",
  "Take one down and pass it around, 1 bottle of beer on the wall.\n"
)

g = sprintf("hi")

sprintf("%d ")

lyrics <- sprintf(
  "%d bottles of beer on the wall, %d bottles of beer.\nTake one down and pass it around, %d bottles of beer on the wall.",
  number, number, number - 1
)


number = 2

lyrics(99, 0)


verse(2) == 
paste(
  sep = "\n",
  "2 bottles of beer on the wall, 2 bottles of beer.",
  "Take one down and pass it around, 1 bottle of beer on the wall.\n"
)

second_line_generator(2)
