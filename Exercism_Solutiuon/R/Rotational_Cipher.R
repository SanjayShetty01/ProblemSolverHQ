get_rotated_character <- function(character, key) {
  if (character %in% LETTERS) {
    letter_position <- which(character == LETTERS)
    new_position <- (letter_position + key - 1) %% 26 + 1
    return(LETTERS[new_position])
  } else if (character %in% letters) {
    letter_position <- which(character == letters)
    new_position <- (letter_position + key - 1) %% 26 + 1
    return(letters[new_position])
  } else {
    return(character)
  }
}

rotate <- function(text, key) {
  text_split <- strsplit(text, "") |> unlist()
  rotated_text <- sapply(text_split, get_rotated_character, key = key)
  return(paste0(rotated_text, collapse = ""))
}



################################################################################

key = 2
text = "B" |> toupper()


which(text == LETTERS)


letters

which("b" == letters)


string = "he " |> strsplit(x = _, split = "") |> unlist()

for (i in string) {
  print(i)
}


sapply(string, print, simplify = T, USE.NAMES = F)


text = "abc"
key = 1

strings <- text |> strsplit(x = _, split = "") |> unlist()


for (string in strings) {
  if (string %in% LETTERS) {
    letter_position <- which(string == LETTERS)
    new_position <- ifelse((letter_position + key) > 26,
                           26 %% (letter_position + key),
                           (letter_position + key))
    print(LETTERS[new_position])
  } else if (string %in% letters) {
    letter_position <- which(string == letters)
    new_position <- ifelse((letter_position + key) > 26,
                           26 %% (letter_position + key),
                           (letter_position + key))
    print(letters[new_position])
  } else {
    print(string)
  }
}


which("a" == letters)

letters[2]


get_rotated_character <- function(character, key) {
  if (character %in% LETTERS) {
    letter_position <- which(character == LETTERS)
    new_position <- ifelse((letter_position + key) > 26,
                           (letter_position + key) %% 26,
                           (letter_position + key))
    return(LETTERS[new_position])
  } else if (character %in% letters) {
    letter_position <- which(character == letters)
    new_position <- ifelse((letter_position + key) > 26,
                           (letter_position + key) %% 26,
                           (letter_position + key))
    return(letters[new_position])
  } else {
    return(character)
  }
}


get_rotated_character("L", 21)


rotate("OMG", 5)
