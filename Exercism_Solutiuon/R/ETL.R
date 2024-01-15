etl <- function(input) {
  stacked_df <- stack(input)
  scores <- strtoi(stacked_df$ind) |> as.list()
  names = tolower(stacked_df$values)
  names(scores) <- names
  
  scores <- scores[order(names(scores))]
  return(scores)
}




################################################################################
a = list("1" = "A")
a
b = list("1" = c("A", "E", "I", "O", "U"))

b$`1`

c = list("1" = c("A", "E"),
         "2" = c("D", "G"))

c$`2`

c$`2`


names <- names(c)


for (name in names) {
  score = as.numeric(name)
  print(score)
  
  all_letters <- c[[score]]
  print(all_letters)
  
  a = tolower(all_letters)
  
  b = list()
  names(b) = a
}

letter_points <- list(
  a = 1,
  e = 1,
  i = 1,
  o = 1,
  u = 1,
  l = 1,
  n = 1,
  r = 1,
  s = 1,
  t = 1,
  d = 2,
  g = 2,
  b = 3,
  c = 3,
  m = 3,
  p = 3,
  f = 4,
  h = 4,
  v = 4,
  w = 4,
  y = 4,
  k = 5,
  j = 8,
  x = 8,
  q = 10,
  z = 10
)

# Example usage

print(letter_points)

a = stack(c)

#a = stack(letter_points)

score = as.numeric(a$ind)

names = tolower(a$values)

lists = as.list(score)

lists
score
names(lists) <- names

lists[order(names(lists))]

list[]


b = list(`1` = c("A", "E"), `2` = c("D", "G"))

etl(b)


etl(list("1" = c("A", "E", "I", "B", "U")))


etl(list(
  "1" = c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T"),
  "2" = c("D", "G"),
  "3" = c("B", "C", "M", "P"),
  "4" = c("F", "H", "V", "W", "Y"),
  "5" = c("K"),
  "8" = c("J", "X"),
  "10" = c("Q", "Z")))



a = list(
  "1" = c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T"),
  "2" = c("D", "G"),
  "3" = c("B", "C", "M", "P"),
  "4" = c("F", "H", "V", "W", "Y"),
  "5" = c("K"),
  "8" = c("J", "X"),
  "10" = c("Q", "Z"))

m = stack(a)

as.numeric(levels(m$ind))[m$ind]


strtoi(m$ind)
?strtoi

