number_product <- function(x){
  # null split because leading zero can be kept
  prod(as.numeric(unlist(strsplit(as.character(x), NULL))))
}


largest_series_product <- function(digits, span){
  
  stopifnot("this error" = nchar(digits) >= span)
  stopifnot("this error" = !grepl("\\D", digits))
  stopifnot("this error" = span > 1)
  
  numbers_split = c()
  
  for (i in 1:(nchar(digits) - (span - 1))) {
    numbers_split = c(numbers_split, substr(digits, i, i + (span - 1)))
  }
  
  numbers_prod <- sapply(numbers_split, \(x) (number_product(x)))
  
  return(max(numbers_prod))
}




################################################################################


largest_series_product("63915", 3)

g = c(1,32,4)

prod(g)

32 * 4

h = "0123456789"
span = 2

g = c()
for (i in 1:(nchar(h) - (span - 1))) {
  print(i)
  g = c(g, substr(h, i, i + (span - 1)))
}
g

v = g |> as.list() 

number_product <- function(x){
  prod(as.numeric(unlist(strsplit(as.character(x), NULL))))
}



v

sapply(g, \(x) (number_product((x)))) 

substring(h, 1, 2)

strsplit()



char_num <- "00123"

num <- as.numeric(char_num)
print(num)  # Output: 123

char_vector <- strsplit(char_num, NULL)[[1]]
num_preserve_zeros <- prod(as.numeric(char_vector))
print(num_preserve_zeros)  # Output: 0 (product of 0, 0, 1, 2, 3)



number_product("001")
