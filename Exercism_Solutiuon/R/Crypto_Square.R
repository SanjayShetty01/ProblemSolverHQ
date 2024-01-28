normalized_plaintext <- function(input) {
  gsub("[^[:alnum:]]", "", input) |> tolower()
}

plaintext_segments <- function(input) {
  if (identical(input, "")) return("")
  input_normalized <- normalized_plaintext(input)

  chunk_length <- nchar(input_normalized) |>
    sqrt() |>
    ceiling()
  

  strsplit(x = input_normalized, 
           paste0("(?<=.{", chunk_length, "})"),# regular expression to split
           perl = TRUE) |>
    unlist()
}


encoded <- function(input) {
  
  if (identical(input, "")) return("")
  
  plain_segment <- plaintext_segments(input)
  
  endcoded_string <- sapply(1:nchar(plain_segment[1]), 
         \(i) sapply(plain_segment, substr, i, i)) |> 
    paste0(collapse = "")
  
  return(endcoded_string)
}

ciphertext <- function(input) {
  if (identical(input, "")) return("")
  
  plain_segment <- plaintext_segments(input)
  
  endcoded_string <- sapply(1:nchar(plain_segment[1]), 
                            \(i) sapply(plain_segment, substr, i, i))
  g = apply(endcoded_string, c(1, 2), \(x) ifelse(x == "", " ", x))
  
  v = paste0(g, collapse = "")
  
  strsplit(x = v, 
           paste0("(?<=.{", 7, "})"),# regular expression to split
           perl = TRUE) |>
    unlist()
}



################################################################################

a <- "aa-46,83!j hdf"
b <- ""
gsub("[^[:alnum:]]","",a)

length(a)
input <- "If man was meant to stay on the ground, god would have given us roots."
input <- "This is fun!"
input <- "If man was meant to stay on the ground, god would have given us roots."


find_dimensions <- function(message_length) {
  for (r in 1:(message_length/2)) {
    c <- ceiling(message_length / r)
    if (r * c >= message_length && c >= r && c - r <= 1) {
      return(c("r" = r, "c" = c))
    }
  }
  return(NULL)
}

find_dimensions(56)


strings <- c("ifmanwas", "meanttos", "tayonthe", "groundgo", "dwouldha", "vegivenu", "sroots")
g = sapply(strings, strsplit, "")

result <- apply(g, 2, paste0, collapse = "")

v = do.call(paste0, g)

sapply(1:nchar(strings[1]), \(i) sapply(strings, substr, i, i)) |> paste0(collapse = "")

paste0(v, collapse = "")

print(result)