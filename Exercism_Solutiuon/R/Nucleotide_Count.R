nucleotide_count <- function(input) {
  string_split = strsplit(input, split = "") |> unlist()
  
  stopifnot(string_split %in% c("G", "C", "T", "A"))
  
  counts = table(string_split)
  result = list( A = counts["A"] |> as.numeric(),
                 C = counts["C"] |> as.numeric(),
                 G = counts["G"] |> as.numeric(),
                 T = counts["T"] |> as.numeric())
  
  
  result[is.na(result)] <- 0
  
  return(result)
}




################################################################################


input = "GGGGGG"

string_split = strsplit(input, split = "") |> unlist()

stopifnot(string_split %in% c("G", "H", "C", "T"))

counts = table(string_split)
l = counts["H"]

l |> as.numeric()


g["G"]

g["H"]

nchar(string_split)

result = list( A = counts["A"] |> as.numeric(),
      C = counts["C"] |> as.numeric(),
      G = counts["G"] |> as.numeric(),
      T = counts["T"] |> as.numeric())


result[is.na(result)] <- 0

class(counts)


nucleotide_count1 <- function(input) {
  if (grepl("[^ACGT]", input)) {
    stop("invalid nucleotide")
  }
  as.list(table(factor(
    strsplit(input, "")[[1]], levels = c("A", "C", "G", "T")
  )))
  
}

nucleotide_count2 <- function(input) {
  count <- list()
  
  if (grepl("[^ACGT]", input)) {
    stop("Error! Invalid nucleotides detected")
  } else {
    count["A"] <- lengths(regmatches(input, gregexpr('A', input)))
    count["C"] <- lengths(regmatches(input, gregexpr('C', input)))
    count["G"] <- lengths(regmatches(input, gregexpr('G', input)))
    count["T"] <- lengths(regmatches(input, gregexpr('T', input)))
  }
  return (count)
}

nucleotide_count3 <- function(input) {
  nucs <- c(strsplit(input, "")[[1]], "A", "C", "G", "T") # Add dummy nucleotides to chum the table
  stopifnot(all(nucs %in% c("A", "C", "G", "T")))
  tbl <- table(nucs) - 1
  as.list(tbl)
}

nucleotide_count4 <- function(input) {
  stopifnot(grepl("^[ACGT]*$", input))
  seq <- strsplit(input, "")[[1]]
  counts <- setNames(nm = list("A", "C", "G", "T"))
  lapply(counts, function(x) sum(x == seq))
}



input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCGCGCGCGCGCGCGCGCGCGCGCAAAAAAAAAAAAAA"

bench::mark(nucleotide_count(input),
            nucleotide_count1(input),
            nucleotide_count2(input),
            nucleotide_count3(input),
            nucleotide_count4(input))
