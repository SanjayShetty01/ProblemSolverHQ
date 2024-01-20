
dna_rna_convertor <- function(dna) {
  switch(dna,
         "G" = "C",
         "C" = "G",
         "T" = "A",
         "A" = "U",
         stop("error"))
}


to_rna <- function(dna) {
  string_vec <- strsplit(dna, split = "") |> unlist()
  rna_seq = sapply(string_vec, dna_rna_convertor)
  rna_seq_char <- paste(rna_seq, sep = "", collapse = "")
  return(rna_seq_char)
}



################################################################################


dna_to_rna <- list('G' = "C",
                   'C' = "G",
                   'T' = "A",
                   'A' = "U")


dna


to_rna <- function(dna) {
  switch(dna,
         "G" = "C",
         "C" = "G",
         "T" = "A",
         "A" = "U",
         stop("error"))
}


to_rna("L")


string_vec <- strsplit("CT", split = "") |> unlist()


rna_seq <- paste(string_vec, sep = "", collapse = "")

dna_rna_convertor <- function(dna) {
  switch(dna,
         "G" = "C",
         "C" = "G",
         "T" = "A",
         "A" = "U",
         stop("error"))
}


a = sapply(string_vec, dna_rna_convertor)


paste(a, sep = "", collapse = "")
