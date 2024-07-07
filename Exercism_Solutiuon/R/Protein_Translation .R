rna_spliter <- function(rna_value){
  unlist(strsplit(rna_value, "(?<=.{3})", perl = TRUE))
}

starts_with_STOP_handling <- function(split_rna, stop_values) {
  ifelse(split_rna[1] %in% stop_values, T, F)
}


get_required_rna <- function(split_rna, stop_values){
  stop_pos <- match(split_rna, stop_values)
  split_rna_required <- ifelse(is.na(stop_pos), split_rna, split_rna[1:(stop_pos)])
  return(split_rna_required)
}

translate <- function(bases) {
  stop_values <- c("UAA", "UAG", "UGA")
  if(bases == "") return(NULL)
  
  split_rna <- rna_spliter(bases)

  if(starts_with_STOP_handling(split_rna, stop_values)) return(c())

  split_rna_required <- get_required_rna(split_rna, stop_values = stop_values)

  codon_to_protein <- list(
    AUG = "Methionine",
    UUA = "Leucine", UUG = "Leucine",
    UUU = "Phenylalanine", UUC = "Phenylalanine",
    UCU = "Serine", UCC = "Serine", UCA = "Serine", UCG = "Serine",
    UAU = "Tyrosine", UAC = "Tyrosine",
    UGU = "Cysteine", UGC = "Cysteine",
    UGG = "Tryptophan",
    UAA = "STOP", UAG = "STOP", UGA = "STOP"
  )

  translated <- sapply(split_rna_required, \(x) codon_to_protein[x]) |> unlist(use.names = F) 
  
  return(translated)
}

translate("UGGUGUUAUUAAUGGUUU")

###################################################################################
rna <- "AUGUUUUGG"

split_rna <- unlist(strsplit(rna, "(?<=.{3})", perl = TRUE))

split_rna

stop_values <- c("UAA", "UAG", "UGA")

match(stop_values, split_rna) |> 

stop_pos <- which(split_rna %in% stop_values) |> min()

split_rna_required <- split_rna[1:(stop_pos)]

codon_to_protein <- list(
  AUG = "Methionine",
  UUA = "Leucine", UUG = "Leucine",
  UUU = "Phenylalanine", UUC = "Phenylalanine",
  UCU = "Serine", UCC = "Serine", UCA = "Serine", UCG = "Serine",
  UAU = "Tyrosine", UAC = "Tyrosine",
  UGU = "Cysteine", UGC = "Cysteine",
  UGG = "Tryptophan",
  UAA = "STOP", UAG = "STOP", UGA = "STOP"
)

codon_to_protein["AUG"]

translated <- sapply(split_rna_required, \(x) codon_to_protein[x]) |> unlist(use.names = F) 

translated
