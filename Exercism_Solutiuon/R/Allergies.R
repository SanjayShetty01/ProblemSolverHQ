get_allergy <- function(num) {
  allergy_list <- c(
    "eggs",
    "peanuts",
    "shellfish",
    "strawberries", 
    "tomatoes",
    "chocolate",
    "pollen",
    "cats"
  )
  
  position <- intToBits(num) |> as.logical()
  allergy <- allergy_list[position] |> na.omit()
  
  return(allergy)
}


allergy <- function(num) {
  return(get_allergy(num))
}

allergic_to <- function(allergy_object, allergy) {
  allergy %in% allergy_object
}

list_allergies <- function(allergy_object) {
  allergy_object
}



################################################################################

g = intToBits(128) |> as.logical()

g

allergy_list <- c(
  "eggs",
  "peanuts",
  "shellfish",
  "strawberries", 
  "tomatoes",
  "chocolate",
  "pollen",
  "cats"
)


allergy_list[g]
