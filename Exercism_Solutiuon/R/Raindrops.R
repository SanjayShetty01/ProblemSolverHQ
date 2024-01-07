get_factors <- function(number) {
  return(which(number %% 1:number == 0))
}


get_sounds <- function(factors_number) {
  if (factors_number == 3) {
    return("Pling")
  } else if (factors_number == 5) {
    return("Plang")
  } else if (factors_number == 7) {
    return("Plong")
  } else {
    return("")
  }
}

valid_sound <- function(sound, number) {
  if (identical(sound, "")) {
    return(as.character(number))
  } else {
    return(sound)
  }
}

raindrops <- function(number) {
  factors <- get_factors(number)
  sounds <- paste0(sapply(factors, get_sounds), collapse = "")
  sound <- valid_sound(sounds, number)
  return(sound)
}


###############################################################################
