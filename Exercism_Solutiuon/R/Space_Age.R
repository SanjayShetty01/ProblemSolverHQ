age_on_planet <- function(seconds, orbital_ratio) {
  seconds / (orbital_ratio * 31557600)
}

space_age <- function(seconds, planet) {
  
  age <- switch(tolower(planet),
                earth = age_on_planet(seconds, 1),
                venus = age_on_planet(seconds, 0.61519726),
                mercury = age_on_planet(seconds, 0.2408467),
                mars = age_on_planet(seconds, 1.8808158),
                jupiter = age_on_planet(seconds, 11.862615),
                saturn = age_on_planet(seconds, 29.447498),
                uranus = age_on_planet(seconds, 84.016846),
                neptune = age_on_planet(seconds, 164.79132))
  
  round(age, 2)
}