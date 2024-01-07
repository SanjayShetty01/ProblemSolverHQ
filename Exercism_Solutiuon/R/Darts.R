get_distance <- function(x, y){
  return(sqrt(x^2 + y^2))
}

get_score <- function(distance){
  if (distance <= 1) {
    return(10)
  } else if (distance <= 5) {
    return(5)
  } else if (distance <= 10) {
    return(1)
  } else {
    return(0)
  }
}

score <- function(x, y) {
  distance <- get_distance(x, y)
  score <- get_score(distance)
  return(score)
}
