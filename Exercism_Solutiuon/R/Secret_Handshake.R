
handshake <- function(n) {
  action <- c()
  
  if (bitwAnd(n, 1) > 0) action <- c(action, "wink")
  if (bitwAnd(n, 2) > 0) action <- c(action, "double blink")
  if (bitwAnd(n, 4) > 0) action <- c(action, "close your eyes")
  if (bitwAnd(n, 8) > 0) action <- c(action, "jump")
  
  return(if (bitwAnd(n, 16) == 0) action else rev(action))
  
}



################################################################################

intToBits(16)
bitwAnd(15, 7)
?bitwAnd

bitwAnd(1001, 16)
