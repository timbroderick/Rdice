# function to simulate a coin flip
#' @import "data.table"

#' @export
coin.flip <- function(flips = 100, weights = c(0.5, 0.5)){
  flips <- dice.roll(faces = 2, dice = 1, rolls = flips, weights)$frequencies
  flips$values <- ifelse(flips$values == 1, "H", "T")
  flips
}
