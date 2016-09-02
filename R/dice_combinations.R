# function to calculate dice rolls combinations
#' @import data.table

my.norm <- function(x){
  norm(x, type = "2")
}

dice.combinations <- function(faces = 6, dice = 2, rolls = 15, weights){
  values <- dice.roll(faces, dice, rolls, weights)$results
  values$norm <- apply(values, 1, FUN = my.norm)
  values_by_norm <- values[, .N, by = norm]
  setkey(values, norm)
  setkey(values_by_norm, norm)
  values <- values[values_by_norm, nomatch = 0]
  values <- unique(values, by = "norm")
}
