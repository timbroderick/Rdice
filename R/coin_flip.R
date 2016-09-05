# function to simulate a coin flip
#' @import data.table

flip.renaming <- function(my_df, coins){
  for(j in 1:coins){
    my_df[, j] <- ifelse(my_df[, j, with = FALSE] == 1, "H", "T")
  }
  my_df
}

#' @export
coin.flip <- function(coins = 5, flips = 100, weights = c(0.5, 0.5), getExact){
  getExact <- ifelse(getExact == "H", 1, 2)
  flips_table <- dice.combinations(faces = 2, dice = coins, rolls = flips, weights, getExact = getExact, toSum = FALSE)$values
  #flips_table <- flip.renaming(flips_table, coins)
  #flips$values <- ifelse(flips$values == 1, "H", "T")
  flips_table
}
