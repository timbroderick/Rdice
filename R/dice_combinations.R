# function to calculate dice rolls combinations
#' @import "data.table"

my.norm <- function(x){
  norm(x, type = "2")
}

format.values <- function(values, dice){
  values$norm <- apply(values, 1, FUN = my.norm)
  values_by_norm <- values[, .N, by = norm]
  setkey(values, norm)
  setkey(values_by_norm, norm)
  values <- values[values_by_norm, nomatch = 0]
  values <- unique(values, by = "norm")
  values <- values[, freq:=N/sum(N)]
  values <- values[, c("N", "norm"):=NULL]
  if(dice > 1){
    colnames(values)[1:dice] <- paste0("value_", 1:dice)
  }
  return(values)
}


is.exact <- function(s,t){
  return(isTRUE(s %in% t))
}


is.partial <- function(s,t){
  return(!isTRUE(!(s %in% t)))
}



#' @export
dice.combinations <- function(faces = 6, dice = 2, rolls = 15, weights){
  values <- dice.roll(faces, dice, rolls, weights)$results
  values <- format.values(values, dice)
  list(values = values)
}
