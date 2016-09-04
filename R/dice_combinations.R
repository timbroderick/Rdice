# function to calculate dice rolls combinations
#' @import "data.table"

my.norm <- function(x){
  norm(x, type = "2")
}

format.values <- function(values, dice){
  values <- as.data.table(t((apply(values,1, sort))))
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


is.exact <- function(my_df, dice, u){
  as.data.table(apply(my_df[, 1:dice, with = FALSE], 1, function(x) all(u%in% x)))
}


is.partial <- function(my_df, dice, u){
  as.data.table(apply(my_df[, 1:dice, with = FALSE], 1, function(x) any(u%in% x)))
}


#' @export
dice.combinations <- function(faces = 6, dice = 3, rolls = 150, weights, getPartial = c(1:faces), getExact, toSum = FALSE){
  e1 <-tryCatch(
    {
      !(is.logical(toSum))
    },
    error = function(){
      return(TRUE)
    }
  )
  if(!e1){
    values <- dice.roll(faces, dice, rolls, weights)$results
    values <- format.values(values, dice)
    values <- values[values[, .I[is.partial(values, dice, getPartial)==TRUE]]]
    if(!missing(getExact)){
      e2 <-tryCatch(
        {
          !(length(getExact)<= faces)
        },
        error = function(){
          return(TRUE)
        }
      )
      if(!e2){
        # case of exact matches with getExact
        values <- values[values[, .I[is.exact(values, dice, getExact)==TRUE]]]
      } else {
        stop("The number of elements in getExact must be at most the number of faces, hence length(getExact) <= faces")
      }
    }
    if(isTRUE(toSum)){
      values <- sum(values$freq)
    }
    list(values = values)
  } else {
    stop("The parameter sum must be set to either TRUE or FALSE")
  }
}
