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
  if(!missing(getExact)){
    e2 <-tryCatch(
      {
        !(length(getExact)<= coins)
      },
      error = function(){
        return(TRUE)
      }
    )
    if(!e2){
      # case of exact matches with getExact
      coinExact <- ifelse(getExact == "H", 1, 2)
      flips_table <- dice.combinations(faces = 2, dice = coins, rolls = flips, weights, getExact = coinExact, toSum = FALSE)
      flips_table <- flip.renaming(flips_table, coins)
      if(coins > 1){
        colnames(flips_table)[1:coins] <- paste0("value_", 1:coins)
      }
      flips_table <- flips_table[flips_table[, .I[is.exact(flips_table, coins, getExact)==TRUE]]]
    } else {
      stop("The number of elements in getExact must be at most the number of dice, hence length(getExact) <= dice")
    }
  } else {
    flips_table <- dice.combinations(faces = 2, dice = coins, rolls = flips, weights, toSum = FALSE)
    flips_table <- flip.renaming(flips_table, coins)
    if(coins > 1){
      colnames(flips_table)[1:coins] <- paste0("value_", 1:coins)
    }
  }

  return_object <- list(results = flips_table
                        ,coins = coins
                        ,flips = flips)
  return_object$call <- match.call()
  class(return_object) <- "coinFlip"
  return(return_object)
}


#' @export
print.coinFlip <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n Results after", x$flips, "flips of", x$coins, "coins:\n")
  print(x$results)
}


#' @export
plot.coinFlip <- function(x, colour = c("light", "dark"), ...){
  x <- x$results
  if(missing(colour) || colour =="light"){
    my.background <- '#fdf6e3'
    my.fill       <- 'burlywood1'
    my.color      <- 'burlywood3'
  } else if (colour == "dark"){
    my.background <- '#002b36'
    my.fill       <- '#657b83'
    my.color      <- '#6c71c4'
  } else {
    stop("colour must be either 'dark' or 'light'.")
  }
    p  <- ggplot(x, aes(y=x$freq, x=factor(c(0:(length(x$freq)-1)))))
    p <- p + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill
                                                   = my.background),
                   axis.line = element_line(colour = "black"),
                   legend.text=element_text(size=16),
                   legend.title=element_blank(),
                   axis.title.x = element_text(vjust=0, size=16),
                   axis.title.y = element_text(vjust=1, size=16),
                   plot.title   = element_text(vjust=1.5, size=20))
    p <- p + geom_bar(width=.7, fill= my.fill,
                      color= my.color, stat = 'identity')
    p <- p + labs(title = "")
    p <- p + labs(x = "number of tails")
    p <- p + labs(y = "frequency")
    show(p)
}



