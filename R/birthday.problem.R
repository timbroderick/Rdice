
# produces one experiment of the random birthdays
birthday.experiment <- function(days, people, gap){
  values <- sample(1:days, people, replace = TRUE)
  values <- sort(values)
  differences <- append(values[-1],NA)-values
  return(gap%in%differences)
}


# function to calculate the first k factorials
k_factorial <- function(n, k){
  if(k==1){
    return(1)
  } else {
    values <- ((n-k+1)/n)*k_factorial(n,k-1)
    values
  }
}


# replicates the birthday experiment a certain amount of
# times and takes the statistics
#' @export
birthday.problem <- function(days = 365, people, gap = 0, repetitions){
  results <- replicate(repetitions, birthday.experiment(days, people, gap))
  if(gap == 0){
    actuals <- 1- k_factorial(days, people)
    return_object <- list(
      experiment = length(which(results==TRUE))/length(results),
      theory = actuals,
      repetitions = repetitions
    )
    return_object$call <- match.call()
    class(return_object) <- "birthdayProblem"
    return(return_object)
  } else {
    return(length(which(results==TRUE))/length(results))
  }
}


#' @export
print.birthdayProblem <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nResults after", x$repetitions, "repetitions of the birthday experiment:\n")
  print(x$experiment)
  cat("\nThe theoretical exact result is:\n")
  print(x$theory)
  cat("\nand therefore we have a relative error of", abs(x$experiment-x$theory)/x$theory)
}
