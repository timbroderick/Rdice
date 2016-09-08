

# produces a list of Z dice with N faces
dice_list.generator <- function(dice, faces){
  dice_list <- vector(mode= "numeric", length = 0)
  for(k in 1:dice){
    dice_list <- cbind(dice_list, sample(0:faces, faces, replace = TRUE))
  }
  dice_list <- cbind(dice_list, dice_list[,1])
  dice_list
}


# checks if the first die wins the second
is.winner <- function(first, second, prob, error){
  comb <- as.data.table(expand.grid(first, second))
  comb$winner <- ifelse(comb$Var1 > comb$Var2, "first", "second")
  results <- comb[, .N, by = winner][, freq:= N/sum(N)]
  return(results[winner == "first", freq > prob-error && freq<prob+error])
}


efron.generator <- function(dice, faces, prob, error = 0.001){
    dice_list <- dice_list.generator(dice, faces)
    print(dice_list)

    truth <- vector(mode="logical", length=0)
    for(j in 1:dice){
      print(dice_list[,j])
      print(dice_list[,j+1])
      truth <- cbind(truth, is.winner(dice_list[,j], dice_list[,j+1], prob = prob, error = error))
      print(truth)
      print(all(truth))
    }
    if(is.na(all(truth)) || !all(truth)){
      values <- NULL
    } else {
      dice_list <- dice_list[, -(dice+1)]
      values <- dice_list
    }
    values
}


