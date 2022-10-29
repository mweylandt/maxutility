#' Calculate the mean Root Pairs number for each respondent
#' over a set of supplied variables
#' @export
#' @name meanRootPairs
#' @param inputVars vector of variable names
#' @param newVarName the name the new variable should take
#' @examples
#' to come


meanRootPairs <- function(x, inputVars, newVarName) {
  combos <- combn(inputVars, m = 2, simplify = FALSE)

  pairsList <- list()
  # calculate the pairs
  for (i in 1:length(combos)){
    pairsList[[i]]  <- abs(x[,combos[[i]][1]] - x[,combos[[i]][2]])

  }

  #paste0(combos)
  # sum across
  t <- as.data.frame(pairsList, col.names = paste0(combos))
  names(t) <- paste0(combos)

  #rowwise even needed now?
  t <- t %>% rowwise() %>% mutate(sums = rowSums(across(.cols = everything()), na.rm = TRUE),
                                  mrp_t= sqrt(sums)/length(vars)) %>%
    ungroup()

  tempmax <- max(t$mrp_t, na.rm=TRUE)
  tempmin <- min(t$mrp_t, na.rm=TRUE)

  t$mrp <- (t$mrp_t - tempmax)/
    (tempmin - tempmax)


  #x <- cbind(x, newVarName =  t$sqr)
  x[[newVarName]] <- t$mrp

  return(x)

}
