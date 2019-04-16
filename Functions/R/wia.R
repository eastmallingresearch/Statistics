#' Willmot Index of Agreement (d)
#'
#' wia calculates the Willmot index of agreement of simulated to observed data.
#'
#' @param obs observed data vector 
#' @param sim simulated data vector
#' @return (d) the index of agreement
#' @example
#' wia(1:10,1:10)
#' wia(1:10,2:11)

wia <- function (obs,sim) {
  d <- 1 -  (sum((obs - sim)^2)   / sum( ( abs(sim - mean(obs)) + abs(obs - mean(obs)) )^2 ))
  d <- c(d,mean(abs(obs-sim)/abs(obs)))
  d
}
