#' Variance check function
#'
#' Simple function to check within and between group variance
#' Equivalent to Mean Sq (and F) values from one-way anova
#'
#' @param DT data as data.table (or data frame) object
#' @param value dependent variable column of DT
#' @param group independent variable (factor) column of DT
#'
#' @import data.table
#'
#' @examples
#' DT <- data.table(dv=c(rnorm(100,10,2),rnorm(100,20,2)),iv=as.factor(c(rep("A",100),rep("B",100))))
#' varcheck(DT,"dv","iv")
#' @export
varcheck <- function(DT,
                     value,
                     group ) {

  requireNamespace("data.table", quietly = TRUE)
  DT <- as.data.table(DT)
  #DT[,(var(value)),by=group][,colMeans(.SD),.SDcols="V1"]
  #exp1[,.(.N*(mean(prop_f1)-mean(DT$value))^2),by=l1][,sum(.SD)/(.N-1),.SDcols="V1"]

  within  <- mean(DT[,(var(get(value))),by=get(group)][[2]])

  between <- sum(DT[,.(.N*(mean(get(value))-mean(DT[[value]]))^2),by=get(group)][[2]])/(length(unique(DT[[group]]))-1)

  return(c(within=within,between=between,ratio=between/within))

}
