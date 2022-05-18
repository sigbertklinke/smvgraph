#' getVariableNames
#'
#' Extracts variable names from a data frame or matrix (column names).
#'
#' @param x data frame/matrix: data set to analyse
#' @param xvar character: variable names to analyse (default: `character(0)` = all variables)
#' @param num logical: should numerical or non-numerical variable use (default: `TRUE`)
#'
#' @return character vector with variable names
#' @export
#'
#' @examples
#' getVariableNames(iris)
#' getVariableNames(iris, num=FALSE)
#' getVariableNames(normalize(iris, 0))
#' getVariableNames(normalize(iris, 0), num=FALSE)
getVariableNames <- function(x, xvar=NULL, num=TRUE) {
  if (is.matrix(x)) {
    names <- colnames(x)
    if(xor(is.numeric(x[1,1]), num)) names <- character(0)
  } else {
    names <- names(x)
    numcl <- sapply(x, class) %in% c("integer", "numeric")
    names <- if(num) names[numcl] else names[!numcl] 
  }
  if (length(xvar)>0) names <- intersect(names, xvar)
  names
}