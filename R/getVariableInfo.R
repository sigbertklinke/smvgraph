#' getVariableInfo
#'
#' Returns a data frame with one row for each variable in `data`:
#' 
#' * `class` the [base::class] of the variable
#' * `missing` the number of missing values
#' * `infinite` the number of infinite values
#' * `unique` the number of unique values
#' * `valid` the number of unique valid values (see [smvgraph::valid])
#' * `values` the values with the decreasing frequency
#'
#' @param data data frame: input data set
#' @param n integer: character length for `values` (default: `47`)
#'
#' @return a data frame with information about the variables of the input data set
#' @export
#'
#' @examples
#' getVariableInfo(iris)
getVariableInfo <- function(data, n=47) {
  data.frame(
    class    = sapply(data, function(e) { paste0(sort(class(e)), collapse=',')}),
    missing  = sapply(data, function(e) { sum(is.na(e))}),
    infinite = sapply(data, function(e) { sum(is.infinite(e))}),
    finite   = sapply(data, function(e) { sum(is.finite(e))}),
    unique   = sapply(data, function(e) { length(unique(e))}),
    valid    = sapply(data, function(e) { length(unique(e[valid(e)]))}),
    values   = sapply(data, function(e) { 
      v <- paste0(names(sort(table(e, useNA="ifany"), decreasing=TRUE)), collapse=", ")
      if (nchar(v)>n) v <- paste0(substr(v, 1, n), '...')
      v
    })
  )
}