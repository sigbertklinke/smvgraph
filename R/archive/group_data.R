#' group_data
#'
#' Creates a single group variable from the data `x`.
#'
#' @param x vector, matrix, or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param ... further parameters to [character_data]
#' @param colname character: 
#'
#' @return a one-column matrix with the merged groups
#' @export
#'
#' @examples
#' group_data(iris$Species, out="vector")
#' group_data(iris)
group_data <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), ..., colname=".singlegroup") {
  browser()
  out <- match.arg(out)
  nx  <- colnames(x)
  cn  <- colname 
  if (is.null(nx)) {
    title <- attr(x, "title")
    if (is.null(title)) title <- colname
  } else {
    title <-  paste0(nx, collapse=",")
  }
  args        <- list(...)
  args$x      <- x
  args$select <- select
  args$out    <- out
  x   <- do.call(character_data, args)
  x   <- apply(x, 1, paste0, collapse=",")
  if (out=='data.frame') {
    x <- data.frame(x=x)
    names(x) <- cn
  }
  if (out=='matrix') {
    x <- matrix(x, ncol=1)
    colnames(x) <- cn
  }
  structure(x, title=title)
}