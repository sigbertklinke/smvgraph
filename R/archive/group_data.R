#' @rdname group_data
#' @title group_data
#' @description Creates a single group variable from the data `x`.
#' @param x vector, matrix, or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param ... further parameters to [character_data]
#' @param title character: title attribute (default `NULL`)
#'
#' @return a one-column matrix with the merged groups
#' @export
#'
#' @examples
#' group_data(iris$Species, out="vector")
#' group_data(iris)
group_data <- function(x, ...) { UseMethod("group_data") }

#' @rdname group_data
#' @export
group_data.default <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out)
  tit <- getval(title, attr(x, 'title'))
  character_data(x, select=select, out=out, ..., title=tit)
}

#' @rdname group_data
#' @export
group_data.matrix <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out)
  cx  <- character_data(x, select, out='matrix', ..., title=title)
  cx2 <- apply(cx, 1, paste0, collapse=",")
  tit <- getval( title, attr(x, 'title'), attr(cx, 'title'), paste0(colnames(cx), collapse=","))
  convertTo(cx2, coln=paste0(colnames(cx), collapse=","), rown=rownames(cx), out = out, title=tit)
}

#' @rdname group_data
#' @export
group_data.data.frame <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), ..., title=NULL) {
  #browser()
  stopifnot(length(x)>0)
  out <- match.arg(out)
  cx  <- character_data(x, select, out='matrix', ..., title=title)
  cx2 <- apply(cx, 1, paste0, collapse=",")
  tit <- getval( title, attr(x, 'title'),  attr(cx, 'title'), paste0(colnames(cx), collapse=","))
  convertTo(cx2, coln=paste0(colnames(cx), collapse=","), rown=rownames(cx), out = out, ..., title=tit)
}