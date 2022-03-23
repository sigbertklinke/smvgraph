#' numeric_data
#'
#' Converts a vector, matrix or data frame into a numeric vector, matrix or data frame.
#'
#' @param x vector, matrix or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param na.action a function which indicates what should happen when the data contain NAs (default: [stats::na.pass])
#'
#' @return the desired R object
#' @export
#'
#' @examples
#' numeric_data(iris)
#' numeric_data(iris, out="matrix")
#' numeric_data(iris, out="vector")
numeric_data <- function (x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass) {
  out <- match.arg(out)
  nx <- NULL
  if ("matrix" %in% class(x)) {
    nx <- colnames(x)
    if (is.null(nx)) nx <- sprintf("V%i", seq_len(ncol(x)))
    x  <- lapply(seq_len(ncol(x)), function(i) x[,i])
    names(x) <- nx
  }
  if ("data.frame" %in% class(x)) {
    nx <- colnames(x)
  }
  if (is.null(nx)) {
    nx <- attr(x, "title")
    if (is.null(nx)) nx <- "V1"
    x  <- list(x)
    names(x) <- nx
  }
  if (is.null(select)) select <- 1:length(x)
  x   <- x[select]
  nx  <- names(x)
  x   <- lapply(x, function(e) {
    if (is.character(e)) e <- factor(e)
    as.numeric(e)
  })
  if (out=="data.frame") x <- as.data.frame(x)
  if (out=="matrix") {
    x <- matrix(unlist(x), ncol=length(x))
    colnames(x) <- nx
  }   
  if (out=="vector") {
    x <- unlist(x)
    attr(x, "title") <- paste0(nx, collapse=",")
  }
  na.action(x)
}