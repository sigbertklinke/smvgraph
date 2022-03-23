#' character_data
#'
#' Converts a matrix or data frame into a character vector, matrix or data frame. If `na.action` is a character then
#' all `NA`s are replaced by `na.action` (default: na.action="NA"`). If `na.action` is a function then the function will be
#' applied to the result.
#'
#' @param x vector, matrix or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param na.action function or character: indicates what should happen when the data contain NAs 
#'
#' @return the desired R object
#' @export
#'
#' @examples
#' character_data(iris)
#' character_data(iris, out="matrix")
#' character_data(iris, out="vector")
character_data <-  function (x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action="NA") {
  out <- match.arg(out)
  nx <- NULL
  if ("matrix" %in% class(x)) {
    nx <- colnames(x)
    if (is.null(nx)) nx <- sprintf("V%i", seq_len(ncol(x)))
    x  <- lapply(seq_len(ncol(x)), function(i) x[,i])
    names(x) <- nx
  }
  if ("data.frame" %in% class(x)) nx <- colnames(x)
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
    e <- as.character(e)
    if (is.character(na.action)) e[is.na(e)] <- na.action 
    e
  })
  if (out=="data.frame") {
    x <- as.data.frame(x)
    colnames(x) <- nx
  }
  if (out=="matrix") {
    x <- matrix(unlist(x), ncol=length(x))
    colnames(x) <- nx
  }
  if (out=="vector") {
    x <- unlist(x)
    attr(x, "title") <- paste0(nx, collapse=",")
  }
  if (is.function(na.action)) x <- na.action(x)
  x
}
