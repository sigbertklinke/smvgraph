#' @rdname numeric_data
#' @title numeric_data
#' @description Converts a vector, matrix or data frame into a numeric vector, matrix or data frame.
#' @param x vector, matrix or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param na.action a function which indicates what should happen when the data contain NAs (default: [stats::na.pass])
#' @param title character: title attribute (default `NULL`)
#' @param ... unused
#'
#' @return the desired R object
#' @export
#'
#' @examples
#' numeric_data(iris)
#' numeric_data(iris, out="matrix")
#' numeric_data(iris, out="vector")
numeric_data <- function (x, select=NULL, out=c("data.frame", "matrix", "vector"),  na.action=stats::na.pass,  ..., title=NULL) {
  stopifnot(length(x)>0)
  if (!inherits(x, "data.frame")) x <- as.data.frame(x)
  out <- match.arg(out) 
  # col names
  nx  <- names(x)
  if (is.null(nx)) nx <- rep('', ncol(x))
  names(x) <- ifelse(nchar(nx)==0, sprintf("V%i", 1:ncol(x)), nx)
  # row names
  nx <- rownames(x)
  if (is.null(nx)) nx <- rep('', nrow(x))
  rownames(x) <- ifelse(nchar(nx)==0, sprintf("%i", 1:nrow(x)), nx)
  #
  if (is.null(select)) select <- names(x)
  vx <- x[,select,drop=FALSE]
  vx <- na.action(data.matrix(vx))
  if (out=="data.frame") {
    vx <- as.data.frame(vx)
  }
  if (out=="vector") {
    rn <- rownames(vx)
    cn <- colnames(vx)
    vx <- as.numeric(vx)
    names(vx) <- apply(as.matrix(rev(expand.grid(rn, cn, stringsAsFactors = FALSE))), 1, paste, collapse=",")
  }
  tit <- getval(attr(x, 'title'), title, paste0(select, collapse=","))
  structure(na.action(vx), title=tit, out=out)
}