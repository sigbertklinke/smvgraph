#' @rdname numeric_data
#' @title numeric_data
#'
#' Converts a vector, matrix or data frame into a numeric vector, matrix or data frame.
#'
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
numeric_data   <- function(x, ...)   { UseMethod("numeric_data") }

#' @rdname numeric_data
#' @export
numeric_data.default <- function(x, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass,  ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out) 
  vx  <- x
  if (!is.numeric(vx)) {
    vx  <- factor(as.character(vx))
    vx <- structure(as.numeric(vx), levels=levels(vx))
  }
  coln <- getval(title, attr(x, 'title'))
  tit  <- getval(attr(x, 'title'), title)
  na.action(convertTo(vx, coln=coln, rown=names(x), title=tit, out=out))
}

#' @rdname numeric_data
#' @export
numeric_data.matrix <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass, ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out) 
  vx  <- if (is.null(select)) x else x[,select,drop=FALSE]
  if (!is.numeric(vx)) {
    vx  <- apply(vx, 2, function(v) {
      as.numeric(factor(as.character(v)))
    })
  }
  tit <- getval(attr(x, 'title'), title, paste0(colnames(vx), collapse=","))
  na.action(convertTo(vx, coln=colnames(vx), rown=rownames(vx), title=tit, out=out))
}

#' @rdname numeric_data
#' @export
numeric_data.data.frame <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass, ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out) 
  vx  <- if (is.null(select)) x else x[,select,drop=FALSE]
  vx  <- lapply(vx, function(v) {
    if (is.numeric(v)) return(v)
    v <- factor(as.character(v))
    structure(as.numeric(v), levels=levels(v))
  })
  tit <- getval(attr(x, 'title'), title, paste0(names(vx), collapse=","))
  na.action(convertTo(as.data.frame(vx), coln=names(vx), rown=rownames(vx), title=tit, out=out))
}
