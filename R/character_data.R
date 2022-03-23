#' @rdname character_data
#' @title character_data
#' @description 
#' Converts a matrix or data frame into a character vector, matrix or data frame. If `na.action` is a character then
#' all `NA`s are replaced by `na.action` (default: na.action="NA"`). If `na.action` is a function then the function will be
#' applied to the result.
#' @param x vector, matrix or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param title character: title attribute (default `NULL`)
#' @param na.action function or character: indicates what should happen when the data contain NAs 
#' @param ... unused
#'
#' @return the desired R object
#' @export
#'
#' @examples
#' character_data(iris)
#' character_data(iris, out="matrix")
#' character_data(iris, out="vector")
character_data <- function(x, ...) { UseMethod("character_data") }

#' @rdname character_data
#' @export
character_data.default <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action='NA',  ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out) 
  vx  <- unlist(x)
  if (!is.character(vx)) {
    vx  <- factor(as.character(vx))
    vx  <- structure(as.character(vx), levels=levels(vx))
    if (is.character(na.action)) vx[is.na(vx)] <- na.action
  }
  coln <- if (is.null(title)) attr(x, 'title') else title
  if (is.function(na.action)) vx <- na.action(vx)
  tit <- getval(attr(x, 'title'), title)
  convertTo(vx, coln=coln, rown=names(vx), title=tit, out=out)
}

#' @rdname character_data
#' @export
character_data.matrix <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action='NA', ..., title=NULL) {
  stopifnot(length(x)>0)
  out <- match.arg(out) 
  vx  <- if (is.null(select)) x else x[,select,drop=FALSE]
  if (!is.numeric(vx)) {
    vx  <- apply(vx, 2, function(v) {
      if (!is.character(v)) v <- as.character(v)
      if (is.character(na.action)) v[is.na(v)] <- na.action
      v
    })
  }
  if (is.function(na.action)) vx <- na.action(vx)
  tit <- getval(attr(x, 'title'), title, paste0(colnames(vx), collapse=","))
  convertTo(vx, coln=colnames(x), rown=rownames(x), title=tit, out=out)
}

#' @rdname character_data
#' @export
character_data.data.frame <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action='NA', ..., title=NULL) {
  stopifnot(length(x)>0)
  #browser()
  out <- match.arg(out) 
  vx  <- if (is.null(select)) x else x[,select,drop=FALSE]
  vx  <- lapply(vx, function(v) {
    if (!is.character(v)) v <- as.character(v)
    if (is.character(na.action)) v[is.na(v)] <- na.action
    v
  })
  if (is.function(na.action)) vx <- na.action(vx)
  tit <- getval(attr(x, 'title'), title, paste0(names(vx), collapse=","))
  convertTo(as.data.frame(vx), coln=colnames(vx), rown=rownames(vx), title=tit, out=out)
}
