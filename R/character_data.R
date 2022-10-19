#' @rdname character_data
#' @title character_data
#' @description 
#' Converts a matrix or data frame into a character vector, matrix or data frame. If `na.action` is a character then
#' all `NA`s are replaced by `na.action` (default: `na.action="NA"`). If `na.action` is a function then the function will be
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
character_data <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action='NA',  ..., title=NULL) {
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
  for (i in 1:ncol(vx)) {
    vx[[i]] <- as.character(vx[[i]])
    if (is.character(na.action)) vx[[i]][is.na(vx[[i]])] <- na.action
  }
  if (is.function(na.action)) vx <- na.action(vx)
  rn <- rownames(vx)
  cn <- colnames(vx)
  if (out=="matrix") {
    vx <- matrix(unlist(vx), ncol=ncol(vx))
    rownames(vx) <- rn
    colnames(vx) <- cn
  }
  if (out=="vector") {
    vx <- do.call(c, vx)
    names(vx) <- apply(as.matrix(rev(expand.grid(rn, cn, stringsAsFactors = FALSE))), 1, paste, collapse=",")
  }
  tit <- getval(attr(x, 'title'), title, paste0(names(x), collapse=","))
  structure(vx, title=tit, out=out)
}
