#' convertTo 
#'
#' Converts an input object (vector, matrix or data frame) to an output asccording to the format in `out`. 
#' Variable, Row and column names are set, if possible as well as a attribute `title`.
#'
#' @param x vector, matrix, or data frame: input 
#' @param coln character: column names if possible
#' @param rown character: row names if possible
#' @param title character: for title attribute
#' @param out character: either 
#'
#' @return the desired output object
#' @export
#'
#' @examples
#' str(convertTo(pi, "Col1", "Row1", "Title", out='data.frame'))
#' str(convertTo(pi, "Col1", "Row1", "Title", out='matrix'))
#' str(convertTo(pi, "Col1", "Row1", "Title", out='vector'))
convertTo <- function(x, coln, rown, title, out=c("data.frame", "matrix", "vector")) {
  out <- match.arg(out)
  if (is.data.frame(x)) {
    if (is.null(coln)) coln <- names(x)
    if (is.null(rown)) rown <- getval(rownames(x), 1:nrow(x))
    if (out=='data.frame') {
      ret           <- x
      names(ret)    <- coln
      rownames(ret) <- rown
    }
    if (out=='matrix') {
      ret           <- matrix(unlist(x), ncol=ncol(x))
      colnames(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='vector') {
      ret        <- unlist(x)
#      g          <- expand.grid(rown, getval(coln, 1:ncol(x)))
#      names(ret) <- paste(g[,1], g[,2], sep=",")
      names(ret) <- rown
    }
  } else if (is.matrix(x)) {
    if (is.null(coln)) coln <- colnames(x)
    if (is.null(rown)) rown <- getval(rownames(x), 1:nrow(x))
    if (out=='data.frame') {
      ret           <- as.data.frame(x)
      names(ret)    <- coln
      rownames(ret) <- rown
    }
    if (out=='matrix') {
      ret <- x
      colnames(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='vector') {
      ret       <- x
      attributes(ret)    <- NULL
#      g          <- expand.grid(rown, getval(coln, 1:ncol(x)))
#      names(ret) <- paste(g[,1], g[,2], sep=",")
      names(ret) <- rown
    }
  } else { # vector
    if (is.null(rown)) rown <- getval(names(x), 1:length(x))
    if (out=='data.frame') {
      ret <- data.frame(x=x)
      names(ret)    <- coln
      rownames(ret) <- rown
    }
    if (out=='matrix') {
      ret <- matrix(unlist(x), ncol=1)
      colnames(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='vector') {
      ret        <- unlist(x)
      names(ret) <- rown
    }
  }
  attr(ret, 'title') <- getval(title, paste0(coln, collapse=","))
  ret
}