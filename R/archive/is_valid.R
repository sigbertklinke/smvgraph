#' @rdname is_valid 
#' @aliases is_invalid
#' @aliases obs_valid
#' @title is_invalid, is_valid
#' @description Checks a data frame for valid values and returns a logical data frame with the same size.
#' Valid values for numeric variables are \code{is.finite(v)} and for other types \code{!is.na}.
#' \code{is_invalid} is the negation of \code{is_valid}. \code{obs_valid} retuns a logical vector if a row has only
#' valid values.
#'
#' @param x data frame
#'
#' @return a data frame with the same size as \code{x}
#' @export
#'
#' @examples
#' data("testdata")
#' is_invalid(testdata)
#' is_valid(testdata)
is_invalid <- function(x) {
  stopifnot("data.frame" %in% class(x)) 
  ret <- vector("list", ncol(x))
  for (i in 1:ncol(x)) {
      ret[[i]] <-  if (is.numeric(x[[i]])) !is.finite(x[[i]]) else is.na(x[[i]])
  }
  as.data.frame(ret, row.names=row.names(x), col.names=colnames(x))
}

#' @rdname is_valid
#' @export
is_valid <- function(x) {
  stopifnot("data.frame" %in% class(x)) 
  ret <- vector("list", ncol(x))
  for (i in 1:ncol(x)) {
    ret[[i]] <-  if (is.numeric(x[[i]])) is.finite(x[[i]]) else !is.na(x[[i]])
  }
  as.data.frame(ret, row.names=row.names(x), col.names=colnames(x))
}

#' @rdname is_valid
#' @export
obs_valid <- function(x) { rowSums(is_invalid(x))==0 }


