#' @rdname valid
#' @aliases invalid
#' @title valid, invalid
#' @description Computes the number a logical matrix or vector if all values are valid in `x` or each colum or row of `x`. Valid values for numeric variables are \code{is.finite(v)} and for other types \code{!is.na}
#' @param x object: anything taht can be coerced to a data frame checked for valid/invalid values
#' @param margin integer: a vector giving the subscripts for which valid/invalid values looked for, e.g. `margin==1` indicates rows, `margin==2` indicates columns, otherwise indicates rows and columns.
#' @param n logical: should just the number of valid/invalid values returned or a logical matrix/vector
#'
#' @return a logical data frame, a logical vector or an integer
#' @export
#'
#' @examples
#' data("testdata")
#' valid(testdata)             # matrix with logical entries if x has valid entry
#' valid(testdata, n=TRUE)     # number of valid entries in x
#' valid(testdata, 1)          # vector with logical entries if each row if x has valid entries
#' valid(testdata, 1, n=TRUE)  # number of rows with valid entries in x
#' valid(testdata$xu)
valid <- function (x, margin=1:2, n=FALSE) {
  xname <- deparse(substitute(x))
  if (is.matrix(x) || is.list(x)) x <- as.data.frame(x)
  if (!is.data.frame(x)) {
    x <- as.data.frame(as.vector(x), ncol=1, row.names=names(x))
    names(x) <- make.names(xname)
  }
  rn <- rownames(x)
  if (is.null(rn)) rn <- sprintf("%i", 1:nrow(x))
  cn <- colnames(x)
  if (is.null(cn)) cn <- sprintf("V%i", 1:ncol(x))
  ret <- matrix(NA, nrow=nrow(x), ncol=ncol(x), dimnames=list(rn, cn))
  for (i in 1:ncol(x)) {
    ret[,i] <-  if (is.numeric(x[[i]])) is.finite(x[[i]]) else !is.na(x[[i]])
  }
  if (isTRUE(all(margin==1))) ret <- (rowSums(ret)==ncol(x))
  if (isTRUE(all(margin==2))) ret <- (colSums(ret)==nrow(x))
  if (n) return(sum(ret))
  ret
}

#' @rdname valid
#' @description Computes the number a logical matrix or vector if any values are valid in `x` or each colum or row of `x`.
#' @export
invalid <- function (x, margin=1:2, n=FALSE) {
  xname <- deparse(substitute(x))
  if (is.matrix(x) || is.list(x)) x <- as.data.frame(x)
  if (!is.data.frame(x)) {
    x <- as.data.frame(as.vector(x), ncol=1, row.names=names(x))
    names(x) <- make.names(xname)
  }
  rn <- rownames(x)
  if (is.null(rn)) rn <- sprintf("%i", 1:nrow(x))
  cn <- colnames(x)
  if (is.null(cn)) cn <- sprintf("V%i", 1:ncol(x))   
  ret <- matrix(NA, nrow=nrow(x), ncol=ncol(x), dimnames=list(rn, cn))
  for (i in 1:ncol(x)) {
    ret[,i] <-  if (is.numeric(x[[i]])) !is.finite(x[[i]]) else is.na(x[[i]])
  }
  if (isTRUE(all(margin==1))) ret <- (rowSums(ret)>0)
  if (isTRUE(all(margin==2))) ret <- (colSums(ret)>0)
  if (n) return(sum(ret))
  ret
}