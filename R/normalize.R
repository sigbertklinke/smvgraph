#' normalize
#'
#' Extracts the numeric vectors from a data frame and normalizes each vector. 
#' Note: In case that a variable is constant for `method==1` (minmax) the entries will be replaced by `0.5` and  
#' for `method==2` (standardization) the entries will be replaced by `0`.
#'
#' @param x data.frame or matrix
#' @param method integer: normalization method (default: \code{1})
#' * 0: no rescaling
#' * 1: \eqn{(x-min(x))/(max(x)-min(x))}
#' * 2: \eqn{(x-mean(x))/sd(x)}
#' @seealso In package \code{\link[andrews]{normalize}} or at \href{https://CRAN.R-project.org/package=andrews}{CRAN}
#'
#' @md
#' @return numeric matrix
#' @export
#'
#' @examples
#' normalize(iris, 2)
normalize <- function(x, method=1) {
  if (is.data.frame(x)) x <- as.matrix(x[,sapply(x, is.numeric)])
  stopifnot("matrix" %in% class(x))
  if (is.null(colnames(x))) colnames(x) <- sprintf("V%.0f", 1:ncol(x))
  if (method==1) {
    mi <- apply(x, 2, min, na.rm=TRUE)
    ma <- apply(x, 2, max, na.rm=TRUE)
    x  <- scale(x, center=mi, scale=ma-mi)
    x[is.na(x)] <- 0.5
  }
  if (method==2) {
    x <- scale(x)
    x[is.na(x)] <- 0
  }
  x
}
