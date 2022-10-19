#' jitter_min
#'
#' Add a small amount of noise to a numeric vector. The result is \code{x + runif(n, -a, a)} where \code{n <- length(x)} 
#' and \code{a <- abs(factor*amount)} argument. If \code{amount==0} then \code{amount} 
#' is set to \code{1e-6} times the smallest non-zero distance between adjacent unique \code{x} values. 
#' In case of no non-zero distances \code{amount} is set to \code{1e-6*(1+min(abs(x)))}. 
#' Note that \code{jitter_min} delivers different results then [base::jitter].
#' 
#' @param x numeric: vector to which jitter should be added
#' @param factor numeric: multiplier for \code{amount} (default: \code{1})  
#' @param amount numeric: amount for jittering (default: \code{0})  
#'
#' @return jittered data
#' @export
#'
#' @examples
#' jitter_min(runif(6))
#' jitter_min(rep(0, 7))
#' jitter_min(rep(10000, 5))
jitter_min <- function(x, factor = 1, amount = 0) {
  jitter1 <- function(x, factor = 1, amount = 0) {
    if (length(x) == 0L) return(x)
    if (!is.numeric(x)) stop("'x' must be numeric")
    if (amount==0) {
      ux <- unique(x)
      if (length(ux)>1) {
        d      <- diff(sort(ux))
        amount <- 1e-4*min(d[d>0]) 
      } else {
        amount <- 1e-4*(1+abs(ux))
      }
    }
    amount <- abs(factor*amount)
    x+stats::runif(length(x), -amount, +amount)
  }
  #
  if (is.data.frame(x) || is.matrix(x)) {
    factor <- rep_len(factor, ncol(x))
    amount <- rep_len(amount, ncol(x))
    for (i in 1:ncol(x)) x[,i] <- jitter1(x[,i], factor[i], amount[i])
  } else {
    x <- jitter1(x, factor, amount)
  }
  x
}