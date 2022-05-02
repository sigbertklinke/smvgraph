#' yeo.johnson
#'
#' Computes the Yeo-Johnson transformation, which is a normalizing transformation. The code and 
#' documentation is taken from the \href{https://CRAN.R-project.org/package=VGAM}{VGAM} package 
#' (see function \code{yeo.johnson}) with some slight modifications, e.g. \code{NA}'s are kept and 
#' do not produce an error.
#'
#' @param y numeric: a vector or matrix.
#' @param lambda numeric: It is recycled to the same length as \code{y} if necessary. 
#' @param derivative non-negative integer: the default is the ordinary function evaluation, 
#' otherwise the derivative with respect to \code{lambda} (default: \code{0})
#' @param epsilon numeric and positive value: the tolerance given to values of \code{lambda} when comparing it to 0 or 2. 
#' @param inverse logical: return the inverse transformation? (default: \code{FALSE})
#'
#' @details The Yeo-Johnson transformation can be thought of as an extension
#' of the Box-Cox transformation. It handles both positive and
#' negative values, whereas the Box-Cox transformation only handles
#' positive values. Both can be used to transform the data so
#' as to improve normality.
#' @references Yeo, I.-K. and Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. \emph{Biometrika}, \bold{87}, 954--959.
#' @note If \code{inverse = TRUE} then the argument \code{derivative = 0} is required.
#' @seealso \code{VGAM::yeo.johnson}, \code{\link[MASS]{boxcox}}.
#' @return The Yeo-Johnson transformation or its inverse, or its derivatives with respect to 
#' \code{lambda}, of \code{y}.
#' @export
#'
#' @examples
#' y <- seq(-4, 4, len = (nn <- 200))
#' ltry <- c(0, 0.5, 1, 1.5, 2)  # Try these values of lambda
#' lltry <- length(ltry)
#' psi <- matrix(as.numeric(NA), nn, lltry)
#' for (ii in 1:lltry)
#'   psi[, ii] <- yeo.johnson(y, lambda = ltry[ii])
#' matplot(y, psi, type = "l", ylim = c(-4, 4), lwd = 2, lty = 1:lltry,
#'         ylab = "Yeo-Johnson transformation", col = 1:lltry, las = 1,
#'         main = "Yeo-Johnson transformation with some values of lambda")
#' abline(v = 0, h = 0)
#' legend(x = 1, y = -0.5, lty = 1:lltry, legend = as.character(ltry),
#'        lwd = 2, col = 1:lltry)
yeo.johnson <- function(y, lambda, derivative = 0,
                        epsilon = sqrt(.Machine$double.eps),
                        inverse = FALSE) {
  nay    <- is.na(y)
  y[nay] <- 1
  if (!all(is.numeric(derivative), length(derivative)==1, as.integer(derivative)==derivative, derivative>=0))
    stop("argument 'derivative' must be a non-negative integer")
  ans <- y
  if (!all(is.numeric(epsilon), length(epsilon)==1, epsilon>0))
    stop("argument 'epsilon' must be a single positive number")
  L <- max(length(lambda), length(y))
  if (length(y)      != L) y      <- rep_len(y,      L)
  if (length(lambda) != L) lambda <- rep_len(lambda, L)
  
  if (inverse) {
    if (derivative != 0)
      stop("argument 'derivative' must 0 when inverse = TRUE")
    if (any(index <- y >= 0 & abs(lambda  ) >  epsilon))
      ans[index] <- (y[index]*lambda[index] + 1)^(1/lambda[index]) - 1
    if (any(index <- y >= 0 & abs(lambda  ) <= epsilon))
      ans[index] <- expm1(y[index])
    if (any(index <- y <  0 & abs(lambda-2) >  epsilon))
      ans[index] <- 1 - (-(2-lambda[index]) *
                           y[index]+1)^(1/(2-lambda[index]))
    if (any(index <- y <  0 & abs(lambda-2) <= epsilon))
      ans[index] <- -expm1(-y[index])
    return(ans)
  }
  if (derivative == 0) {
    if (any(index <- y >= 0 & abs(lambda  ) >  epsilon))
      ans[index] <- ((y[index]+1)^(lambda[index]) - 1) / lambda[index]
    if (any(index <- y >= 0 & abs(lambda  ) <= epsilon))
      ans[index] <- log1p(y[index])
    if (any(index <- y <  0 & abs(lambda-2) >  epsilon))
      ans[index] <- -((-y[index]+1)^(2-lambda[index]) - 1)/(2 -
                                                              lambda[index])
    if (any(index <- y <  0 & abs(lambda-2) <= epsilon))
      ans[index] <- -log1p(-y[index])
  } else {
    psi <- Recall(y = y, lambda = lambda, derivative = derivative - 1,
                  epsilon = epsilon, inverse = inverse)
    if (any(index <- y >= 0 & abs(lambda  ) >  epsilon))
      ans[index] <- ( (y[index]+1)^(lambda[index]) *
                        (log1p(y[index]))^(derivative) - derivative *
                        psi[index] ) / lambda[index]
    if (any(index <- y >= 0 & abs(lambda  ) <= epsilon))
      ans[index] <- (log1p(y[index]))^(derivative + 1) / (derivative + 1)
    if (any(index <- y <  0 & abs(lambda-2) >  epsilon))
      ans[index] <- -( (-y[index]+1)^(2-lambda[index]) *
                         (-log1p(-y[index]))^(derivative) - derivative *
                         psi[index] ) / (2-lambda[index])
    if (any(index <- y <  0 & abs(lambda-2) <= epsilon))
      ans[index] <- (-log1p(-y[index]))^(derivative + 1) / (derivative + 1)
  }
  ans[nay] <- NA
  ans
}
