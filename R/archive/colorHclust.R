#' colorHclust
#'
#' Determines colors for `x` based on [hclust]. `x` is normalized according [normalize].
#'
#' @param x a numeric matrix, data frame or "dist" object.
#' @param normalize integer: normalization method (default: \code{1})
#' * 0: no rescaling
#' * 1: \eqn{(x-min(x))/(max(x)-min(x))}
#' * 2: \eqn{(x-mean(x))/sd(x)}
#' @param ncol integer: maximal number colors
#' @param colpal color palette: a function which generates "ncol" colors with "colpal(ncol)" (default: [grDevices::rainbow])
#' @param dist the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra" or "binary"(default: `euclidean`)
#' @param ... further parameters given to [stats::hclust]
#'
#' @return a color vector
#' @export
#'
#' @examples
#' colorHclust(iris[,-5], ncol=6)
colorHclust <- function(x, normalize=1, ncol=2,  colpal=rainbow, dist="euclidean", ...) {
  x  <- normalize(x, method=normalize)
  hx <- hclust(dist(x, method=dist), ...) 
  k  <- cutree(hx, k=ncol)
  colpal(ncol)[k]
}