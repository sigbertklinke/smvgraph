#' color_hclust
#'
#' Determines colors for `x` based on [stats::hclust]. `x` is normalized according [normalize].
#'
#' @param x a numeric matrix, data frame or "dist" object.
#' @param normalize integer: normalization method (default: \code{1})
#' * 0: no rescaling
#' * 1: \eqn{(x-min(x))/(max(x)-min(x))}
#' * 2: \eqn{(x-mean(x))/sd(x)}
#' @param ncol integer: maximal number colors
#' @param colpal color palette: a function which generates "ncol" colors with "colpal(ncol)" (default: [grDevices::hcl.colors])
#' @param na.action a function which indicates what should happen when the data contain NAs (default: \code{na.pass})
#' @param dist the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra" or "binary"(default: `euclidean`)
#' @param ... further parameters given to [stats::hclust]
#'
#' @return a color vector
#' @export
#'
#' @examples
#' color_hclust(iris[,-5], ncol=6)
color_hclust <- function(x, normalize=1, ncol=2, colpal=grDevices::hcl.colors, dist="euclidean", na.action=stats::na.pass, ...) {
  nx  <- colnames(x)
  if (is.null(nx)) nx <- attr(x, "title")
  x   <- numeric_data(x, na.action=na.action)
  x   <- normalize(x, method=normalize)
  hx  <- stats::hclust(dist(x, method=dist), ...) 
  k   <- stats::cutree(hx, k=ncol)
  structure(color_data(k, colpal=colpal), title="hclust", names=as.character(k))
}
