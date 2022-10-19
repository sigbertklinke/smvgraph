#' color_data
#'
#' Assigns a color to the data `x` based on the color palette `colpal`.
#'
#' @param x vector, matrix, or data frame
#' @param select vector: indicating columns to select (default: \code{1}) 
#' @param colpal color palette (default: [grDevices::hcl.colors])
#' @param title character: title attribute (default `NULL`)
#' @param ... further parameters to [factor_data]
#'
#' @return a color vector
#' @export
#'
#' @examples
#' color_data(iris)
#' color_data(iris$Species)
color_data <- function(x, colpal=grDevices::hcl.colors, select=NULL, ..., title=NULL) {
  stopifnot(length(x)>0)
  cx     <- factor_data(x, select=select, out='vector', ..., title=title)
  colors <- colpal(length(levels(cx))) 
  ret    <- colors[match(as.character(cx), levels(cx))]
  tit    <- getval(title, attr(cx, 'title'))
  structure(ret, title=tit, names=as.character(cx))
}
