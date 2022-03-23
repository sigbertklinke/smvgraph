#' color_data
#'
#' Assigns a color to the data `x` based on the color palette `colpal`.
#'
#' @param x vector, matrix, or data frame
#' @param select vector: indicating columns to select (default: \code{1}) 
#' @param colpal color palette (default: [grDevices::hcl.colors])
#' @param title character: title attribute (default `NULL`)
#' @param ... further parameters to [group_data]
#'
#' @return a color vector
#' @export
#'
#' @examples
#' color_data(iris)
#' color_data(as.matrix(iris))
#' color_data(iris$Species)
color_data <- function(x, colpal=grDevices::hcl.colors, select=NULL, ..., title=NULL) {
  stopifnot(length(x)>0)
  #browser()
  cx     <- group_data(x, select=select, out='vector', ..., title=title)
  colors <- colpal(length(unique(cx))) 
  ret    <- colors[match(cx, unique(cx))]
  tit    <- getval(title, attr(cx, 'title'))
  convertTo(ret, rown=cx, coln=colnames(cx), out='vector', title=tit)
}
