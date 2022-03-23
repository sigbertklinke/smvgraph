#' shingle
#'
#' Creates a factor variable from a numerical variable. Possible methods are 
#' * `quantile` approximately the same number of observations in each level
#' * `distance` approximately the same interval length in each level
#' * `hclust` uses [stats::hclust] to find the intervals
#' @param x numeric: observations
#' @param n integer: number of levels
#' @param type character: method for level creation
#' @param depth integer: accuracy of breaks
#'
#' @return a factor variable
#' @importFrom FNN knnx.index
#' @export
#'
#' @examples
#' shingle(iris$Sepal.Width, 10, "quantile")
#' shingle(iris$Sepal.Width, 10, "distance")
#' shingle(iris$Sepal.Width, 10, "hclust")
shingle <- function(x, n, type=c("quantile", "distance", "hclust")) {
  inInterval <- function(val, interval) {
    cond <- outer(val, interval[,1], '<') & outer(val, interval[,1], '>')
    ret  <- NULL
    for (i in 1:nrow(interval)) {
      indi <- findInterval(val, interval[i,], left.open=TRUE)
      indi <- which(indi==1)
      if (length(indi)) ret  <- c(ret, median(val[indi]))
    }
    ret
  }
  #
  browser()
  type  <- match.arg(type)
  if (type=="quantile") cln <- findInterval(x, quantile(x, (0:n)/n, na.rm=TRUE), all.inside = TRUE)
  if (type=="distance") cln <- findInterval(x, seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=n+1), all.inside = TRUE)
  if (type=="hclust")   cln <- cutree(hclust(dist(x)), n) 
  lb      <- sort(tapply(x, cln, min, na.rm=TRUE))
  ub      <- sort(tapply(x, cln, max, na.rm=TRUE))
  borders <- cbind(ub[-length(ub)], lb[-1])
  df      <- min(borders[,2]-borders[,1])
  d       <- 10^ceiling(log10(df))
  rx      <- range(x, na.rm=TRUE)
  repeat {
    sbreaks <- d*(floor(rx[1]/d):ceiling(rx[2]/d))    #browser(
    breaks  <- inInterval(sbreaks, borders)
    if (length(breaks)==nrow(borders)) break
    sbreaks <- d/2*(floor(2*rx[1]/d):ceiling(2*rx[2]/d))
    breaks  <- inInterval(sbreaks, borders)
    if (length(breaks)==nrow(borders)) break
    sbreaks <- d/5*(floor(5*rx[1]/d):ceiling(5*rx[2]/d))
    breaks  <- inInterval(sbreaks, borders)
    if (length(breaks)==nrow(borders)) break
    d <- d/10
  }
  #browser()
  sbreaks <- c(sbreaks[1], breaks, sbreaks[length(sbreaks)])
  cut(x, sbreaks)
}