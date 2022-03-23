#' pyramid
#'
#' @param tab table: a table with two columns 
#' @param gap numeric(2): relative size of gap in `y`- and `x`-direction (default: `c(0,0)`)
#' @param left list: parameters for the left polygons (default: `list(col="red")`)
#' @param right list: parameters for the right polygons (default: `list(col="blue")`)
#' @param ... further parameters to use in [graphics::plot.default]
#'
#' @return a pyramid plot
#' @export
#'
#' @examples
#' data("Boston", package="MASS")
#' tab <- table(data.frame(Boston$rad, Boston$chas))
#' pyramid(tab, main="Absolute frequencies")
#' pyramid(tab, gap=c(0.2, 0.2))
#' rtab <- tab/sum(tab)
#' pyramid(rtab, gap=c(0.2, 0.2), main="Relative frequencies")
#' ctab <- proportions(tab, 2)
#' pyramid(ctab, gap=c(0.2, 0.2), main="Conditional frequencies on columns")
#' rtab <- proportions(tab, 1)
#' pyramid(rtab, gap=c(0.2, 0.2), main="Conditional frequencies on rows")
#' # zebraing 
#' pyramid(tab, gap=c(0.2, 0.2), 
#'         left=list(list(col="black"), list(col="white")), 
#'         right=list(list(col="blue"), list(col="green")))
pyramid <- function(tab, gap=0, left=list(col="red"), right=list(col="blue"), ...) {
  stopifnot(length(dim(tab))==2, ncol(tab)==2)
  if (length(gap)==1) gap <- c(gap, 0)
  maxx <- max(pretty(tab))
  maxy <- (nrow(tab)+1)*(1+gap[1])-gap[1]
  args <- list(...)
  tdn  <- attr(tab, "dimnames")
  if (is.null(args$xlab)) args$xlab <- if (is.null(tdn)) "Columns" else names(tdn)[2]
  if (is.null(args$ylab)) args$ylab <- if (is.null(tdn)) "Rows" else names(tdn)[1]
  gapx   <- gap[2]/2*maxx
  args$x <- c(-maxx-gapx, maxx+gapx)
  args$y <- c(0, maxy)
  args$type <- "n"
  args$axes <- FALSE
  do.call(graphics::plot.default, args)
  left  <- rep(if (is.list(left[[1]])) left else list(left), length.out=nrow(tab))
  right <- rep(if (is.list(right[[1]])) right else list(right), length.out=nrow(tab))
  y <- 0
  for (i in 1:nrow(tab)) {
    args <- left[[i]]
    args$x <- -gapx+c(0, -tab[i,1], -tab[i,1], 0)
    args$y <- c(y, y, y+1, y+1)
    do.call(graphics::polygon, args)
    args <- right[[i]]
    args$x <- gapx+c(0, tab[i,2], tab[i,2], 0)
    args$y <- c(y, y, y+1, y+1)
    do.call(graphics::polygon, args)
    y <- y+(1+gap[1])
  }
  coln <- if(is.null(colnames(tab))) as.character(1:2) else colnames(tab)
  graphics::text(-gapx, y+0.5, colnames(tab)[1], pos=2)
  graphics::text(gapx, y+0.5, colnames(tab)[2], pos=4)    
  rown <- if(is.null(rownames(tab))) as.character(1:nrow(tab)) else rownames(tab)
  if (gap[2]>0) {
    graphics::text(0, 0.5+((1:nrow(tab))-1)*(1+gap[1]), rown)
  } else {
    graphics::axis(2, at=0.5+((1:nrow(tab))-1)*(1+gap[1]), rown, las=1)
  }
  atx <- pretty(c(0, tab))
  graphics::axis(1, at=c(-rev(gapx+atx), gapx+atx), c(rev(atx), atx))
  graphics::box()
}
