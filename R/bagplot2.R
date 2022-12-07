#' bagplot
#'
#' A non-ggplot2 bagplot based on [mrfDepth::bagplot]. 
#'
#' @param x,y the x and y arguments provide the x and y coordinates for the bagplot. 
#' Any reasonable way of defining the coordinates is acceptable. See the function \code{xy.coords}  
#' for details. If supplied separately, they must be of the same length.
#' @inheritParams mrfDepth::bagplot
#' @inheritParams mrfDepth::compBagplot
#' @param ... further parameters given to `plot`
#' @details The bagplot has been proposed by Rousseeuw et al. (1999) as a generalisation of the 
#' boxplot to bivariate data. It is constructed based on halfspace depth and as such is invariant 
#' under affine transformations. Similar graphical representations can be obtained by means of other 
#' depth functions, as illustrated in Hubert and Van der Veeken (2008) and in Hubert et al. (2015). 
#' See [mrfDepth::compBagplot] for more details.
#' 
#' The deepest point is indicated with a "*" sign, the outlying observations with red points.
#' @seealso [mrfDepth::compBagplot] and [mrfDepth::bagplot]
#' @references Rousseeuw P.J., Ruts I., Tukey, J.W. (1999). The bagplot: a bivariate boxplot. The American Statistician, 53, 382–387.
#' 
#' Hubert M., Van der Veeken S. (2008). Outlier detection for skewed data. Journal of Chemometrics, 22, 235–246.
#' 
#' Hubert M., Rousseeuw P.J., Segaert, P. (2015). Rejoinder to 'Multivariate functional outlier detection'. Statistical Methods & Applications, 24, 269–277.
#' @importFrom grDevices chull extendrange rgb xy.coords
#' @importFrom graphics points polygon
#' @importFrom mrfDepth compBagplot
#' @return Invisibly the result of the call to [mrfDepth::compBagplot]
#' @export
#'
#' @examples
#' bagplot2(iris$Sepal.Length, iris$Sepal.Width)
#' bagplot2(iris[,1:2])
#' bagplot2(iris[,3:4], title="Bagplot with Tukey depth", xlab="Petal.Length", ylab="Petal.Width") 
#' #
#' library("mrfDepth")
#' data("bloodfat")
#' result <- compBagplot(bloodfat)
#' bagplot(result, colorbag = rgb(0.2,0.2,0.2), colorloop = "green")
bagplot2 <- function (x, y=NULL, colorbag = NULL, colorloop = NULL, 
                      colorchull = NULL, databag = TRUE, dataloop = TRUE, plot.fence = FALSE,
                      type = "hdepth", sizesubset = 500,
                      extra.directions = FALSE, options = NULL, ...) { 
  if (isTRUE(all(c('mrfDepth', 'compBagplot') %in% class(x)))) {
    compbag.result <- x
    xlabel <- colnames(compbag.result$datatype)[1]
    ylabel <- colnames(compbag.result$datatype)[2]
  } else {
    xlabel <- if (!missing(x)) deparse1(substitute(x))
    ylabel <- if (!missing(y)) deparse1(substitute(y))
    x <- xy.coords(x, y, xlabel, ylabel)
    compbag.result <- compBagplot(cbind(x$x, x$y), type, sizesubset, extra.directions, options)
    stopifnot("mrfDepth" %in% class(compbag.result)) 
    stopifnot("compBagplot" %in% class(compbag.result))
  }
  if (is.null(colorbag)) colorbag <- rgb(0.6, 0.6, 1)
  if (is.null(colorloop)) colorloop <- rgb(0.8, 0.8, 1)
  if (is.null(colorchull)) colorchull <- rgb(1, 1, 1)
  #
  ind.bag <- which(compbag.result$datatype[, 3] == 1)
  data.bagcontour <- data.frame(compbag.result$bag)
  data.inbag <- data.frame(compbag.result$datatype[ind.bag, 1:2, drop = FALSE])
  ind.loop <- which(compbag.result$datatype[, 3] == 2)
  data.inloop <- data.frame(compbag.result$datatype[ind.loop, 1:2, drop = FALSE])
  data.infence <- data.frame(compbag.result$fence)
  ind.outl <- which(compbag.result$datatype[, 3] == 3)
  data.outliers <- data.frame(compbag.result$datatype[ind.outl, 1:2, drop = FALSE])
  #browser()
  colnames(data.infence)    <- c("x", "y")
  colnames(data.inloop)     <- c("x", "y")
  colnames(data.inbag)      <- c("x", "y")
  colnames(data.bagcontour) <- c("x", "y")
  colnames(data.outliers)   <- c("x", "y")
  label.x <- colnames(compbag.result$datatype)[1]
  label.y <- colnames(compbag.result$datatype)[2]
  #plot <- ggplot()
  boxes <- list()
  if (plot.fence) {
    data.reg <- data.infence[chull(data.infence), ]
    colnames(data.reg) <- c("x", "y")
    data.reg <- rbind(data.reg, data.reg[1, ])
    boxes[["plot.fence"]] <- data.reg
    #    plot <- plot + geom_path(data = data.reg, mapping = aes_string(x = "x", 
    #                                                                   y = "y"), linetype = "dashed")
  }
  data.reg <- data.inloop[chull(data.inloop), ]
  colnames(data.reg) <- c("x", "y")
  boxes[["data.inloop"]] <- data.reg
  #  plot <- plot + geom_polygon(data = data.reg, mapping = aes_string(x = "x", 
  #                                                                    y = "y"), fill = colorloop)
  data.reg <- data.bagcontour[chull(data.bagcontour), ]
  colnames(data.reg) <- c("x", "y")
  boxes[["data.bagcontour"]] <- data.reg
  #  plot <- plot + geom_polygon(data = data.reg, mapping = aes_string(x = "x", 
  #                                                                    y = "y"), fill = colorbag)
  if (nrow(compbag.result$chull) > 1) {
    data.chull <- data.frame(compbag.result$chull)
    colnames(data.chull) <- c("x", "y")
    boxes[["data.chull"]] <- data.chull
    #    plot <- plot + geom_polygon(data = data.chull, mapping = aes_string(x = "x", 
    #                                                                        y = "y"), fill = colorchull)
  }
  data.reg <- data.frame(matrix(compbag.result$center, ncol = 2))
  colnames(data.reg) <- c("x", "y")
  boxes[["compbag.result"]] <- data.reg
  #  plot <- plot + geom_point(data = data.reg, mapping = aes_string(x = "x", 
  #                                                                  y = "y"), shape = 23, size = 4, color = "red", fill = "red")
  if (databag) {
    boxes[["data.inbag"]] <- data.inbag
    #    plot <- plot + geom_point(data = data.inbag, mapping = aes_string(x = "x", 
    #                                                                      y = "y"))
  }
  if (dataloop) {
    boxes[["data.loop"]] <- data.inloop   
    #    plot <- plot + geom_point(data = data.inloop, mapping = aes_string(x = "x", 
    #                                                                       y = "y"))
  }
  if (nrow(data.outliers) > 0) {
    boxes[["data.outliers"]] <- data.outliers   
    #    plot <- plot + geom_point(data = data.outliers, mapping = aes_string(x = "x", 
    #                                                                         y = "y"), shape = 8, color = "red")
  }
  x.range <- y.range <- c(Inf, -Inf)
  for (i in 1:length(boxes)) {
    ri <- apply(boxes[[i]], 2, range) 
    if (ri[1,1]<x.range[1]) x.range[1] <- ri[1,1]
    if (ri[2,1]>x.range[2]) x.range[2] <- ri[2,1]  
    if (ri[1,2]<y.range[1]) y.range[1] <- ri[1,2]
    if (ri[2,2]>y.range[2]) y.range[2] <- ri[2,2]  
  }
  #  plot.data <- compbag.result$datatype[, -3]
  #  colnames(plot.data) <- c("x", "y")
  #  plot.data <- rbind(plot.data, data.infence)
  x.range <- extendrange(x.range, f = 0.05)
  y.range <- extendrange(y.range, f = 0.05)
  args <- list(...)
  args$x <- x.range
  args$y <- y.range
  args$type <- "n"
  if (is.null(args$xlab)) args$xlab <- xlabel
  if (is.null(args$ylab)) args$ylab <- xlabel
  if (is.null(args$cex)) args$cex <- 1
  do.call("plot", args)
  #  plot <- plot + coord_cartesian(xlim = x.range, ylim = y.range)
  #  plot <- plot + mrfDepth_theme()
  #  plot <- plot + xlab(label.x) + ylab(label.y)
  #  if (compbag.result$type == "hdepth") {
  #    label.title <- paste("Bagplot based on halfspace depth")
  #  }
  #  else if (compbag.result$type == "projdepth") {
  #    label.title <- paste("Bagplot based on projection depth")
  #  }
  #  else if (compbag.result$type == "sprojdepth") {
  #    label.title <- paste("Bagplot based on skewness-adjusted projection depth")
  #  }
  #  else {
  #    label.title <- paste("Bagplot based on", compbag.result$type, 
  #                         "depth")
  #  }
  #  plot <- plot + ggtitle(label.title) + theme(plot.title = element_text(hjust = 0.5))
  #  
  #  return(plot)
  if (!is.null(boxes[["plot.fence"]])) polygon(boxes[["plot.fence"]], lty="dashed")
  if (!is.null(boxes[["data.inloop"]])) polygon(boxes[["data.inloop"]], col=colorloop, border=NA)
  if (!is.null(boxes[["data.bagcontour"]])) polygon(boxes[["data.bagcontour"]], col=colorbag, border=NA)
  if (!is.null(boxes[["data.chull"]])) polygon(boxes[["data.chull"]], col=colorchull, border=NA)
  if (!is.null(boxes[["compbag.result"]])) points(boxes[["compbag.result"]], pch=8, cex=args$cex, col="red")
  if (!is.null(boxes[["data.inbag"]])) points(boxes[["data.inbag"]], pch=19, cex=0.5*args$cex)
  if (!is.null(boxes[["data.loop"]])) points(boxes[["data.loop"]], pch=19, cex=0.5*args$cex)
  if (!is.null(boxes[["data.outliers"]])) points(boxes[["data.outliers"]], pch=19, cex=0.5*args$cex, col="red")
  invisible(compbag.result)
}