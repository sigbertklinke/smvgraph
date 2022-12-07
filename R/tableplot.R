#' tableplot
#'
#' A tableplot is a visualisation of multivariate data sets. Each column represents a variable 
#' and each row bin is an aggregate of a certain number of records. For numeric variables, a value box is 
#' plotted with minimum, mean (black line) and maximum value. If any missing values in a bin of 
#' a numeric variable appear the box left from the value box is plotted in gray.
#' For categorical variables, a stacked bar chart is depicted of the proportions of categories. 
#' Missing values are taken into account.
#' 
#' The idea and some code of the tableplot is taken from \href{https://github.com/mtennekes/tabplot}{tableplot package}
#' by Martijn Tennekes and Edwin de Jonge.
#' It differs from their package by
#' 
#' * multicolumn sorting is possible, and
#' * no support for 'ff' (out of memory vectors).
#' 
#' @param x data frame
#' @param select numeric/character: variable to show in the plot (default: `NULL`)
#' @param subset numeric: index of observations to show
#' @param bin integer: bin numbers to which a observations belongs (default: `NULL` = all)
#' @param yj numeric: Yeo Johnson coefficient (default: `NA`). If `NA` then it will be set to 0 (=log) or 1 (=identity) 
#' @param IQR_bias numeric: parameter that determines when a logarithmic scale is used when \code{yj} is set to `NA`. The argument IQR_bias is multiplied by the interquartile range as a test.
#' @param colpal color palette to draw (default: `rainbow`)
#' @param color.NA_num color for missing of infinity values for numeric variables (default: `gray75`)
#' @param color.NA color for missing values for categorical variables (default: `grey75`)
#' @param color.num color for lower box for numeric variables (default: `lightblue`)
#' @param color.box color for upper box for numeric variables (default: `deepskyblue`)
#' @param color.line color for line in upper box for numeric variables  (default: `black`)
#' @param box.lower function: determine lower border in upper box for numeric variables (default: `NULL`). If `NULL` then `min(.,na.rm=TRUE)` is used.
#' @param box.upper function: determine upper border in upper box for numeric variables (default: `NULL`). If `NULL` then `max(.,na.rm=TRUE)` is used.
#' @param box.line function: determine line position in upper box for numeric variables (default: `NULL`). If `NULL` then `mean(.,na.rm=TRUE)` is used.
#' @param cex.main number: magnification to be used for the titles (default: `1`)
#' @param cex.legend number: magnification to be used for the legends (default: `1`)
#' @param width number: width of percentage axis (default: `1`). If `1` then the width is as wide as a plot.
#' @param height number: percentage of the height of the legends (default: `0.15`)
#' @return nothing
#' @references Tennekes, M., Jonge, E. de, Daas, P.J.H. (2013), Visualizing and Inspecting Large Datasets with Tableplots, Journal of Data Science 11 (1), 43-58.
#' @export
#'
#' @examples
#' data("Boston", package="MASS")
#' tableplot(Boston, bin=sortbin(Boston))
#  tableplot(Boston, bin=sortbin(Boston, sortCol='chas')) 
#  tableplot(Boston, bin=sortbin(Boston, sortCol=c('chas', 'rad"), equibin=FALSE) 
tableplot <- function(x, select=NULL, subset = NULL, bin=NULL,  yj=NA, IQR_bias=5, 
                      colpal       = grDevices::rainbow, 
                      color.NA_num = "gray75", 
                      color.NA     = "grey75", 
                      color.num    = "lightblue", 
                      color.box    = "deepskyblue", 
                      color.line   = "black",
                      box.lower    = NULL,
                      box.upper    = NULL,
                      box.line     = NULL,
                      cex.main     = 1,
                      cex.legend   = 1,
                      width        = 1,
                      height       = 0.15) {
  useLog <- function(x, IQR_bias) {
    # tabplot::scaleNumCol 
    quant <- stats::quantile(x, na.rm=TRUE)
    IQR   <- quant[4] - quant[2]
    ## simple test to determine whether scale is lin or log
    (quant[5]>0 && quant[5] > quant[4] + IQR_bias * IQR) || 
    (quant[1]<0 && quant[1] < quant[2] - IQR_bias * IQR)
  }
  #
  stopifnot(is.data.frame(x))
  # convert either to factor of numeric
  df  <- x
  for (i in 1:ncol(df)) {
    if (!is.numeric(df[[i]]) && !is.factor(df[[i]])) {
      df[[i]] <- factor(df[[i]])
    }
  }
  # bin
  if (is.null(bin)) bin <- sortbin(x, select)
  stopifnot(is.integer(bin))
  # subset and select
  if (is.null(select)) select <- 1:ncol(df)
  if (is.null(subset)) subset <- 1:nrow(df) else bin <- bin[subset]
  df <- df[subset,select]
  # adapt sortCol
  opar <- par(mar=c(0, 0, 2.1,0))
  # create basic plot
  graphics::layout(matrix(1:(2*ncol(df)+2), nrow=2, byrow=TRUE), heights=c(1-height, height),
         widths=c(width, rep(1, ncol(df))))
  on.exit({
    graphics::layout(1)
    par(opar)
  })
  plot(c(0,0), c(1,0), axes=FALSE, type="l", xlim=c(-1,0), ylim=c(0,1), xlab="", ylab="")
  for (i in seq(0,100,2)) lines(c(-0.03, 0), (1-i/100)*c(1,1))
  i <- (0:10)/10
  graphics::text(rep(0, length(i)), 1-i, labels=sprintf("%i%%", 100*i), pos=2)
  # reorder by sortColumn
  nbin <- table(sort(bin))
  ubin <- proportions(nbin) 
  # do plots
  yji <- rep_len(yj, ncol(df)) 
  ranges <- list()
  for (i in 1:ncol(df)) {
    dfi <- df[[i]]
    if (is.numeric(dfi)) {
      l <- if (is.na(yj[i])) 1-as.numeric(useLog(dfi, IQR_bias)) else yj[i]
      l <- round(2*l)/2
      if (l!=1) {
        dfi  <- yeo.johnson(dfi, l)
        main <- sprintf("yj(%s,%s)", names(df)[i], as.character(l))
      } else
        main <- names(df)[i]
      ranges[[i]]   <- range(dfi[is.finite(dfi)])
      plot(ranges[[i]], c(0, 1), type="n", axes=FALSE, main=main, xlab="", ylab="", cex.main=cex.main)
      by <- 1
      for (j in seq_along(ubin)) {
        dfij <- dfi[bin==as.integer(names(ubin)[j])]
        ifin     <- is.finite(dfij)
        dfij     <- dfij[ifin]
        dy       <- ubin[j]
        boxcolor <- if (any(!ifin)) color.NA_num else color.num
        if (length(dfij)) {
          bl <- if (is.null(box.lower)) min(dfij)  else box.lower(dfij)
          bc <- if (is.null(box.line))  mean(dfij) else box.line(dfij)
          bu <- if (is.null(box.upper)) max(dfij)  else box.upper(dfij)
          if (bu>bl) {
            graphics::polygon(c(ranges[[i]][1], bl, bl, ranges[[i]][1]), c(by, by, by-dy, by-dy), border=NA, col=boxcolor)
            graphics::polygon(c(bl, bu, bu, bl), c(by, by, by-dy, by-dy), border=NA, col=color.box)
          } else {
            graphics::polygon(c(ranges[[i]][1], bl, bl, ranges[[i]][1]), c(by, by, by-dy, by-dy), border=NA, col=boxcolor)
            lines(c(bl,bu), c(by, by-dy),  col=color.box)
          }
          lines(c(bc, bc), c(by, by-dy), col=color.line)
        } else {
          graphics::polygon(c(ranges[[i]][1], ranges[[i]][2], ranges[[i]][2], ranges[[i]][1]), c(by, by, by-dy, by-dy), border=NA, col=boxcolor)
        }
        by <- by-dy
      }
    } else {
      plot(c(0, 1), c(0, 1), type="n", axes=FALSE, main=names(df)[i], xlab="", ylab="")
      col <- c(colpal(length(levels(dfi))), color.NA)
      tab <- proportions(table(bin, dfi, exclude = NULL),1)
      by  <- 1
      for (j in seq_along(ubin)) {
        dy <- ubin[j]
        crs <- cumsum(tab[j,])
        for (k in seq_along(crs)) {
          if (k==1) 
            graphics::polygon(c(0, crs[k], crs[k], 0), c(by, by, by-dy, by-dy), border=NA, col=col[1])
          else
            graphics::polygon(c(crs[k-1], crs[k], crs[k], crs[k-1]), c(by, by, by-dy, by-dy), border=NA, col=col[k])
        }
        by <- by-dy
      }
      nrnt <- length(colnames(tab))
      if (nrnt<length(col)) col <- col[1:nrnt]
      ranges[[i]] <- cbind(colnames(tab), col)
    }
    #box()
  } 
  plot(c(0,1), c(1,0), axes=FALSE, type="n", xlab="", ylab="")
  graphics::text(rep(0,4), seq(0.2, 0.8, by=.2), 
       c(sprintf("n: %i", nrow(df)), sprintf('min: %i', min(nbin)),
         sprintf('max: %i', max(nbin)), sprintf("bins: %i", length(nbin))), pos=4)
  for (i in 1:ncol(df)) {
    if (is.numeric(ranges[[i]])){
      par(mar=c(0,0,0,0))
      plot(ranges[[i]], c(0,1), type="n", axes=FALSE)
      at <- graphics::axTicks(1)
      lines(range(at), c(1,1))
      for (ati in at) lines(c(ati, ati), c(1, 0.95))
      graphics::text(at, 0.95, at, pos=1, cex=cex.legend)
    } else {
      par(mar=c(0.1,0, 0, 0))
      plot(c(0,1), c(0,1), type="n", axes=FALSE)
      by <- 1
      dy <- 1/nrow(ranges[[i]])
      for (j in 1:nrow(ranges[[i]])) {
        graphics::polygon(c(0, 0.1, 0.1, 0), c(by, by, by-dy, by-dy), border=NA, col=ranges[[i]][j,2])
        graphics::text(0.2, by-dy/2, if(is.na(ranges[[i]][j,1])) "NA" else ranges[[i]][j,1], pos=4, cex=cex.legend)
        by <- by-dy
      }
    }
  }
}

