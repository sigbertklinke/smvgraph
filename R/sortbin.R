#' sortbin
#'
#' Sorts and bins the rows of the data frame \code{x} according the sorting columns in \code{sortCol}. 
#' \code{decreasing} and \code{na.last} are recycled is necessary. If \code{equibin} is \code{TRUE} 
#' and \code{nBins==NA} then \code{nBins} is set to \code{100}.  If \code{equibin} is \code{FALSE} 
#' and \code{nBins==NA} then the bins are returned as they come from sorting; only identical values are
#' in one bin. If \code{nBins} is positive then the bins are merged until \code{nBins} reached. 
#' Note that the numbers of observations per bin may vary.
#'
#' @param x data frame
#' @param sortCol numeric/character: names or indices of variable used for sorting (default: \code{1})
#' @param decreasing logical: should the sort order be increasing or decreasing (default: \code{FALSE})
#' @param na.last logical: for controlling the treatment of NAs (default: \code{TRUE})
#' @param nBins integer: maximal number of bins (default: \code{NA}). 
#' @param equibin logical: should the number of observations equal per bin (default: \code{TRUE})
#'
#' @return (non-sequential) bin numbers as integer
#' @export
#'
#' @examples
#' data("Boston", package="MASS")
#' tableplot(Boston, bin=sortbin(Boston))
#  tableplot(Boston, bin=sortbin(Boston, sortCol='chas')) 
#  tableplot(Boston, bin=sortbin(Boston, sortCol=c('chas', 'rad"), equibin=FALSE) 
sortbin <- function(x, sortCol=1, decreasing=FALSE, na.last=TRUE, nBins=NA, equibin=TRUE) {
  x <- as.data.frame(x)
  if (is.null(sortCol)) sortCol <- 1:ncol(x)
  if (is.numeric(sortCol))   sortCol <- intersect(sortCol, 1:ncol(x))
  if (is.character(sortCol)) sortCol <- intersect(sortCol, names(x))
  decreasing <- rep_len(as.logical(decreasing), length(sortCol))
  na.last    <- rep_len(as.logical(na.last), length(sortCol))
  #
  args <- list()
  for (i in seq_along(sortCol)) {
    xi <- x[, sortCol[i]]
    xi <- sort(xi, decreasing=decreasing[i], na.last=na.last[i])
    args[[i]] <- match( x[, sortCol[i]], xi[!duplicated(xi)])
  }
  index <- do.call(base::order, args)
  rownames(x) <- 1:nrow(x)
  x   <- x[index,sortCol,drop=FALSE]
  sx  <- apply(x, 1, toString)
  if (equibin) { # standard tableplot
    if (is.na(nBins)) nBins <- 100
    nBins <- min(nBins, length(sx))
    bin <- floor(seq(1, nBins+1, length.out=1+length(sx)))[1:length(sx)]
  } else {
    bin <- match(sx, unique(sx))
    if (!is.na(nBins)) {
      if (max(bin)>nBins) {
        repeat {
          tab  <- c('0'=nrow(x), table(bin))
          ntab <- length(tab)
          if ((ntab-1)<=nBins) break
          merger   <- cbind(tab[1:(ntab-1)]+tab[-1], 1:(ntab-1), 2:ntab)
          ind      <- which(merger[,1]==min(merger[,1]))
          if (length(ind)>1) ind <- sample(ind, 1)          
          bin[bin==as.integer(names(tab)[merger[ind,2]])] <- as.integer(names(tab)[merger[ind,3]])
        }
      }
    }
  }
  bin[as.integer(row.names(x))] <- bin
  as.integer(bin)
}