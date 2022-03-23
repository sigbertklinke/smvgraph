#' binData
#' 
#' Bins each variable in `data` in `bins` Bins. It can return a data frame (`out="data.frame"`), a table with the counts (`out="table"`), or
#' a table converted to a data frame with an additional variable `Freq`. The values can be either the bin mids (`val="mids"`) or 
#' the bin numbers (`val="interval"`). If possible all variables contain an attribute `breaks` with breaks used.
#' 
#' @param data object: a data.frame or object that can be converted to a data frame with variables to bin  
#' @param bins integer: number of bins, will be recycled if necessary
#' @param out character: output type, either `"data.frame"`, `"table"`, or `"binned"`
#' @param val character: values for outer, eiter `"mids"` (interval centers), or `"interval` (interval number)
#' @param pretty logical: should be [base::pretty] used or minimum and maximum (default: `TRUE`) 
#' @param numeric logical: return output a `factor` or as `numeric` (default: `TRUE`)
#'
#' @return a data frame or table with the results
#' @export
#'
#' @examples
#' df <- data.frame(x=runif(25), y=runif(25))
#' binData(df, 5, 'data.frame')
#' binData(df, 5, 'table')
#' binData(df, 5, 'binned')
binData <- function(data, bins, out=c("data.frame", "table", "binned"), val=c("mid", "interval"), pretty=TRUE, numeric=TRUE) {
  out  <- match.arg(out)
  val  <- match.arg(val)
  if (!is.data.frame(data)) data <- as.data.frame(data)
  bins <- rep(bins, length.out=ncol(data))
  ret  <- vector("list", ncol(data))
  for (i in 1:ncol(data)) {
      cr <- range(if (pretty) pretty(data[[i]]) else data[[i]])
      cb <- seq(cr[1], cr[2], length.out=bins[i]+1)
      ret[[i]] <- findInterval(data[[i]], cb, all.inside=TRUE)
      if (val=="mid") {
        mids     <- (cb[-1]+cb[-length(cb)])/2 
        ret[[i]] <- factor(mids[ret[[i]]], levels=mids)
      } else {
        ret[[i]] <- factor(ret[[i]], levels=1:bins[i]) 
      }
      attr(ret[[i]], 'breaks') <- cb
  }
  names(ret) <- names(data)
  if (out=="data.frame") {
    if (numeric) ret <- lapply(ret, function(v) { structure(as.numeric(as.character(v)), breaks=attr(v, 'breaks')) }) 
    return(as.data.frame(ret))
  }
  tab <- table(ret)
  if (out=="table") return(tab)
  df <- expand.grid(lapply(attr(tab, "dimnames"), as.numeric))
  for (i in 1:ncol(df)) {
    if (!numeric) df[[i]] <- factor(df[[i]])
    attr(df[[i]], "breaks") <- attr(ret[[i]], "breaks")
  }
  df$Freq <- as.vector(tab)
  structure(df, out.attrs=NULL)
}