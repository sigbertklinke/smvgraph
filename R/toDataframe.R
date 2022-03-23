#' toDataframe
#'
#' Converts a table to a full data frame.
#'
#' @param obj R object (`table` or `ts`) to convert to a data frame
#' @param name character: vector of variable name(s), only use for a `ts` object
#' @param ...  further parameters given to [base::as.data.frame.table]
#'
#' @md
#' @return a data frame
#' @export
#'
#' @examples
#' toDataframe(Titanic)
#' toDataframe(austres)
toDataframe <- function(obj, name=NULL, ...) {
  stopifnot(is.table(obj) || stats::is.ts(obj) || is.data.frame(obj))
  df <- obj
  if (is.table(obj)) {
    df    <- as.data.frame(obj, ...)
    index <- rep(1:nrow(df), df$Freq)
    df    <- df[index,-ncol(df)]    
  }
  if (stats::is.ts(obj)) {
    if (is.null(name)) name <-paste(deparse(substitute(obj), 500), collapse = "\n")
    df <- structure(obj, names=name)
    df <- as.data.frame(df, )
  }
  df
}
