#' toDataframe
#'
#' Converts a table to a full data frame.
#'
#' @param obj R object (`table` or `ts`) to convert to a data frame
#' @param name character: vector of variable name(s), only use for a `ts` object
#' @param ...  further parameters given to [base::as.data.frame.table]
#'
#' @md
#' @return a data frame with `sum(tab)` rows and `length(dim(tab))` cols
#' @export
#'
#' @examples
#' table2dataframe(Titanic)
table2dataframe <- function(obj, name=NULL, ...) {
  stopifnot(is.table(obj) || is.ts(obj) || is.data.frame(obj))
  df <- obj
  if (is.table(obj)) {
    df    <- as.data.frame(tab, ...)
    index <- rep(1:nrow(df), df$Freq)
    df[index,-ncol(df)]    
  }
  if (is.ts(obj)) {
    if (is.null(name)) name <-paste(deparse(substitute(obj), 500), collapse = "\n")))
    df <- structure(obj, names=name)
    df <- as.data.frame(df, )
  }
  df
}
