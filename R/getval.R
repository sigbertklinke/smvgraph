#' getval
#'
#' Returns `val` if `length(val)>1`. Otherwise it runs through `args=list(...)` until it finds an element with
#' `length(args[[i]])>0` and returns it. If everything fails `NULL` will be returned.
#'
#' @param val current value
#' @param ... sequence of alternative values
#'
#' @return a value
#' @export
#'
#' @examples
#' getval(NULL, 0)
#' getval(1, 0)
getval <- function(val, ...) { 
  if (length(val)>0) return(val)
  args <- list(...)
  for (i in seq_along(args)) {
    if (length(args[[i]])>0) return(args[[i]]) 
  } 
  NULL
}
