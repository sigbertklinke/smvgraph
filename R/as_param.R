#' @rdname as_param
#' @title as_param
#' @aliases txt
#' @description Create a parameter list or a function call. For a function call `fun` must be explicitly given.
#' @param ... list of named and unnamed parameters
#' @param fun character: 
#' @param x character: replaces `"x"` by `"'x'"`
#'
#' @return a character as parameter list of function call
#' @export
#'
#' @examples
#' as_param(letters[1:5])
#' as_param(txt(letters[1:5]))
#' as_param(a=txt("a"))
#' as_param(txt(letters[1:5]), fun="c")
as_param <- function(..., fun=NULL) {
  args <- list(...)
  nargs <- names(args)
  param <- c()
  if (is.null(nargs)) nargs <- rep('', length(args))
  k <- 1
  for (i in 1:length(args)) {
    param[k]  <- if (nchar(nargs[i])>0) paste0(nargs[i], "=") else ''
    for (j in 1:length(args[[i]])) {
      param[k] <- paste0(if (j==1) param[k] else '', as.character(args[[i]][j]))
      k <- k+1
    }
  }
  paste0(fun, '(', paste0(param, collapse=', '), ')')
}

#' @rdname as_param
#' @export
txt <- function(x) { paste0("'", x, "'") }
