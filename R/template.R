#' template 
#' 
#' Each line of a code template consists of condition based on the unnamed parameters and R code in which replacements
#' with named parameters done.
#' 
#' @param text a code template
#' @param ... further parameters
#'
#' @return a character vector 
#' @export
#'
#' @examples
#' template("
#' 1:   'Hello {{letter}}'
#' !1:  'Good-bye {{letter}}'
#'          ",
#'          letter=sample(LETTERS, 1),
#'          runif(1)<0.5 #1 = first unnamed parameter
#'          )
template <- function (text, ...) {
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  for (name in nargs) {
    if (nchar(name)) {
      aan <- as.character(args[[name]])
      if (length(aan)!=1) stop(sprintf("Only one string expected for '%s'", name))
      text <- gsub(paste0('{{', name, '}}'), as.character(args[[name]]), text, fixed=TRUE)      
    }
  }
  args    <- args[nchar(nargs)==0]
  text    <- strsplit(unlist(strsplit(text, "\n")), ":")
  text[lengths(text)<2] <- NULL
  include <- sapply(text, function(e) {
    inc <- gsub("0", "TRUE", e[1], fixed=TRUE)
    if (length(args)) {
      for (i in 1:length(args)) inc <- gsub(as.character(i), as.character(args[[i]]), inc, fixed=TRUE)
    }
    isTRUE(eval(parse(text=inc)))
  })
  text <- sapply(text, function(e) { paste0(e[-1], collapse=":")})
  trimws(text[include])
}