#' toChoice
#'
#' The elements in `...` will coerced into one text vector. The entries will either the text (`method==NA`) or integer number starting at `method`. 
#' The first letter of the list element names will be capitalized.
#'
#' @param method integer: which method is used for creating the list elements
#' @param ... character: choice values
#'
#' @return a list
#' @export
#'
#' @examples
#' txt <- c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")
#' toChoice(NA, txt)
#' toChoice(0, txt) # integer sequence starts at zero
toChoice <-function (method=NA, ... ) {
  txt    <- as.character(unlist(list(...)))
  method <- as.integer(method)
  ret    <- if (is.null(method) || is.na(method)) as.list(txt) else as.list(seq_along(txt)+method-1)
  word   <- strsplit(txt, " ")
  txt    <- sapply(word, function(s)  {
    s[1] <- paste0(toupper(substring(s[1], 1,1)), substring(s[1], 2))
    paste(s, sep="", collapse=" ")
  })
  structure(ret, names=txt)
}