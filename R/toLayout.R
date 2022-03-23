#' toLayout
#'
#' Given a (minimal) length `len` and the number unused entries a layout is generated. 
#' If `sel<0` then less rows and more columns are used and if `sel>0` then more rows and less columns are used.
#' @param len integer: minimal size of layout
#' @param sel integer: select less or more rows (default: `0`)
#' @param unused integer: number of unused entries (default: `0`)
#'
#' @return a matrix
#' @export
#'
#' @examples
#' toLayout(13)
toLayout <- function(len, sel=0, unused=0) {
  stopifnot(len>0)
  stopifnot(unused>=0)
  # select window
  lenu <- len+unused
  nr   <- 1:lenu
  nc   <- ceiling(lenu/nr)
  dupc <- !duplicated(nc)
  nr   <- nr[dupc]
  nc   <- nc[dupc]
  pos  <- (length(nr)+length(nr)%%2)%/%2+sel
  if (pos<1) pos <- 1
  if (pos>length(nr)) pos <- length(nr)
  #
  nrcp   <- nr[pos]*nc[pos]
  layout <- rep(0, nrcp)
  layout[1:len] <- 1:len
  if (unused) layout[(nrcp-unused+1):nrcp] <- len+1:unused
  # 
  layout <- matrix(layout, nrow=nr[pos], ncol=nc[pos], byrow=TRUE)
  structure(layout, mass=cbind(as.vector(t(col(layout))), as.vector(t(nr[pos]-row(layout)+1)))[1:len,])
}