#' @rdname factor_data
#' @title factor_data
#' @description Creates a single group variable from the data `x`.
#' @param x vector, matrix, or data frame
#' @param select vector: indicating columns to select (default: \code{NULL}) 
#' @param exclude vector: values to be excluded when forming the set of levels (default: \code{NULL})
#' @param na.action a function which indicates what should happen when the data contain NAs (default: [stats::na.pass])
#' @param out output as `data.frame` (default), `matrix`, or `vector`
#' @param ... further parameters to [character_data]
#' @param title character: title attribute (default `NULL`)
#'
#' @return a one-column matrix with the merged groups
#' @export
#'
#' @examples
#' factor_data(iris$Species, out="vector")
#' factor_data(iris)
factor_data <- function (x, select=NULL, out=c("data.frame", "matrix", "vector"),  exclude=NULL,  na.action=stats::na.pass, ..., title=NULL) {
  stopifnot(length(x)>0)
  if (!inherits(x, "data.frame")) x <- as.data.frame(x)
  out <- match.arg(out) 
  if (out=="matrix") stop("out='matrix' is not possible")
  # col names
  nx  <- names(x)
  if (is.null(nx)) nx <- rep('', ncol(x))
  names(x) <- ifelse(nchar(nx)==0, sprintf("V%i", 1:ncol(x)), nx)
  # row names
  nx <- rownames(x)
  if (is.null(nx)) nx <- rep('', nrow(x))
  rownames(x) <- ifelse(nchar(nx)==0, sprintf("%i", 1:nrow(x)), nx)
  #
  if (is.null(select)) select <- names(x) 
  vx <- x[,select,drop=FALSE]
  for (i in 1:ncol(vx)) vx[[i]] <- factor(vx[[i]], exclude=exclude)
  if (out=="vector") {
    args <- list()
    for (i in 1:ncol(vx)) args[[i]] <- as.character(vx[[i]])
    args$sep <- ','
    lab <- do.call(paste, args)
    #
    o   <- do.call(order, vx)
    vx  <- factor(lab, levels=unique(lab[o]))
    names(vx) <- rownames(vx)
  }
  tit <- getval(attr(x, 'title'), title, paste0(select, collapse=","))
  structure(na.action(vx), title=tit, out=out)
}
