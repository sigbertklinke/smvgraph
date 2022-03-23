#' colorGroup
#'
#' Determines colors for `x` based on its values. `NA` are codes as `na.txt`. If `NA`s should be kept use `na.txt=NULL`.
#'
#' @param x vector, matrix or data frame: contaisn the grouping variable(s)
#' @param colpal color palette: a function which generates "ncol" colors with "colpal(ncol)" (default: [grDevices::rainbow])
#' @param na.txt character: replacement text for `NA` (default: `"NA"`)
#' @param title character: user defined title for the attribute `title` of the result (default: `NULL`)
#' @param ...  further parameters given to [base::factor]
#'
#' @return a color vector, the name is stored in `attr(.,"title")`
#' @export
#'
#' @examples
#' colorGroup(iris$Species)
#' colorGroup(LETTERS)
colorGroup <- function(x, colpal=rainbow, na.txt="NA", title=NULL, ...) {
  xtitle <- title
  if (is.null(xtitle)) {
    xtitle <- paste(deparse(substitute(x), 500), collapse = "\n")
    if (!is.null(colnames(x))) xtitle <- paste0(colnames(x), collapse=",")
  } 
  #
  xdf    <- as.data.frame(x)
  if (!is.null(na.txt)) na.txt <- rep(na.txt, length.out=ncol(xdf))
  txt  <- character(nrow(xdf))
  for (i in 1:ncol(xdf)) {
    xdf[[i]] <- as.character(xdf[[i]])
    if (!is.null(na.txt)) xdf[is.na(xdf[,i]),i] <- na.txt[i]
    txt <- if (i>1) paste0(txt, ',', xdf[[i]]) else xdf[[i]]
  }
  xf  <- factor(txt, ...)
  lvl <- as.numeric(xf)
  structure(colpal(max(lvl, na.rm=TRUE))[lvl], names=txt, title=xtitle)
}