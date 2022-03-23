#' sandrews
#'
#' Shiny app for creating an Andrews curve diagram with interactive variable selection.
#'
#' @param data matrix or data frame
#' @param xvar character: names of selected variables for the plot
#' @param ... unused
#'
#' @md
#' @return nothing
#' @import shiny
#' @export
#'
#' @examples
#' if (interactive()) sandrews(iris)
sandrews <- function(data, xvar=character(0), ...) {
#  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (length(xvar)==0) xvar <- names(data)[sapply(data, class) %in% c("integer", "numeric")] 
  xvar <- intersect(xvar, names(data))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="andrews_smvgraph"))
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
