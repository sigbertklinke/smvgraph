#' sandrews
#'
#' Shiny app for creating a Parallel Coordinate plot with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... further parameters given to [MASS::parcoord]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom MASS parcoord
#' @export
#'
#' @examples
#' if (interactive()) sparcoord(iris)
sparcoord <- function(data, xvar=character(0), ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (length(xvar)==0) xvar <- names(data)[sapply(data, class) %in% c("integer", "numeric")] 
  xvar <- intersect(xvar, names(data))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="parcoord"))
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages('MASS')
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
