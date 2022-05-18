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
#' @importFrom shiny shinyOptions
#' @export
#'
#' @examples
#' if (interactive()) sparcoord(iris)
sparcoord <- function(data, xvar=character(0), ...) {
  #main <- paste(deparse(substitute(data), 500), collapse = "\n")
  xvar <- getVariableNames(data, xvar)
  if (length(xvar)<3) stop("At least three variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule='parcoord', error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="parcoord"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
