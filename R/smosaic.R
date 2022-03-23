#' smosaic
#'
#' Shiny app for creating a Mosaic plot with interactive variable selection.
#'
#' @param data table or data.frame
#' @param xvar character: names of selected variables for x-axis
#' @param yvar character: names of selected variables for y-axis
#' @param ... further parameters given to [graphics::mosaicplot]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom graphics mosaicplot
#' @export
#'
#' @examples
#' if (interactive()) smosaic(Titanic)
#' dfTitanic <- toDataframe(Titanic)
#' if (interactive()) smosaic(dfTitanic)
smosaic <- function(data, xvar=character(0), yvar=character(0), ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (is.table(data)) data <- toDataframe(data)
  xvar <- if (length(xvar)==0) names(data)[!(sapply(data, class) %in% c("integer", "numeric"))] else intersect(xvar, names(data))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="mosaicplot"))
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
