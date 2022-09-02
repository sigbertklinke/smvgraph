#' sdbscan
#'
#' Shiny app which allows to run a cluster analysis with DBSCAN with interactive choice of variables,
#' core distance, and minimal neighbours.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param ... unused
#'
#' @md
#' @return nothing
#' @importFrom shiny shinyOptions
#' @export
#'
#' @examples
#' if (interactive()) sdbscan(iris)
sdbscan <- function(data, xvar=character(0), ...) {
  if (missing(data)) data <- smvgraph::testdata
  xvar <- getVariableNames(data, xvar)
  if (length(xvar)<2) stop("At least two variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="dbscan_plot", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="dbscan_plot"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
