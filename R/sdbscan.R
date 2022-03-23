#' sdbscan
#'
#' Shiny app which allows to run a cluster analysis with DBSCAN with interactive choice of variables,
#' core distance, and minimal neighbours.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param ... further parameters given to [dbscan::dbscan]
#'
#' @md
#' @return nothing
#' @import shiny
#' @export
#'
#' @examples
#' if (interactive()) sdbscan(iris)
sdbscan <- function(data, xvar=character(0), ...) {
  if (length(xvar)==0) xvar <- names(data)[sapply(data, class) %in% c("integer", "numeric")] 
  xvar <- intersect(xvar, names(data))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="dbscan_plot"))
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
