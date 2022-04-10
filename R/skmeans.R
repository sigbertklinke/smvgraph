#' skmeans
#'
#' Shiny app which allows to run a k-means cluster analysis with interactive choice of variables.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param ... unused
#'
#' @md
#' @return nothing
#' @import shiny
#' @export
#'
#' @examples
#' if (interactive()) skmeans(iris)
skmeans <- function(data, xvar=character(0), ...) {
  xvar <- if (length(xvar)==0) names(data)[sapply(data, class) %in% c("integer", "numeric")] else intersect(xvar, names(data))
  if (length(xvar)<2) stop("At least two variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages(plotmodule="kmeans_plot")
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="kmeans_plot"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
