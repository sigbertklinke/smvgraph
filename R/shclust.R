#' shclust
#'
#' Shiny app which allows to run a hierarchical cluster analysis with interactive choice of variables,
#' distance, and agglomeration method.
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
#' if (interactive()) shclust(iris)
shclust <- function(data, xvar=character(0), ...) {
  xvar <- if (length(xvar)==0) names(data)[sapply(data, class) %in% c("integer", "numeric")] else intersect(xvar, names(data))
  if (length(xvar)<2) stop("At least two variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="hclust_plot", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="hclust_plot"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
