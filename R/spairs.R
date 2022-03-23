#' spairs
#'
#' Shiny app for creating a scatterplot matrix with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... further parameters given to [graphics::pairs]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom graphics pairs
#' @export
#'
#' @examples
#' if (interactive()) spairs(iris)
spairs <- function(data, xvar=character(0), ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (length(xvar)==0) xvar <- names(data)[sapply(data, class) %in% c("integer", "numeric")] 
  xvar <- intersect(xvar, names(data))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="splom_pairs"))
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
