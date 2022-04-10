#' sradar
#'
#' Shiny app for creating radar charts with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ...  unused
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom graphics par
#' @export
#'
#' @examples
#' if (interactive()) sradar(iris)
sradar <- function(data, xvar=character(0), ...) {
  #main <- paste(deparse(substitute(data), 500), collapse = "\n")
  xvar <- if (length(xvar)==0) names(data)[sapply(data, class) %in% c("integer", "numeric")] else intersect(xvar, names(data))
  if (length(xvar)<3) stop("At least three variables required")  
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages(plotmodule="radarchart_mass")
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="radarchart_mass"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
