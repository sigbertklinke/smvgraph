#' schernoff
#'
#' Shiny app for creating a Chernoff faces plot with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... further parameters given to [DescTools::PlotFaces]
#'
#' @md
#' @return nothing
#' @importFrom shiny shinyOptions
#' @export
#'
#' @examples
#' if (interactive()) schernoff(normalize(iris))
schernoff <- function(data, xvar=character(0), ...) {
#  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (missing(data)) data <- smvgraph::testdata
  xvar <- getVariableNames(data, xvar)
  if (length(xvar)<3) stop("At least three variables required")  
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="faces_aplpack", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="faces_aplpack"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}