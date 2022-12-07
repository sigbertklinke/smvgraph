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
#' @importFrom shiny shinyOptions
#' @importFrom graphics par
#' @export
#'
#' @examples
#' if (interactive()) sradar(iris)
sradar <- function(data, xvar=character(0), ...) {
  if (missing(data)) data <- smvgraph::testdata
  if(!is.data.frame(data)) data <- as.data.frame(data)
  xvar <- getVariableNames(data, xvar)
  if (length(xvar)<3) stop("At least three variables required")  
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="radarchart_mass", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="radarchart_mass"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
