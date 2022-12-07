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
#' @importFrom shiny shinyOptions
#' @importFrom graphics mosaicplot
#' @export
#'
#' @examples
#' if (interactive()) smosaic(Titanic)
#' dfTitanic <- toDataframe(Titanic)
#' if (interactive()) smosaic(dfTitanic)
smosaic <- function(data, xvar=character(0), yvar=character(0), ...) {
  if (missing(data)) data <- smvgraph::testdata
  if (is.table(data)) data <- toDataframe(data)
  if(!is.data.frame(data)) data <- as.data.frame(lapply(as.data.frame(data), as.factor))
  xvar <- getVariableNames(data, xvar, num=FALSE)
  if (length(xvar)<2) stop("At least two variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="mosaicplot", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="mosaicplot"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
