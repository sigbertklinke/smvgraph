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
#' @importFrom shiny shinyOptions
#' @importFrom graphics pairs
#' @export
#'
#' @examples
#' if (interactive()) spairs(iris)
spairs <- function(data, xvar=character(0), ...) {
  if (missing(data)) data <- smvgraph::testdata
  if(!is.data.frame(data)) data <- as.data.frame(data)
  xvar <- getVariableNames(data, xvar)
  if (length(xvar)<2) stop("At least two variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="splom_pairs", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="splom_pairs"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
