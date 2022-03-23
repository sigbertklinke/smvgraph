#' splot
#'
#' Shiny app for choosing a specific plot.
#'
#' @param data data.frame: input data
#' @param xvar character: selected variables (default: \code{character(0)})
#'
#' @md
#' @return nothing
#' @export
#'
#' @examples
#' if (interactive()) splot(iris)
splot <- function(data, xvar=character(0)) {
  if (is.table(data) || stats::is.ts(data)) data <- toDataframe(data, paste(deparse(substitute(data), 500), collapse = "\n"))
  stopifnot(is.data.frame(data))
#  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  pkgs <- checkPackages()
  if (!all(pkgs)) stop(sprintf("Package '%s' not installed", names(pkgs)[!pkgs]))
  shinyOptions('smvgraph.param'=list(file=toRDS(data), xvar=xvar, plotmodule=''))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}