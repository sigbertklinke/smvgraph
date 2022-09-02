#' sdistance
#'
#' Shiny app which shows the contribution of each variable to the distance between two observations
#' with interactive variable selection. If \eqn{dijk} is the distance between observations \eqn{i}
#' and \eqn{j} in variable \eqn{k} then the contribution is computed:
#'
#' * Total variance: \eqn{var_k/sum(var_k)} with \eqn{var_k} the variance of the \eqn{k}th variable
#' * Minimum: \eqn{dijk==min_k(dijk)}
#' * Manhattan: \eqn{dijk/sum(dijk)}
#' * Gower: \eqn{dijk} is rescaled to \eqn{[0, 1]} in each variable and then \eqn{dijk/sum(dijk)}
#' * Euclidean: \eqn{dijk^2/sum(dijk^2)}
#' * Manhattan: \eqn{dijk/sum(dijk)}
#' * Maximum: \eqn{dijk==max_k(dijk)}
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... unused
#'
#' @md
#' @return nothing
#' @importFrom shiny shinyOptions
#' @export
#'
#' @examples
#' if (interactive()) sdistance(iris)
sdistance <- function(data, xvar=character(0), ...) {
  if (missing(data)) data <- smvgraph::testdata
  xvar <- getVariableNames(data, xvar)
  if (length(xvar)<2) stop("At least two variables required")
  # 
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(resetpar(oldpar))
  checkPackages(plotmodule="distance_plot", error=TRUE)
  shinyOptions('smvgraph.param'=list(file=toRDS(data), analysis=xvar, plotmodule="distance_plot"))
  source(system.file("app", "app.R", package = "smvgraph"), local = TRUE, chdir = TRUE)$value
}
