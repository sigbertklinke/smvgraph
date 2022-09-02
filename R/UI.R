#' @rdname UI
#' @title General UI elements
#' @aliases UIdatanormalization UIdistance UIobservations UImergegroups UIpointsize UIpointsymbol UIlinewidth UIlinetype UItextsize 
#' @aliases UIlegend UIlegendsize UIplottype
#' @description Some general UI elements for common use where last selected value is stored for reuse:
#' * `UIplottype` plot type, defines `smvgraph_type`
#' * `UIpointsymbol` plot symbol for point, defines `smvgraph_pch`
#' * `UIpointsize` point size, defines `smvgraph_cex`
#' * `UIlinetype` line type, defines `smvgraph_lty`
#' * `UIlinewidth` line width, defines `smvgraph_lwd`
#' * `UItextsize` text size, defines `smvgraph_tex`
#' * `UIlegend` legend position, defines `smvgraph_legend`
#' * `UIlegendsize` legend size, defines `smvgraph_lex`
#' * `UIdatanormalization` should data be rescaled, defines `smvgraph_normalize` (no, minMax, mtandardization)
#' * `UIdistance` distance to use, defines `smvgraph_distance`
#' * `UIobservations` range of observations, defines `smvgraph_obs`
#' * `UImergegroups` should a set of grouping variables merged into one group variable, defines `smvgraph_single`
#' 
#' From the top menu are the following input elements are defined
#' * `input$smvgraph_pch` point symbol,
#' * `input$smvgraph_cex` point size, 
#' * `input$smvgraph_lty` line type,
#' * `input$smvgraph_lwd` line width,
#' * `input$smvgraph_tex` text size, and
#' * `inpus$smvgraph_legend` legend position.
#' 
#' @param sel selected element
#' @param n integer: number of observations
#'
#' @return an UI element for shiny
#' @export
#'
#' @examples
#' # none
UIdatanormalization <- function(sel=getShinyOption("smvgraph.current")$smvgraph_normalize) {
  if (is.null(sel)) sel <- 2
  selectInput("smvgraph_normalize", "Data normalization",
              choices=c("None"=0, "Minmax"=1, "Scale"=2), selected = sel)
}

#' @rdname UI
#' @export
UIdistance <- function(sel=getShinyOption("smvgraph.current")$smvgraph_distance) {
  if (is.null(sel)) sel <- "euclidean"
  selectInput("smvgraph_distance", "Distance",
              choices=c("Euclidean"="euclidean", "Maximum"="maximum", "Manhatten"="manhattan", "Canberra"="canberra"), selected = sel)
}

#' @rdname UI
#' @export
UIobservations <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_obs) {
  if (is.null(sel)) sel <- c(1, 2)
  sliderInput("smvgraph_obs", "Observation range", 1, n, sel, 1)
}

#' @rdname UI
#' @export
UImergegroups <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_single) {
  if (is.null(sel)) sel <- FALSE
  checkboxInput("smvgraph_single", "Merge groups into one group", value=sel)
}

#' @rdname UI
#' @export
UIpointsize <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_cex) {
  if (is.null(sel)) sel <- 1
  sliderInput("smvgraph_cex", "Point size", 0, 3, sel, 0.1)
}

#' @rdname UI
#' @export
UIpointsymbol <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_pch) {
  if (is.null(sel)) sel <- 19
  sliderInput("smvgraph_pch", "Point symbol", 0, 25, sel, 1)
}

#' @rdname UI
#' @export
UIlinewidth <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_lwd) {
  if (is.null(sel)) sel <- 1
  sliderInput("smvgraph_lwd", "Line width", 0, 3, sel, 0.1)
}

#' @rdname UI
#' @export
UIlinetype <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_lty) {
  if (is.null(sel)) sel <- 1
  selectInput("smvgraph_lty", "Line type", 
              choices = c("Blank" = 0, "Solid" = 1, "Dashed" = 2, "Dotted" = 3, "Dotdash" = 4, "Longdash"= 5, "Twodash"=6),
              selected = sel
  )
}

#' @rdname UI
#' @export
UItextsize <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_tex) {
  if (is.null(sel)) sel <- 1
  sliderInput("smvgraph_tex", "Point size", 0, 2, sel, 0.1)
}

#' @rdname UI
#' @export
UIlegend <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_legend) {
  if (is.null(sel)) sel <- 1
  selectInput("smvgraph_legend", "Legend position",
              choices=list("Top left"     = "topleft", 
                           "Top right"    = "topright", 
                           "Bottom left"  = "bottomleft",
                           "Bottom right" = "bottomright",
                           "Bottom"       = "bottom",
                           "Left"         = "left",
                           "Top"          = "top",
                           "Right"        = "right",
                           "Center"       = "center"))
}

#' @rdname UI
#' @export
UIlegendsize <- function(n, sel=getShinyOption("smvgraph.current")$mvgraph_lex) {
  if (is.null(sel)) sel <- 1
  sliderInput("smvgraph_lex", "Legend size", 0, 1.5, 1, 0.05)
}

#' @rdname UI
#' @export
UIplottype <- function(n, sel=getShinyOption("smvgraph.current")$smvgraph_type) {
  if (is.null(sel)) sel <- "b"
  selectInput("smvgraph_type", "Plot type", 
              choices = c("Points" = "p", "Lines" = "l", "Point & Lines" = "b", "Empty points joined by lines" = "c", 
                          "Overplotted points and lines" = "o", "Histogram-like vertical lines"= "h", "None"="n"),
              selected = sel
  )
}