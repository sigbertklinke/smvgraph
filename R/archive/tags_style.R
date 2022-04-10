#' tags_style
#'
#' A default style for the Shiny apps:
#' * `HTML(".black-text .rank-list-item { color: #000000; }")`,
#' * `HTML(".shiny-split-layout > div { overflow: visible;}")`, and
#' * `HTML(".main-sidebar { font-size: 10px; }")`.
#'
#' @md
#' @return HTML style
#' @export
#'
#' @examples
#' tags_style()
tags_style <- function() {
  tags$style( HTML(".black-text .rank-list-item { color: #000000; }"),
              HTML(".shiny-split-layout > div { overflow: visible;}"),
              HTML(".main-sidebar { font-size: 10px; }")
  )
}