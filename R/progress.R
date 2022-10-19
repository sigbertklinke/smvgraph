#' @rdname progress
#' @title Progress
#' @description Reports progress to the user during long-running operations.
#' @param ... see [[shiny::withProgress]]
#'
#' @return see [[shiny::withProgress]]
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'    options(device.ask.default = FALSE)
#'    ui <- fluidPage(plotOutput("plot"))
#'    #
#'    server <- function(input, output) {
#'      output$plot <- renderPlot({
#'        with_progress(message = 'Calculation in progress',
#'                      detail = 'This may take a while...', value = 0, {
#'                      for (i in 1:15) {
#'                        inc_progress(1/15)
#'                        Sys.sleep(0.25)
#'                      }
#'                    })
#'        plot(cars)
#'    })
#'  }
#'  #
#'  shinyApp(ui, server)
#' }
with_progress <- function(...) {
  res <- try(isRunning(), silent=TRUE)
  if (isTRUE(res)) return(withProgress(..., env = parent.frame()))
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  eval(if (is.null(args$expr)) args[[which(nargs=='')[1]]] else args$expr)
}

#' @rdname progress
#' @export
set_progress <- function(...) {
  res <- try(isRunning(), silent=TRUE)
  if (isTRUE(res)) setProgress(...)
}

#' @rdname progress
#' @export
inc_progress <- function(...) {
  res <- try(isRunning(), silent=TRUE)
  if (isTRUE(res)) incProgress(...)
}