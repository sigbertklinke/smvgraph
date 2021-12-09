#' sandrews
#'
#' Shiny app for creating an Andrews curve diagram with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... further parameters given to [andrews]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @export
#'
#' @examples
#' if (interactive()) sandrews(iris)
sandrews <- function(data, xvar=character(0), ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  data <- prepare_data(data, main)
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Andrews curves"),
      dashboardSidebar(
        tags$style( HTML(".black-text .rank-list-item { color: #000000; }")),
        variable_bucket_list(data, xvar, order_andrews)
      ),
      dashboardBody(
        fluidRow(
          box(plotOutput("plot")),
          box(verbatimTextOutput("command"), title="Basic R code")
      ))
    ),
    server = function(input, output, session) {
      output$plot <- renderPlot({
        if ((length(input$xvar)>1)) {
          #browser()
          args     <- list(...)
          args$x   <- data[,input$xvar]
          if (is.null(args$main)) args$main <- main
          do.call("andrews", args)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
          txt <- c(paste0(" x <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                   sprintf("andrews(%s[,x])\n", main))
        }
        txt
      })
    }
  )
}
