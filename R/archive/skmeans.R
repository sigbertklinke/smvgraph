#' skmeans
#'
#' Shiny app which allows to run a k-means cluster analysis with interactive choice of variables.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param colpal color palette for cluster colors (default: [grDevices::rainbow])
#' @param ... unused
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom stats kmeans
#' @export
#'
#' @examples
#' if (interactive()) skmeans(iris)
skmeans <- function(data, xvar=character(0), colpal=rainbow, ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  data <- prepare_data(data, main)
  #
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="K-Means clustering"),
      dashboardSidebar(
        tags_style(),
        sliderInput("cluster", "Cluster", min=2, max=min(15, nrow(data)), value = 2),
        actionButton("runagain", "Cluster again"),
        variable_bucket_list(data, xvar)
      ),
      dashboardBody(
        fluidRow(
          box(plotOutput("plot")),
          box(verbatimTextOutput("command"), title="Basic R code")
      ))
    ),
    server = function(input, output, session) {

      output$plot <- renderPlot({
        input$runagain
        if (length(input$xvar)>1) {
          km  <- kmeans(data[,input$xvar], input$cluster)
          x   <- prcomp(data[,input$xvar])$x[,1:2]
          col <- colpal(max(km$cluster))[km$cluster]
          plot(x, col=col, pch=19, asp=TRUE, main=main)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
           txt <- c(paste0(" v   <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                    sprintf("x   <- prcomp(%s[,v])$x[,1:2]\n", main),
                    sprintf("km  <- kmeans(%s[,v], %s)\n", main, input$cluster),
                    "col <- rainbow(max(km$cluster))[km$cluster]\n",
                    "plot(x, col=col, asp=TRUE, pch=19)\n"
                   )
        }
        txt
      })
    }
  )
}
