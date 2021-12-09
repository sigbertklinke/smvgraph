#' sdbscan
#'
#' Shiny app which allows to run a cluster analysis with DBSCAN with interactive choice of variables,
#' core distance, and minimal neighbours.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param colpal color palette (default: [grDevices::rainbow])
#' @param ... further parameters given to [dbscan::dbscan]
#' @param maxdist numeric: maximal core distance (default: \code{NA}). If the value is \code{NA} then \code{median(dist(data))} is used.
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom dbscan dbscan
#' @importFrom stats prcomp quantile
#' @export
#'
#' @examples
#' if (interactive()) sdbscan(iris)
sdbscan <- function(data, xvar=character(0), colpal=rainbow, ..., maxdist=NA) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  data <- prepare_data(data, main)
  if (is.na(maxdist)) {
    maxdist <- quantile(dist(data), 0.5)
    maxdist <- round(maxdist, 2-log10(maxdist))
  } else {
    maxdist <- as.numeric(maxdist)
  }
  #
  #  oldpar <- par(no.readonly = TRUE)
  #  on.exit(par(oldpar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Dbscan"),
      dashboardSidebar(
        tags_style(),
        sliderInput("eps", "Core distance", min=0, max=maxdist, value = maxdist/3),
        sliderInput("pts", "Minimal neighbours", min=2, max=10, value = 5),
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
        if (length(input$xvar)>1) {
          db  <- dbscan(x=data[,input$xvar], eps=as.numeric(input$eps), minPts=as.numeric(input$pts), ...)
          col <- c('grey', colpal(max(db$cluster)))
#          if (length(input$xvar)==2) {
#            x <- data[,input$xvar]
#          } else {
            x <- prcomp(data[,input$xvar])$x[,1:2]
#          }
          plot(x, col=col[1+db$cluster], pch=19, asp=TRUE)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
           txt <- c(paste0(" v   <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                    sprintf("x   <- prcomp(%s[,v])$x\n", main),
                    sprintf("db  <- dbscan(%s[,v], eps=%s, minPts=%s)\n", main, input$eps, input$pts),
                    "col <- c('grey', rainbow(max(db$cluster)))\n",
                    "plot(x, col=col[1+db$cluster], pch=19, asp=TRUE)\n")
        }
        txt
      })
    }
  )
}
