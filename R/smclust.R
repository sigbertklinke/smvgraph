#' smclust
#'
#' Shiny app which allows to run a EM clustering with interactive choice of variables.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param colpal color palette for cluster colors (default: [grDevices::rainbow])
#' @param ... unused
#' @param srt numeric: model string rotation in degrees (default: \code{0}) 
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom graphics text
#' @importFrom mclust Mclust mclustBIC
#' @export
#'
#' @examples
#' if (interactive()) smclust(iris)
smclust <- function(data, xvar=character(0), colpal=rainbow, ..., srt=0) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  data <- prepare_data(data, main)
  #
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="EM clustering"),
      dashboardSidebar(
        tags_style(),
        sliderInput("cluster", "Cluster", min=2, max=15, value = 2),
        selectInput("model", "Models", choices=list(
          "ellipsoidal, varying volume, shape, and orientation (VVV)" ="VVV"   ,
          "ellipsoidal, equal volume (EVV)" ="EVV"    ,
          "ellipsoidal, equal shape (VEV)" ="VEV"    ,
          "ellipsoidal, equal volume and equal shape (EEV)" ="EEV",
          "ellipsoidal, equal orientation (VVE)" ="VVE"    ,
          "ellipsoidal, equal shape and orientation (VEE)" ="VEE",
          "ellipsoidal, equal volume and orientation (EVE)" ="EVE",
          "ellipsoidal, equal volume, shape, and orientation (EEE)" ="EEE",
          "diagonal, varying volume and shape (VVI)" ="VVI"    ,
          "diagonal, equal volume, varying shape (EVI)" ="EVI"  ,
          "diagonal, varying volume, equal shape (VEI)" ="VEI"   ,
          "diagonal, equal volume and shape (EEI)" ="EEI"    ,
          "spherical, unequal volume (VII)" ="VII"    ,
          "spherical, equal volume (EII)" ="EII"
        )),
        variable_bucket_list(data, xvar)
      ),
      dashboardBody(
        fluidRow(
          box(plotOutput("plot")),
          box(verbatimTextOutput("command"), title="Basic R code")
        ))
    ),
    
    server = function(input, output, session) {
      
      rv <- reactiveValues(bic=numeric(0), df=numeric(0))
      
      observeEvent(input$xvar, {
        rv$bic <- numeric(0)
        rv$df <- numeric(0)
      })
      
      output$plot <- renderPlot({
        input$runagain
        if (length(input$xvar)>1) {
          em  <- Mclust(data[,input$xvar], G=input$cluster, modelNames=input$model, x=NULL)
          mn  <- sprintf("%s,%.0f", em$modelName, em$G)
          isolate(rv$bic[mn] <- em$BIC)
          isolate(rv$df[mn]  <- em$df)
          x   <- prcomp(data[,input$xvar])$x[,1:2]
          col <- colpal(max(em$classification))[em$classification]
          layout(c(1,1,2))
          plot(x, col=col, pch=19, asp=TRUE, main=main, sub=mn)
          plot(range(rv$bic), range(rv$df), type="n", main="BIC", xlab="BIC", ylab="df")
          text(rv$bic, rv$df, labels=names(rv$bic), srt=srt)
        }
      })
      
      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
          txt <- c(paste0(" v   <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                   sprintf("x   <- prcomp(%s[,v])$x[,1:2]\n", main),
                   sprintf("em  <- Mclust(%s[,v], G=%s, modelNames=\"%s\")\n", main, input$cluster, input$model),
                   "col <- rainbow(max(em$classification))[em$classification]\n",
                   "plot(x, col=col, asp=TRUE, pch=19)\n"
          )
        }
        txt
      })
    }
  )
}
