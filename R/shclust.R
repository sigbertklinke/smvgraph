#' shclust
#'
#' Shiny app which allows to run a hierarchical cluster analysis with interactive choice of variables,
#' distance, and agglomeration method.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the clustering
#' @param colpal color palette for cluster colors (default: [grDevices::rainbow])
#' @param ... unused
#' @param color color: used for dendrogram (default: \code{"black"})
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom graphics layout
#' @importFrom stats cutree
#' @export
#'
#' @examples
#' if (interactive()) shclust(iris)
shclust <- function(data, xvar=character(0), colpal=rainbow, ..., color="black") {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  data <- prepare_data(data, main)
  #
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Hierarchical clustering"),
      dashboardSidebar(
        tags_style(),
        sliderInput("cluster", "Cluster", min=2, max=min(15, nrow(data)), value = 2),
        splitLayout(cellWidths = c("50%", "50%"),
          selectInput("dist", "Distance",
                      choices = list("Euclidean"="euclidean", "Maximum"="maximum", "Manhattan"="manhattan",
                                   "Canberra"="canberra")),
          selectInput("method", "Method",
                      choices = list("Ward.D"="ward.D", "Ward.D2"="ward.D2",  "Single"="single",
                                     "Complete"="complete", "Average"="average", "McQuitty"="mcquitty",
                                     "Median"="median", "Centroid"="centroid"))),
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
          hc     <- hclust(dist(data[,input$xvar], method=input$dist), method = input$method)
          colcl  <- colpal(input$cluster)
          clt    <- cutree(hc, input$cluster)
          x      <- prcomp(data[,input$xvar])$x[,1:2]
          layout(mat = matrix(c(1,1,1,2,2,1,1,1,3,3), ncol=2))
          plot(x, col=colcl[clt], pch=19, asp=TRUE, main=main)
          # dendrogram
          plot(c(1, length(hc$order)), c(0, max(hc$height)), type="n", main="Dendrogram",
               xlab="Observations", ylab="Height")
          cl <- -hc$order
          hx <- rep(NA, max(hc$merge))
          for (i in seq(hc$height)) {
            if (hc$merge[i,1]<0) {
              x1 <- which(hc$order==-hc$merge[i,1])
              y1 <- 0
            } else {
              y1 <- hc$height[hc$merge[i,1]]
              x1 <- mean(range(which(cl==hc$merge[i,1])))
            }
            if (hc$merge[i,2]<0) {
              x2 <- which(hc$order==-hc$merge[i,2])
              y2 <- 0
            } else {
              y2 <- hc$height[hc$merge[i,2]]
              x2 <- mean(range(which(cl==hc$merge[i,2])))
            }
            cli <- clt[hc$order[cl %in% hc$merge[i,]]]
            col <- if (length(unique(cli))==1) colcl[cli[1]] else color
            lines(c(x1, x1, x2, x2), c(y1, hc$height[i], hc$height[i], y2), col=col)
            cl[cl==hc$merge[i,1]] <- i
            cl[cl==hc$merge[i,2]] <- i
          }
#          plot(hc, hang=-1, xlab=sprintf("%s & %s", input$dist, input$method), sub="",
#               labels=FALSE, ylim=c(0, max(hc$height)))
          rheight <- rev(hc$height)
          rh <- (rheight[input$cluster-1]+rheight[input$cluster])/2
          plot(1:15, rheight[1:15], pch=19, type="b", ylim=c(0, max(hc$height)), xlim=c(1,15),
               xlab="Cluster", ylab="Height")
          lines(c(1,15), c(rh, rh))
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
           txt <- c(" layout(matrix(c(1,2,1,3), nrow=2))\n",
                    paste0("v   <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                    sprintf("x   <- prcomp(%s[,v])$x[,1:2]\n", main),
                    sprintf("d   <- dist(%s[,v], \"%s\")\n", main, input$dist),
                    sprintf("hc  <- hclust(d, \"%s\")\n", input$method),
                    sprintf("ck  <- cutree(hc, %s)\n", input$cluster),
                    "col <- rainbow(max(ck))[ck]\n",
                    "plot(x, col=col, asp=TRUE, pch=19)\n",
                    "plot(hc, hang=-1, labels=FALSE)\n",
                    "plot(2:15, rev(hc$height)[1:14], type=\"b\")\n"
                   )
        }
        txt
      })
    }
  )
}
