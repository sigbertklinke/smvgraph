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
#' @param colpal color palette (default: [grDevices::rainbow])
#' @param ... unused
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom grDevices rainbow
#' @importFrom graphics axis barplot
#' @importFrom stats var
#' @export
#'
#' @examples
#' if (interactive()) sdistance(iris)
sdistance <- function(data, xvar=character(0), colpal=rainbow, ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  data <- prepare_data(data, main)
  #
  mind <- apply(data, 2, min, na.rm=TRUE)
  maxd <- apply(data, 2, max, na.rm=TRUE)
  #
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Distance plot"),
      dashboardSidebar(
        tags_style(),
        sliderInput("obs", "Observations", min=1, max=nrow(data), step=1, value = c(1, nrow(data))),
        variable_bucket_list(data, xvar)
      ),
      dashboardBody(
        fluidRow(
          box(plotOutput("plot")),
          box(verbatimTextOutput("command"), title="Basic R code")
      ))
    ),
    server = function(input, output, session) {

      rv <- reactiveValues(tab=NULL)

      output$plot <- renderPlot({
        rv$tab <- NULL
        if (length(input$xvar)>1) {
          par(mar=c(5.1, 4.1*max(nchar(input$xvar))/7, 4.1, 2.1))
          rvar <- rev(input$xvar)
          tab <- matrix(0, ncol=6, nrow=length(rvar))
          dij <- abs(data[input$obs[1], rvar]-data[input$obs[2], rvar])
          # Variance
          tab[,1] <- diag(var(data[,rvar]))
          tab[,1] <- tab[,1]/sum(tab[,1])
          # Minimum
          tab[,2] <- seq(dij)==which.min(dij)
          # Manhattan
          tab[,3] <- dij/sum(dij)
          # Gower
          gij     <- dij/(maxd-mind)
          tab[,4] <- gij/sum(gij)
          # Euclidean
          tab[,5] <- dij^2/sum(dij^2)
          # Maximum
          tab[,6] <- seq(dij)==which.max(dij)
          colnames(tab) <- c("Total\nVariance", "Minimum", "Manhattan", "Gower", "Euclidean", "Maximum")
          barplot(tab, axes=FALSE, col=colpal(length(rvar)), cex.names=0.8,
                  main=main, sub=sprintf("Observation %.0f - %.0f", input$obs[1], input$obs[2]))
          axis(2, at=cumsum(tab[,1])-tab[,1]/2, labels = rvar, las=2)
          axis(4, at=(0:5)/5, labels=sprintf("%.0f%%", (0:5)*20))
          rv$tab <- round(tab, 2)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
          txt <- c(sprintf(" tab <- matrix(c(%s), ncol=6)\n", toString(rv$tab)),
                   "colnames(tab) <- c(\"Total\\nVariance\", \"Minimum\", \"Manhattan\", \"Gower\", \"Euclidean\", \"Maximum\")\n",
                   paste0("rownames(tab) <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                   "barplot(as.table(tab), col=rainbow(nrow(tab)))")
        }
        txt
      })
    }
  )
}
