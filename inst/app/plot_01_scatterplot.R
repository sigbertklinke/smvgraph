module[["scatter_plot"]] <- list(
  label = "Scatter plot",
  help  = "graphics::plot.default",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2) && (nrow(group)==0) 
  },
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    template("
             0: x <- numeric_data(data, select={{x}}) 
             0: plot.default(x, pch={{pch}}, cex={{cex}})
             1: lmobj <- lm({{y}}~., data=x)
             1: abline(lmobj, lwd={{lwd}}, lty={{lty}})
             2: x <- x[order(x[,1], x[,2]), ]
             2: lwobj <- lowess(x${{xl}}, x${{y}}, f={{span}})
             2: lines(lwobj, lwd={{lwd}}, lty={{lty}})
             ",
             x=sprintf("c(%s)", paste0("'", row.names(analysis), "'", collapse=", ")),
             xl=row.names(analysis)[1],
             y=row.names(analysis)[2],
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             lwd=getval(input$smvgraph_lwd,1),
             lty=getval(input$smvgraph_lty,1),   
             span=getval(input$scatter_plot_span, 0.65),
             getval(input$scatter_plot_regression, 0)==1, #1
             getval(input$scatter_plot_regression, 0)==2  #2
             )
  },
  ui = function(analysis, group, data, input) {
    list(UIpointsymbol(),
         UIpointsize(),
         radioGroupButtons("scatter_plot_regression", "Regression line",
                            choices=c("None"=0, "Linear"=1, "Lowess"=2)),
         conditionalPanel('input.scatter_plot_regression==1',
                          UIlinetype(),
                          UIlinewidth()),
         conditionalPanel('input.scatter_plot_regression==2',
                          sliderInput("scatter_plot_span", "Span", 0.05, 2, 0.65, 0.05),
                          UIlinetype(),
                          UIlinewidth()
                          )
    )
  }
)