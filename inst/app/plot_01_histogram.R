module[["histogram_default"]] <- list(
  label = "Histogram",
  help  = "graphics::hist",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (nrow(group)==0) 
  },
  code = function(analysis, group, data, input) {
    template("
0:      x     <- numeric_data({{x}}, out='vector')
2|3:    ylim  <- NULL # default for histogram
2:      xh    <- pretty(x)
2:      xg    <- seq(min(xh), max(xh), length.out=512)
2:      yg    <- dnorm(xg, mean=mean(x), sd=sd(x))
2:      ylim  <- range(ylim, yg)
3:      dens  <- density(x, adjust={{adjust}})
3:      ylim  <- range(ylim, dens$y)
2|3:    h     <- hist(x, breaks={{breaks}}, plot=FALSE)
2|3:    ylim  <- range(ylim, h$density)
!(2|3): hist(x, breaks={{breaks}})
2|3:    hist(x, breaks={{breaks}}, ylim=ylim, freq=FALSE)
1:      rug(x)
2:      lines(xg, yg, col='blue', lwd=2)
3:      lines(dens, col='green', lwd=2)             
             ",
             x      = sprintf("data$%s", row.names(analysis)),
             breaks = sprintf("%.0f", getval(input$histogram_default_breaks, 7)),
             adjust = sprintf("%.2f", getval(input$histogram_default_density, 0)),
             getval(input$histogram_default_rug, FALSE), #1
             getval(input$histogram_default_norm, FALSE), #2
             getval(input$histogram_default_density>0, FALSE) #3
             )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("histogram_default_breaks", "Number of breaks", 2, 50, 10, 1),
         checkboxInput("histogram_default_rug", "Show observations"),
         checkboxInput("histogram_default_norm", "Add normal curve (blue)"),
         sliderInput("histogram_default_density", "Bandwidth for density (green)", 0, 2, 0, 0.05)
    )
  }
)
