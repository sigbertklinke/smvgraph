module[["vioplot"]] <- list(
  label = "Violin plot (vioplot)",
  help  = "vioplot::vioplot",
  packages = "vioplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   x    <- numeric_data(data, select={{x}}, 'vector')
             0:   keep <- is.finite(x)
             0:   x    <- x[keep]
             1:   col <- color_data(data, select={{g}})[keep]
             1:   h   <- h.select(x, group=col)*{{adjust}}
             1:   vioplot(x~names(col),  horizontal={{horiz}}, h=h, xlab=attr(col, 'title'))
             !1:  h   <- h.select(x)*{{adjust}}
             !1:  vioplot(x,  horizontal={{horiz}}, h=h, xlab={{xlab}})
             2:   rug(x, side={{side}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             horiz=getval(input$vioplot_horizontal, FALSE),
             adjust=getval(input$vioplot_adjust, 1),
             side=2-getval(input$vioplot_horizontal, FALSE),
             xlab=txt(row.names(analysis)),
             lab=if(getval(input$vioplot_horizontal, FALSE)) "ylab" else "xlab",
             nrow(group)>0,                                     #1
             getval(input$vioplot_rug, FALSE)                   #2
            )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("vioplot_rug", "Show observations"),
         checkboxInput("vioplot_horizontal", "Horizontal"),
         sliderInput("vioplot_adjust", "Multiplication factor for bandwidth", 0.05, 2, 1, 0.05)
)
  }
)
