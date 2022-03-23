module[["stripchart"]] <- list(
  label = "Strip chart",
  help  = "graphics::stripchart",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && prod(group$unique)<43
  },
  code = function(analysis, group, data, input) {
    template("
             0:  x <- numeric_data(data, select={{x}}, out='vector')
             0:  keep <- is.finite(x)
             0:  x    <- x[keep]
             1:  col  <- color_data(data, select={{g}})[keep]
             !1: stripchart(x, vertical={{vert}}, method={{method}}, pch={{pch}}, cex={{cex}}, jitter={{jitter}})
             1:  ucol  <- col[!duplicated(col)]
             1:  stripchart(x~names(col), vertical={{vert}}, method={{method}}, col=ucol, pch={{pch}}, cex={{cex}}, jitter={{jitter}})
             1:  title({{lab}}=attr(col,'title'))
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             method=txt(getval(input$stripchart_method, "overplot")),
             vert=txt(getval(input$stripchart_vertical, FALSE)),
             jitter=getval(input$stripchart_jitter, 0.1),
             cex=getval(input$smvgraph_cex, 1),
             pch=getval(input$smvgraph_pch, 1),
             lab=if(getval(input$stripchart_vertical, FALSE)) "xlab" else "ylab",
             nrow(group)>0    #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(selectInput("stripchart_method", "Method", 
                     choices=list("Overplot"="overplot", "Jitter"="jitter", "Stack"="stack")),
         sliderInput("stripchart_jitter", "Amount of jittering", 0, 0.5, 0.1, 0.05),
         UIpointsymbol(),
         UIpointsize(),
         checkboxInput("stripchart_vertical", "Vertical")
         )
  }
)