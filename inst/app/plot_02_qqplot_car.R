module[["qqplot_car"]] <- list(
  label = "Q-Q norm plot (car)",
  help  = "car::qqPlot",
  packages = "car",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (analysis$unique>1) && (nrow(group)<2) && (prod(group$unique)<13)
  },
  code = function(analysis, group, data, input) {
    template("
             0:    library('car')
             0:    x <- numeric_data({{x}}, out='vector')
             0:    keep <- is.finite(x)
             0:    x <- x[keep]
             1:    grp <- factor_data(data, select={{g}}, out='vector')[keep]
             2&!1: qqPlot(x, pch={{pch}}, cex={{cex}}, distribution='norm', mean=mean(x), sd=sd(x), line={{line}})
             2&1:  qqPlot(x, pch={{pch}}, cex={{cex}}, distribution='norm', mean=mean(x), sd=sd(x),  line={{line}}, group=grp)
             ", 
             x  =sprintf("data$%s", row.names(analysis)),
             g=as_param(txt(row.names(group)), fun="c"),
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             lwd=txt(getval(input$smvgraph_lwd, "solid")),
             lty=getval(input$smvgraph_lty,1),
             line=txt(getval(input$qqplot_car_line, "quartiles")),
             nrow(group)>0 #1
             )
  },
  ui = function(analysis, group, data, input) {
    x  <- data[[row.names(analysis)]]
    mx <- signif(mean(x[is.finite(x)]), 3) 
    sx <- signif(sd(x[is.finite(x)]), 3)
    rx <- seq(mx-5*sx, mx+5*sx, by=sx/5)
    list(selectInput('qqplot_car_line', "Line",
                     choices=toChoice(NA, "quartiles", "robust", "none")),
         UIpointsymbol(),
         UIpointsize()
    )
  }
)