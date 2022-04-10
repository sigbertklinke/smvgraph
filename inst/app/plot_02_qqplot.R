module[["qqplot"]] <- list(
  label = "Q-Q plot",
  help  = "graphics::qqplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2)
  },
  code = function(analysis, group, data, input) {
    template("
             0: x   <- numeric_data(data, select={{x}})
             0: keep <- is.finite(rowSums(x))
             0: x <- x[keep,]
             1: col <- color_data({{g}})
             1: qqplot(x[,1], x[,2], pch={{pch}}, cex={{cex}}, col=col)
             !1: qqplot(x[,1], x[,2], pch={{pch}}, cex={{cex}})
             0: abline(a=0, b=1, lwd={{lwd}}, lty={{lty}})
             1: ucol <- col[!duplicated(col)]
             1: legend({{pos}}, legend=names(ucol), fill=ucol, title=attr(col, 'title'), cex={{lex}})  
             ", 
             x=sprintf("c(%s)", paste0("'", row.names(analysis), "'", collapse=", ")),
             g=sprintf("subset(data, select=c(%s))", paste0("'", row.names(group), "'", collapse=", ")), 
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             lwd=txt(getval(input$smvgraph_lwd, "solid")),
             lty=getval(input$smvgraph_lty,1),
             lex=getval(input$smvgraph_lex,1),
             pos=sprintf("'%s'", getval(input$smvgraph_legend, "topleft")),
             nrow(group)>0 #1
             )
  },
  ui = function(analysis, group, data, input) {
    list(UIpointsymbol(),
         UIpointsize(),
         UIlinetype(),
         UIlinewidth(),
         if (nrow(group)>0) UIlegend() else NULL,
         if (nrow(group)>0) UIlegendsize() else NULL
    )
  }
)