module[["index_default"]] <- list(
  label = "Index plot",
  help  = "graphics::plot.default",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1)
  },
  code = function(analysis, group, data, input) {
    template("
0:     x   <- numeric_data({{x}}, out='vector')
1:     col <- color_data({{g}})
1:     plot(x, pch={{pch}}, cex={{cex}}, col=col)
!1:    plot(x, pch={{pch}}, cex={{cex}})
1:     ucol <- col[!duplicated(col)]
1:     legend({{pos}}, legend=names(ucol), fill=ucol, title=attr(col, 'title'), cex={{lex}})
             ",
             x  = sprintf("data$%s", row.names(analysis)),  
             g  = sprintf("subset(data, select=c(%s))", paste0("'", row.names(group), "'", collapse=", ")), 
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             lex=getval(input$smvgraph_lex,1),
             pos=sprintf("'%s'", getval(input$smvgraph_legend, "topleft")),
             nrow(group)>0 #1
             )
  },
  ui = function(analysis, group, data, input) {
    list(UIpointsymbol(),
         UIpointsize(),
         if (nrow(group)>0) UIlegend() else NULL,
         if (nrow(group)>0) UIlegendsize() else NULL
         )
  }
)
