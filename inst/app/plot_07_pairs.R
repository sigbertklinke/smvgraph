
module[["splom_pairs"]] <- list(
  label = "Scatter plot matrix (pairs)",
  help  = "graphics::pairs",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1)
  },
  code = function(analysis, group, data, input) {
    template("
             0: x <- numeric_data(data, select={{x}})
             1: col <- color_data(data, select={{g}})
             !1: pairs(x, pch={{pch}}, cex={{cex}})
             1: pairs(x, pch={{pch}}, cex={{cex}}, col=col)
             1: ucol <- col[!duplicated(col)] 
             1: oldpar <- par(xpd = TRUE)
             1: legend({{pos}}, fill = ucol, legend = names(ucol), title=attr(col, 'title'))
             1: par(oldpar)
             ",
             x=sprintf("c(%s)", paste0("'", row.names(analysis), "'", collapse=',')),
             g=sprintf("c(%s)", paste0("'", row.names(group), "'", collapse=',')),
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             pos=sprintf('"%s"', getval(input$smvgraph_legend, "topleft")),
             nrow(group)>0 #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(UIpointsymbol(),
         UIpointsize(),
         if (nrow(group)) UIlegend() else NULL
    )
  }
)
