module[["bagplot_aplpack_pairs"]] <- list(
  label = "Bagplot (aplpack)",
  help  = "aplpack::bagplot.pairs",
  packages = "aplpack",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   library('aplpack')
             0:   x <- numeric_data(data, select={{x}})
             0:   x <- x[is.finite(rowSums(x)),]
             0:   bagplot.pairs(x, factor={{factor}}, pch={{pch}}, cex={{cex}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             factor=getval(input$bagplot_aplpack_pairs_factor, 3),
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1)
    )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("bagplot_aplpack_pairs_factor", "Loop factor", 1, 5, 3, 0.1),
         UIpointsymbol(),
         UIpointsize()
    )
  }
)

