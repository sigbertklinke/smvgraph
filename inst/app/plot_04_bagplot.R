module[["bagplot_aplpack"]] <- list(
  label = "Bagplot (aplpack)",
  help  = "aplpack::bagplot",
  packages = "aplpack",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   library('aplpack')
             0:   x <- numeric_data(data, select={{x}})
             0:   x <- x[is.finite(rowSums(x)),]
             0:   bagplot(x, factor={{factor}}, show.outlier={{out}}, show.whiskers={{whisker}}, show.looppoints={{lpoint}}, show.bagpoints={{bpoint}}, show.loop={{lhull}}, show.baghull={{bhull}}, pch={{pch}}, cex={{cex}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             out=getval('Outliers' %in% input$bagplot_aplpack_type, TRUE),
             whisker=getval('Whiskers' %in% input$bagplot_aplpack_type, TRUE),
             lpoint=getval('Loop points' %in% input$bagplot_aplpack_type, TRUE),
             bpoint=getval('Bag points' %in% input$bagplot_aplpack_type, TRUE),
             lhull=getval('Loop hull' %in% input$bagplot_aplpack_type, TRUE),
             bhull=getval('Bag hull' %in% input$bagplot_aplpack_type, TRUE),
             factor=getval(input$bagplot_aplpack_factor, 3),
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1)
    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxGroupInput("bagplot_aplpack_type", "Show",
                            choices=c("Outliers", "Whiskers", "Loop points", "Bag points", "Loop hull", "Bag hull"),
                            selected=c("Outliers", "Whiskers", "Loop points", "Bag points", "Loop hull", "Bag hull")),
         sliderInput("bagplot_aplpack_factor", "Loop factor", 1, 5, 3, 0.1),
         UIpointsymbol(),
         UIpointsize()
    )
  }
)
