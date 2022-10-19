module[["edcf"]] <- list(
  label = "Cumulative Distribution",
  help  = "stats::ecdf",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (prod(group$unique)<43)
  },
  code = function(analysis, group, data, input) {
    template("
             0: x    <- numeric_data(data, select={{x}}, out='vector')
             0: keep <- is.finite(x)
             0: x    <- x[keep]
             !1: plot(ecdf(x), verticals={{vert}})
             1: plot.ecdf(ecdf(x), cex=0, lwd=0) # no points, no lines
             1: col   <- color_data(data, select={{g}})[keep]
             1: ecdfs <- tapply(x, col, ecdf)
             1: ucol  <- col[!duplicated(col)]
             1: lapply(seq_along(ucol), ucol=ucol, function(i, ucol) { plot(ecdfs[[i]], col=ucol[i], verticals={{vert}}, add=TRUE) }) 
             1: legend({{pos}}, legend=names(ucol), lwd=2, col=ucol, title=attr(col, 'title'))
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             pos=txt(getval(input$smvgraph_legend, "topleft")),
             vert=getval(input$ecdf_verticals, FALSE),
             nrow(group)>0   #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("ecdf_verticals", "Vertical lines", FALSE),       
         if (nrow(group)>0) UIlegend() else NULL
    )
  }
)