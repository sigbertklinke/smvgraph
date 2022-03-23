module[["scatterplot3d"]] <- list(
  label = "Scatter plot 3D (scatterplot3d)",
  help  = "graphics::plot.default",
  packages = "scatterplot3d",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==3) && (prod(group$unique)<13) 
  },
  code = function(analysis, group, data, input) {
    template("
    0:  library('scatterplot3d')
    0:  x    <- numeric_data(data, select={{x}}) 
    !1: col  <- 'black'
    1:  col  <- color_data(data, select={{g}})
    0:  s3d  <- scatterplot3d(x, pch={{pch}}, cex.symbols={{cex}}, scale.y={{scale}}, angle={{angle}}, color=col)
    1:  ucol <- col[!duplicated(col)]
    1:  legend({{pos}}, fill = ucol, legend = names(ucol), title=attr(col, 'title'))
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             scale=getval(input$scatterplot3d_scale,1),
             angle=getval(input$scatterplot3d_angle,40),
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             pos=txt(getval(input$smvgraph_legend, "topleft")),
             nrow(group)>0 #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("scatterplot3d_scale", "Scale Y axis", 0, 3, 1, 0.05),
         sliderInput("scatterplot3d_angle", "Angle X-Y", 0, 90, 40, 1),
         UIpointsymbol(),
         UIpointsize(),
         if (nrow(group)>0) UIlegend() else NULL
    )
  }
)