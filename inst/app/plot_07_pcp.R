module[["parcoord"]] <- list(
  label = "Parallel coordinate plot",
  help  = "MASS::parcoord",
  packages = "MASS",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) && isTRUE(all(analysis$unique>1))
  },
  code = function(analysis, group, data, input) {
    template("
0:      library('MASS')    
0:      x   <- numeric_data(data, select={{x}})
1:      col <- color_hclust(x, normalize={{norm}}, ncol={{col}})
2:      col <- color_data(data, select={{g}})
1|2:    parcoord(x, col=col) 
!(1|2): parcoord(x)  
1|2:    ucol <- col[!duplicated(col)]
1|2:    legend({{pos}}, legend=names(ucol), col=ucol, lwd=2, title=attr(col, 'title'))
             ",
             x      = sprintf("c(%s)", paste0("'", row.names(analysis), "'", collapse=', ')),
             g      = sprintf("c(%s)", paste0("'", row.names(group), "'", collapse=', ')),
             col    = getval(input$parcoord_col, 1),       
             norm   = getval(input$parcoord_normalize, 1), 
             pos    = sprintf('"%s"', getval(input$smvgraph_legend, "topleft")),
             getval(input$parcoord_col,1)>1,               #1  
             nrow(group)>0                                         #2
             )
  },
  ui = function(analysis, group, data, input) {
    list(if (nrow(group)==0) sliderInput("parcoord_col", "Color by hclust", 1, 10, 1, 1) else NULL,
         selectInput("parcoord_normalize", "Scale for hclust", choices=list(None=0, MinMax=1, Standardize=2), 1),
         if (nrow(group)) selectInput("smvgraph_legend", "Legend position",
                     choices=list("Top left"     = "topleft", 
                                  "Top right"    = "topright", 
                                  "Bottom left"  = "bottomleft",
                                  "Bottom right" = "bottomright",
                                  "Bottom"       = "bottom",
                                  "Left"         = "left",
                                  "Top"          = "top",
                                  "Right"        = "right",
                                  "Center"       = "center")) else NULL
    )
  }
)