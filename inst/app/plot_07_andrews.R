module[["andrews_smvgraph"]] <- list(
  label = "Andrews curves (smvgraph)",
  help  = "smvgraph::andrews",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) 
  },
  code = function(analysis, group, data, input) {
    template("
0:      x   <- numeric_data(data, select={{x}})
1:      col <- color_hclust(x, normalize={{norm}}, ncol={{col}})[keep]
2:      col <- color_data(data, select={{g}})
1|2:    andrews(x, type={{type}}, normalize={{norm}}, col=col) 
!(1|2): andrews(x, type={{type}}, normalize={{norm}})  
1|2:    ucol <- col[!duplicated(col)]
1|2:    legend({{pos}}, legend=names(ucol), col=ucol, lwd=2, title=attr(col, 'title'))
             ",
             x      = sprintf("c(%s)", paste0("'", row.names(analysis), "'", collapse=', ')),
             g      = sprintf("c(%s)", paste0("'", row.names(group), "'", collapse=', ')),
             type   = getval(input$andrews_smvgraph_type, 1),        
             col    = getval(input$andrews_smvgraph_col, 1),       
             norm   = getval(input$smvgraph_normalize, 1), 
             pos    = sprintf('"%s"', getval(input$smvgraph_legend, "topleft")),
             getval(input$andrews_smvgraph_col,1)>1,               #1  
             nrow(group)>0                                         #2
             )
  },
  ui = function(analysis, group, data, input) {
    list(selectInput("andrews_smvgraph_type", "Type", as.character(1:4)),
         if (nrow(group)==0) sliderInput("andrews_smvgraph_col", "Color by hclust", 1, 10, 1, 1) else NULL,
         UIdatanormalization(),
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