module[["pyramid_smvgraph"]] <- list(
  label = "Pyramid plot (smvgraph)",
  help  = "smvgraph::pyramid",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (nrow(group)==1) && (group$unique==2) 
  },
  code = function(analysis, group, data, input) {
    template("
             0: x   <- character_data(data, select={{x}})
             0: tab <- table(x)
             1: tab <- proportions(tab, 1)
             2: tab <- proportions(tab, 2)
             3: tab <- proportions(tab)
             0: pyramid(tab, gap=c({{gapy}}, {{gapx}}))
             ", 
             x=as_param(txt(c(row.names(analysis),row.names(group))), fun="c"),
             gapx=getval(input$pyramid_smvgraph_gapx, 0),
             gapy=getval(input$pyramid_smvgraph_gapy, 0),
             getval(input$pyramid_smvgraph_freq==1, FALSE),  #1
             getval(input$pyramid_smvgraph_freq==2, FALSE),  #2
             getval(input$pyramid_smvgraph_freq==3, FALSE)   #3
             )
  },
  ui = function(analysis, group, data, input) {
   list(sliderInput("pyramid_smvgraph_gapx", "Gap in X", 0, 1, 0, 0.05),
        sliderInput("pyramid_smvgraph_gapy", "Gap in Y", 0, 1, 0, 0.05),
        selectInput("pyramid_smvgraph_freq", "Frequencies", 
                    choices=list("absolute"=0, 
                                 "relative"=3, 
                                 "conditional by row"=1, 
                                 "conditional by column"=2))
        ) 
  }
)