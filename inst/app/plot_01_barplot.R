module[["barplot"]] <- list(
  label = "Bar chart",
  help  = "graphics::barplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (nrow(group)==0) && (analysis$unique<43)
  },
  code = function(analysis, group, data, input, col) {
    template("
             !1: tab <- table({{x}})
             1:  tab <- proportions(table({{x}}))
             0:  barplot(tab, horiz={{horiz}})
             ", 
             x=sprintf("data$%s", row.names(analysis)),
             horiz=getval(input$barplot_horiz, FALSE),
             getval(input$barplot_prop, FALSE) #1

    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("barplot_prop", "Relative"),
         checkboxInput("barplot_horiz", "Horizontal")
    )
  }
)

module[["barplot2"]] <- list(
  label = "Bar chart (stacked/grouped)",
  help   = "graphics::barplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2) && (nrow(group)==0) && isTRUE(all(analysis$unique<13))
  },
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    template("
             0: tab <- table({{x}}, {{y}})
             1: tab <- proportions(tab, 1)
             2: tab <- proportions(tab, 2)
             3: tab <- proportions(tab)
             0: barplot(tab, horiz={{horiz}}, beside={{beside}}, xlab={{yv}})
             0: legend({{pos}}, legend=rownames(tab), fill=grey.colors(nrow(tab)), title={{xv}}, cex={{size}})
             ", 
             x=sprintf("data$%s", rn[1]),
             xv=sprintf("'%s'", rn[1]),
             y=sprintf("data$%s", rn[2]),
             yv=sprintf("'%s'", rn[2]),
             horiz=getval(input$barplot2_horiz, FALSE),
             beside=getval(input$barplot2_beside, FALSE),
             pos=sprintf("'%s'", getval(input$smvgraph_legend, "topleft")),
             size=getval(input$smvgraph_lex, 1),
             getval(input$barplot2_freq==1, FALSE),  #1
             getval(input$barplot2_freq==2, FALSE),  #2
             getval(input$barplot2_freq==3, FALSE)   #3
    )
  },
  ui = function(analysis, group, data, input) {
    list(selectInput("barplot2_freq", "Frequencies", 
                     choices=list("absolute"=0, 
                                  "relative"=3, 
                                  "conditional by row"=1, 
                                  "conditional by column"=2)),
         checkboxInput("barplot2_beside", "Grouped"),
         checkboxInput("barplot2_horiz", "Horizontal"),
         UIlegend(),
         UIlegendsize() # defines smvgraph_legend
    )
  }
)