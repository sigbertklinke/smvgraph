module[["boxplot"]] <- list(
  label = "Box plot",
  help  = "graphics::boxplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1)
  },
  code = function(analysis, group, data, input) {
    template("
             0:  x   <- numeric_data(data, select={{x}}, out='vector')
             1:  grp <- names(color_data(data, select={{g}}))
             !1: boxplot(x, varwidth={{width}}, horizontal={{horiz}}, notch={{notch}})
             1:  boxplot(x~grp, varwidth={{width}}, horizontal={{horiz}}, notch={{notch}}, {{lab}}=attr(grp, 'title'))
             2:  rug(x, side={{side}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             width=getval(input$boxplot_varwidth, FALSE),
             horiz=getval(input$boxplot_horizontal, FALSE),
             notch=getval(input$boxplot_notch, FALSE),
             side=2-getval(input$boxplot_horizontal, FALSE),
             lab=if(getval(input$boxplot_horizontal, FALSE)) "ylab" else "xlab",
             nrow(group)>0,                                     #1
             getval(input$boxplot_rug, FALSE)                   #2
            )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("boxplot_rug", "Show observations"),
         checkboxInput("boxplot_varwidth", "Width adjusted"),
         checkboxInput("boxplot_notch", "Notch"),
         checkboxInput("boxplot_horizontal", "Horizontal")
)
  }
)
