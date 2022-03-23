module[["mosaicplot"]] <- list(
  label = "Mosaic plot",
  help  = "graphics::mosaicplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && (nrow(group)==0) && isTRUE(all(analysis$unique<13))
  },
  code = function(analysis, group, data, input) {
    template("
             0:  x <- character_data(data, select={{x}})
             1:  mosaicplot(table(x), color=TRUE)
             !1: mosaicplot(table(x), shade=TRUE, type={{type}})  
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             type=txt(getval(input$mosaicplot_type, "none")),
             getval(input$mosaicplot_type, "none")=="none"     #1
            )
  },
  ui = function(analysis, group, data, input) {
    list(selectInput("mosaicplot_type", "Type of residual",
                     choices=list("None"="none", "Pearson"="pearson", "Deviance"="deviance", "Freedman-Tukey"="FT"))
         )
  }
)
